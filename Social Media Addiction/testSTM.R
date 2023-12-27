#stm
#ser wd
setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Social media/Only Reddit")

#load in data
reddit <- read.csv("FinalFixedContractions.csv")

#libraries
library(tidyverse)
library(tidytext)
library(dplyr)
library(stm)
library(furrr)
library(ggthemes)
library(scales)
library(kableExtra)
#plan(multiprocess)

#subset columns
df <- subset(reddit, select = c(Query.Id, Date, Title, Snippet, Url, Author, Full.Text, Page.Type))

#change queryid to row count
df <- df %>% 
  mutate(Query.Id = row_number())

#begin with sample
#sample <- df %>% 
 # sample_n(10000, replace = FALSE)

#write.csv(sample, "sampleSTM.csv")

#adding same stop words used in mallet
malletstopwords <- read_lines("mallet.en.txt") 
customSW <- c(malletstopwords, as.character(0:9))

tidy_hacker_news <- df %>%
  unnest_tokens(word, Full.Text) %>% 
  anti_join(get_stopwords()) %>%
  filter(!word %in% customSW) %>%
  add_count(word) %>%
  filter(n > 100) %>%
  select(-n)

hacker_news_sparse <- 
  tidy_hacker_news %>%
  count(Query.Id, word, name = "word_count") %>%
  cast_sparse(Query.Id, word, word_count)


plan(multisession)
many_models <- tibble(K = c(4,6,7,8,9)) %>%
  mutate(topic_model = future_map(
    ## notice that it is `K = .` here:
    K, ~ stm(hacker_news_sparse, K = ., verbose = FALSE), 
    ## new way to pass seed arg:
    .options = furrr_options(seed = TRUE)
  ))


heldout <- make.heldout(hacker_news_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, hacker_news_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, hacker_news_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result

#i assume that the topics are not as clear due to the smaller sample size, tutorial emphasizes that this works best with larger sample sizes
k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 6 or 7")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(4,6,7)) %>% #why does 7 not show?
  unnest(c(exclusivity, semantic_coherence)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


#topic modeling
#we are going to use 7 topics
topic_model <- k_result %>% 
  filter(K == 7) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model

td_beta <- tidy(topic_model)

td_beta

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(hacker_news_sparse))

td_gamma

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

#create bar chart
gamma_terms %>%
  top_n(7, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, max(gamma_terms$gamma) + 0.01),
                     labels = percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family = "IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 7 topics by prevalence in Reddit",
       subtitle = "With the top words that contribute to each topic")


#create table of bar chart results 
gamma_terms %>%
  arrange(topic) %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms")) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12, 
                latex_options = c("striped", "scale_down"))
