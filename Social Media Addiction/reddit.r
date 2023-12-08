setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Social media/Only Reddit")

library(readr)
library(dplyr)
library(readxl)
library(stringr)
library(emoji)
library(tidyverse)
library(tidytext)
library(ldatuning)
library(topicmodels)
library(qdapRegex)
library(text2vec)
library(tm)
library(mallet)
library(servr)
library(LDAvis)
library(textclean)
library(htmlwidgets)
library(textstem)
#library(ldavis)

reddit <- read_excel("Reddit 2013-2023.xlsx")
#cleaning
#subset columns
df <- subset(reddit, select = c(`Query Id`, Date, Title, Snippet, Url, Author, `Full Text`, `Page Type`))

#delete na values
df <- na.omit(df)

#turn links into URL
pattern <- "https?://\\S+"
df$`Full Text` <- gsub(pattern, "URL", df$`Full Text`, perl = TRUE)
df$Title <- gsub(pattern, "URL", df$Title, perl = TRUE)

#delete duplicates
df_nodup <- df[!duplicated(df$`Full Text`), ]


#Create random sample 
df_nodup_sample <- df_nodup %>% sample_n(1000)

#replace emojis
emoji <- emojis
emojisimple <- emoji[,1:2]

delete_emoji <- function(text) {
  for (i in 1:nrow(emojisimple)) {
    emoji <- emojisimple$emoji[i]
    text <- gsub(emoji, "", text, fixed = TRUE)
  }
  return(text)
}

# Delete every emoji from the 'Full Text' column in sample data
df_nodup$`Full Text` <- sapply(df_nodup$`Full Text`, delete_emoji)

#save sample file
write.csv(df_nodup, file = "Reddit_clean.csv", row.names = FALSE)

#expanded contractions using python
#ran in batches to find that line 115384 was causing error
#combing batches ran
# Read the first dataset
dataset1 <- read.csv("outputnw1-50000.csv")

# Read the second dataset
dataset2 <- read.csv("output.csv")

# Subset the first 50k rows of the first dataset
subset1 <- dataset1[1:50000, ]

# Subset the 50,001 - 100k rows of the second dataset
subset2 <- dataset2[50001:100001, ]

dataset3 <- read.csv("output (1) 100-115300.csv")
# Subset the 100,001 - 115,301 rows
subset3 <- dataset3[100002:115301, ]

# Read the fourth dataset
dataset4 <- read.csv("output (1) 115300 - 115382.csv")
# Subset the 115,302 - 115,383 rows
subset4 <- dataset4[115302:115383, ]

# Read the fifth dataset
dataset5 <- read.csv("output (1) 115820 - end.csv")
# Subset the 115,385 - 115,472 rows
subset5 <- dataset5[115385:115471, ]

# Combine the subsets into one dataset
FinalFixedContractions <- rbind(subset1, subset2, subset3, subset4, subset5)

write.csv(FinalFixedContractions, "FinalFixedContractions.csv", row.names = FALSE)



#Analysis
clean <- read.csv("FinalFixedContractions.csv")

#estimate number of topics
#convert text to lowercase
clean <- clean %>%
  mutate(Full.Text = tolower(Full.Text)) %>%
  mutate(Title = tolower(Title))


malletstopwords <- read_lines("mallet.en.txt") 
customSW <- c(malletstopwords, as.character(0:9))


##
#WORKING
tidy_sample <- clean %>%
  mutate(line = row_number()) %>% #which line every work is from
  unnest_tokens(word, Full.Text) %>% #one word per row 
  anti_join(stop_words) %>%
  filter(!word %in% customSW)

word_counts <- tidy_sample %>%
  count(Page.Type, word, sort = TRUE) %>%
  group_by(Page.Type) %>%
  top_n(10)

reddit_tf_idf <- tidy_sample %>%
  count(Page.Type, word, sort = TRUE) %>%
  bind_tf_idf(word, Page.Type, n) %>%
  arrange(-tf_idf) %>%
  group_by(Page.Type) %>%
  top_n(10) %>%
  ungroup

#create graphic
reddit_tf_idf %>%
  group_by(Page.Type) %>%
  top_n(10, wt = n) %>%
  ggplot(aes(x = (n/sum(n)), y = word, fill = Page.Type)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~Page.Type, ncol = 2, scales = "free_y") +
  labs(x = "TF-IDF Value", y = "Top Words",
       title = "Top 10 tf-idf words in 'reddit'")


#topics
sample_dfm <- tidy_sample %>%
  count(Page.Type, word, sort = TRUE) %>%
  cast_dfm(Page.Type, word, n)

result <- FindTopicsNumber(
  sample_dfm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)

#edit findtopicnumber_plot funtion
findtopics <- function (values) 
{
  if ("LDA_model" %in% names(values)) {
    values <- values[!names(values) %in% c("LDA_model")]
  }
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(values["topics"], base::apply(columns, 
                                                           2, function(column) {
                                                             scales::rescale(column, to = c(0, 1), from = range(column))
                                                           }))
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  values$group <- values$variable %in% c("Griffiths2004", 
                                         "Deveaud2014")
  values$group <- base::factor(values$group, levels = c(FALSE, 
                                                        TRUE), labels = c("minimize", "maximize"))
  p <- ggplot(values, aes_string(x = "topics", y = "value", 
                                 group = "variable"))
  p <- p + geom_line()
  p <- p + geom_point(aes_string(shape = "variable"), size = 3)
  p <- p + guides(size = FALSE, shape = guide_legend(title = "metrics:"))
  p <- p + scale_x_continuous(breaks = values$topics)
  p <- p + labs(x = "number of topics", y = NULL)
  p <- p + facet_grid(group ~ .)
  p <- p + theme_bw() %+replace% theme(panel.grid.major.y = element_blank(), 
                                       panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour = "grey70"), 
                                       panel.grid.minor.x = element_blank(), legend.key = element_blank(), 
                                       strip.text.y = element_text(angle = 90))
  p <- p + ggtitle("Estimated Topics")
  g <- ggplotGrob(p)
  g$layout[g$layout$name == "strip-right", c("l", "r")] <- 3
  grid::grid.newpage()
  grid::grid.draw(g)
}
findtopics(result)

#mallet topic modeling
#large character vector
texts=c()
for(sid in 1:nrow(sample)){
  texts=c(texts,paste(c(sample[['Title']][sid],sample[['expanded']][sid])," ",collapse=" "))
}
#transform to a dataframe, title is used as IDs for later purpose
jtext=data.frame(sid=sample$Title,text=texts)
#lower case
jtext$text=tolower(jtext$text)

#delete punctuation
jtext[['text']]=str_replace_all(jtext[['text']], "[[:punct:]]", " ")

#delete any alphabetic or numeric character and other letters produced online but are not useful for our analysis such as "amp" "https"..
jtext[['text']]=str_replace_all(jtext[['text']], "https://t.co/[A-Za-z\\d]+|https://[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp|@[A-Za-z\\d]+|&amp",'')

# Create a Corpus object from the 'jtext' data frame
corpus <- Corpus(VectorSource(jtext[['text']]))

stopwords_regex <- paste("\\b(", paste(customSW, collapse = "|"), ")\\b")



#drop all stopwords contained in the jtext data frame
jtext[['text']] = str_replace_all(jtext$text, stopwords_regex, '')



# Convert the corpus back to a data frame
jtext[['text']] <- sapply(corpus, as.character)

jtext[['text']] <- trimws(jtext[['text']])

#drop words length 1 or 2
jtext[['text']]=sapply(jtext[['text']], function(x) rm_nchar_words(x, "1,2"))

#apply stopwords again
jtext[['text']] = str_replace_all(jtext[['text']], stopwords_regex, '')

ntext=1:nrow(jtext) #jtext$sid



#mallet.import: This function takes an array of document IDs and text files (as character strings) and converts them into a Mallet instance list.
mall.instance <- mallet.import(
  as.character(ntext),
  jtext[['text']],
  "mallet.en.txt",
  FALSE,
  token.regexp="[\\p{L}]+")

#set number of topic
topic.model=MalletLDA(num.topics=8)
topic.model$loadDocuments(mall.instance)
vocab=topic.model$getVocabulary()
word.freqs=mallet.word.freqs(topic.model)
topic.model$setAlphaOptimization(40,80)
topic.model$train(400)

topic.words.m<-mallet.topic.words(topic.model,smoothed=TRUE,normalized=TRUE)

dim(topic.words.m)

vocabulary <- topic.model$getVocabulary() 
colnames(topic.words.m) <- vocabulary 

doc.topics.m <- mallet.doc.topics(topic.model, smoothed=T,
                                  normalized=T)

doc.topics.df <- as.data.frame(doc.topics.m)
doc.topics.df <- cbind(ntext, doc.topics.df)

doc.topic.means.df <- aggregate(doc.topics.df[,2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),mean)

phi <- mallet.topic.words(topic.model, smoothed = TRUE, normalized = TRUE)
phi.count <- t(mallet.topic.words(topic.model, smoothed = TRUE, normalized = FALSE))

topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
topic.counts <- rowSums(topic.words.m)

topic.proportions <- topic.counts/sum(topic.counts)

vocab <- topic.model$getVocabulary() 

doc.tokens <- data.frame(id=c(1:nrow(doc.topics.m)), tokens=0)

# Create an ldavis visualization (replace this with your ldavis code)
for(i in vocab){
  # Find word if word in text
  matched <- grepl(i, jtext$text)
  doc.tokens[matched,2] =doc.tokens[matched,2] +  1
}

#TAKES LONG TIME TO RUN
ldajson=createJSON(phi = phi, theta = doc.topics.m, 
                   doc.length = doc.tokens[["tokens"]], 
                   vocab = vocab, 
                   term.frequency = apply(phi.count, 1, sum))

#saveWidget(ldajson, "ldavis_visualization.html")

write(ldajson,'8topics_mallet_petitions_1119.json')
