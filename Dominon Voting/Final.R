#setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon")
setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/raw NER/Larger Model/Onlytwt")
library(readxl)
library(dplyr)
library(sqldf)
library(ggplot2)
library(tm)
library(lsa)

Combined <- read.csv("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/Combined_twt.csv")

NER_dates <- read.csv("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/raw NER/Larger Model/fuzzy/NER_complete_80.csv")
#Combined <- read.csv("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/Combined2.csv")
#change instances of dominion being tagged as GPE to ORG manually
NER_dates <- NER_dates %>%
  mutate(tag = ifelse(grepl("Dominion", entity) & tag == "GPE", "ORG", tag))
#clean locations
NER_dates$entity <- gsub("(?i)PENNSYLVANIA|State Pennsylvaniya", "Pennsylvania", NER_dates$entity, ignore.case = TRUE)
NER_dates$entity <- gsub("china|communist china|	China China", "China",NER_dates$entity, ignore.case = TRUE)
NER_dates$entity <- gsub("Georgia","Georgia",NER_dates$entity, ignore.case = TRUE)
NER_dates$entity <- gsub("Texas|state texas", "Texas",NER_dates$entity, ignore.case = TRUE)
NER_dates$entity <- gsub("Antrim|Michigan Antrim|Antrim County","Antrim County",NER_dates$entity, ignore.case = TRUE)
NER_dates$entity <- gsub("Michigan","Michigan",NER_dates$entity, ignore.case = TRUE)
#combining alternative english ways of saying united states 
NER_dates <- NER_dates %>%
  mutate(entity = case_when(
    entity %in% c("US", "U.S", "USA", "U.S.", "America") ~ "United States",
    TRUE ~ entity
  ))
#combining spanish way to saying united states
NER_dates <- NER_dates %>%
  mutate(entity = case_when(
    entity %in% c("EE.UU") ~ "EEUU",
    TRUE ~ entity
  ))

NER_dates <- NER_dates %>%
  mutate(entity = case_when(
    entity %in% c("GA") ~ "Georgia",
    TRUE ~ entity
  ))
NER_dates <- NER_dates %>%
  mutate(entity = case_when(
    entity %in% c("PA") ~ "Pennsylvania",
    TRUE ~ entity
  ))
NER_dates <- NER_dates %>%
  mutate(entity = case_when(
    entity %in% c("MI") ~ "Michigan",
    TRUE ~ entity
  ))

#Get counts of unique locations for entire dataset  
locationCounts <- sqldf("SELECT entity, COUNT(entity) FROM NER_dates WHERE tag ='GPE' 
                        GROUP BY entity ORDER BY COUNT(entity) DESC")

#subset location
subsetloc <- NER_dates[NER_dates$tag == 'GPE', ]


# Count occurrences of each entity on each date using SQL
CountbyDate <- sqldf("SELECT entity, date, COUNT(entity) AS count
                 FROM subsetloc
                 GROUP BY entity, date
                 ORDER BY count DESC, entity")

#bar graph
top_10_entities <- CountbyDate %>%
  group_by(entity) %>%
  summarize(count = sum(count)) %>%
  top_n(10, wt = count) %>%
  arrange(desc(count))

NER_dates <- as.Date(NER_dates$date)
NER_dates <- as.data.frame(NER_dates)

#bar chart with only top 10
# Filter the data to select only the first 10 unique entities
top_entities <- unique(result$entity)[1:10]
result_filtered <- result[result$entity %in% top_entities, ]

entity_colors <- c(
  "Pennsylvania" = "purple",
  "China" = "blue",
  "Michigan" = "green",
  "Georgia" = "orange",
  "DC" = "red",
  "Texas" = "pink",
  "United States" = "cyan",
  "Serbia" = "yellow",
  "Antrim County" = "brown",
  "Arizona" = "gray"
)

# Update the ggplot code with scale_fill_manual
incdup <- ggplot(result_filtered, aes(x = date, y = count, fill = as.factor(entity))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Date",
    y = "Count",
    title = "Location counts without duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
incdup
###########################
#FuzzyWuzzy
#load in fuzzy results 



altnodup <- sqldf("SELECT entity, date, COUNT(entity) AS count
                 FROM altgraph
                 GROUP BY entity, date
                 ORDER BY COUNT(entity) DESC, entity")

#bar graph
top_10_entities_alt <- altnodup %>%
  group_by(entity) %>%
  summarize(count = sum(count)) %>%
  top_n(10, wt = count) %>%
  arrange(desc(count))


#bar chart with only top 10
# Filter the data to select only the first 10 unique entities
top_entities_alt <- unique(altnodup$entity)[1:10]
result_filtered_alt <- altnodup[altnodup$entity %in% top_entities_alt, ]

entity_colors <- c(
  "Pennsylvania" = "purple",
  "Michigan" = "green",
  "Georgia" = "orange",
  "China" = "blue",
  "DC" = "red",
  "Antrim County" = "brown",
  "United States" = "cyan",
  "Gwinnett County" = "plum",
  "Morgan County" = "maroon1",
  "Oakland County" = "darkgreen"
)

# Update the ggplot code with scale_fill_manual
#add position="fill"
nodupl <- ggplot(result_filtered_alt, aes(x = date, y = count, fill = as.factor(entity))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    x = "Date",
    y = "Count",
    title = "Location counts without duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
nodupl

#set charts next to each other
grid.arrange(incdup, nodupl, ncol = 2)

#work on percentage chart 
