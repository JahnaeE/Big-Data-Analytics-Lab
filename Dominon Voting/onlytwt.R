#only twts
setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/raw NER/Larger Model/Onlytwt")

library(readxl)
library(dplyr)
library(sqldf)
library(ggplot2)
library(tm)
library(lsa)
library(gridExtra)


Combined <- read.csv("combinedtwt.csv")
#ner with dup
NER_dates <- read.csv("NER_combined.csv")


#manually change domnion gpe tag to org
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

NER_dates$Date <- as.Date(NER_dates$Date, format = "%m/%d/%Y")

#Get counts of unique locations for entire dataset  
locationCounts <- sqldf("SELECT entity, COUNT(entity) FROM NER_dates WHERE tag ='GPE' 
                        GROUP BY entity ORDER BY COUNT(entity) DESC")

#subset location
subsetloc <- NER_dates[NER_dates$tag == 'GPE', ]

CountbyDate <- sqldf("SELECT entity, date, COUNT(entity) AS count
                 FROM subsetloc
                 GROUP BY entity, Date
                 ORDER BY count DESC, entity")


top_10_entities <- CountbyDate %>%
  group_by(entity) %>%
  summarize(count = sum(count)) %>%
  top_n(10, wt = count) %>%
  arrange(desc(count))


#bar chart with only top 10
# Filter the data to select only the first 10 unique entities
top_entities <- unique(CountbyDate$entity)[1:10]
result_filtered <- CountbyDate[CountbyDate$entity %in% top_entities, ]

#UPDATE COLORS FOR NEW COUNTS
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
  "EEUU" = "gray"
)

# Update the ggplot code with scale_fill_manual
incdup <- ggplot(result_filtered, aes(x = Date, y = count, fill = as.factor(entity))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Date",
    y = "Count",
    title = "Location counts with duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
incdup

##########################################################################################
#create chart without duplicates
#ner without dup
NER_dates_nodup <- read.csv("NER_combined_no_duplicates_80.csv")

#manually change domnion gpe tag to org
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(tag = ifelse(grepl("Dominion", entity) & tag == "GPE", "ORG", tag))
#clean locations
NER_dates_nodup$entity <- gsub("(?i)PENNSYLVANIA|State Pennsylvaniya", "Pennsylvania", NER_dates_nodup$entity, ignore.case = TRUE)
NER_dates_nodup$entity <- gsub("china|communist china|	China China", "China",NER_dates_nodup$entity, ignore.case = TRUE)
NER_dates_nodup$entity <- gsub("Georgia","Georgia",NER_dates_nodup$entity, ignore.case = TRUE)
NER_dates_nodup$entity <- gsub("Texas|state texas", "Texas",NER_dates_nodup$entity, ignore.case = TRUE)
NER_dates_nodup$entity <- gsub("Antrim|Michigan Antrim|Antrim County","Antrim County",NER_dates_nodup$entity, ignore.case = TRUE)
NER_dates_nodup$entity <- gsub("Michigan","Michigan",NER_dates_nodup$entity, ignore.case = TRUE)
#combining alternative english ways of saying united states 
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("US", "U.S", "USA", "U.S.", "America") ~ "United States",
    TRUE ~ entity
  ))
#combining spanish way to saying united states
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("EE.UU") ~ "EEUU",
    TRUE ~ entity
  ))

NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("GA") ~ "Georgia",
    TRUE ~ entity
  ))
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("PA") ~ "Pennsylvania",
    TRUE ~ entity
  ))
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("MI") ~ "Michigan",
    TRUE ~ entity
  ))
NER_dates_nodup <- NER_dates_nodup %>%
  mutate(entity = case_when(
    entity %in% c("AZ") ~ "Arizona",
    TRUE ~ entity
  ))
NER_dates_nodup$Date <- as.Date(NER_dates_nodup$Date, format = "%m/%d/%Y")

subsetloc_nodup <- NER_dates_nodup[NER_dates_nodup$tag == 'GPE', ]

Countbydate_nodup <- sqldf("SELECT entity, date, COUNT(entity) AS count
                 FROM subsetloc_nodup
                 GROUP BY entity, Date
                 ORDER BY COUNT(entity) DESC, entity")



#bar graph
top_10_entities_nodup <- Countbydate_nodup %>%
  group_by(entity) %>%
  summarize(count = sum(count)) %>%
  top_n(10, wt = count) %>%
  arrange(desc(count))


#bar chart with only top 10
# Filter the data to select only the first 10 unique entities
top_entities_nodup <- unique(Countbydate_nodup$entity)[1:10]
result_filtered_nodup <- Countbydate_nodup[Countbydate_nodup$entity %in% top_entities_nodup, ]

entity_colors <- c(
  "Pennsylvania" = "purple",
  "Michigan" = "green",
  "Georgia" = "orange",
  "China" = "blue",
  "Denver" = "black",
  "Maricopa County" = "chocolate",
  "United States" = "cyan",
  "Canada" = "plum",
  "Texas" = "pink",
  "Arizona" = "darkgreen"
)



# Update the ggplot code with scale_fill_manual
nodupl <- ggplot(result_filtered_nodup, aes(x = Date, y = count, fill = as.factor(entity))) +
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
nodupl

#set charts next to each other
grid.arrange(incdup, nodupl, ncol = 2)


#################################################################
#change charts to percentage
#with dup
result_filtered <- result_filtered %>%
  group_by(entity) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup()

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
  "EEUU" = "gray")

incdup_percentage <- ggplot(result_filtered, aes(x = Date, y = percentage, fill = as.factor(entity))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Location counts with duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
incdup_percentage

#alternative char y-axis only to 100%, bars stacked next to each other 
incdup_percentage_dodge <- ggplot(result_filtered, aes(x = Date, y = percentage, fill = as.factor(entity))) +
  geom_bar(position = "dodge",  # Use "dodge" instead of "stack"
           stat = "identity") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Location counts with duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
incdup_percentage_dodge


#change charts to percentage
#without dup
result_filtered_nodup <- result_filtered_nodup %>%
  group_by(entity) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup()

entity_colors_nodup <- c(
  "Pennsylvania" = "purple",
  "Michigan" = "green",
  "Georgia" = "orange",
  "China" = "blue",
  "Denver" = "black",
  "Maricopa County" = "chocolate",
  "United States" = "cyan",
  "Canada" = "plum",
  "Texas" = "pink",
  "Arizona" = "darkgreen"
)

# Update the ggplot code with scale_fill_manual
nodupl_percentage <- ggplot(result_filtered_nodup, aes(x = Date, y = percentage, fill = as.factor(entity))) +
  geom_bar(stat = "identity") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Location counts without duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors_nodup, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
nodupl_percentage

nodupl_percentage_dodge <- ggplot(result_filtered_nodup, aes(x = Date, y = percentage, fill = as.factor(entity))) +
  geom_bar(position = "dodge",  # Use "dodge" instead of "stack"
           stat = "identity") +
  labs(
    x = "Date",
    y = "Percentage",
    title = "Location counts without duplicates",
    fill = "Entity"
  ) +
  scale_fill_manual(values = entity_colors_nodup, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
nodupl_percentage_dodge



grid.arrange(incdup_percentage, nodupl_percentage, ncol = 2)

#alt way to graph
combined_data <- rbind(
  transform(result_filtered, source = "With Duplicates"),
  transform(result_filtered_nodup, source = "Without Duplicates")
)

entity_colors_cmb <- c(
  "Pennsylvania" = "purple",
  "Michigan" = "green",
  "Georgia" = "orange",
  "China" = "blue",
  "Denver" = "black",
  "Maricopa County" = "chocolate",
  "United States" = "cyan",
  "Canada" = "plum",
  "Texas" = "pink",
  "Arizona" = "darkgreen",
  "DC" = "red",
  "Serbia" = "yellow",
  "Antrim County" = "brown",
  "EEUU" = "gray"
)

ggplot(combined_data) +
  geom_bar(aes(x = Date, y = percentage, fill = as.factor(entity)),
           position = "stack",
           stat = "identity") +
  labs(
    title = "Location counts percentage"
  ) +
  scale_fill_manual(values = entity_colors_cmb, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ source)


#alternative bar chart with locations for each date next to each other
ggplot(combined_data) +
  geom_bar(aes(x = Date, y = percentage, fill = as.factor(entity)),
           position = "dodge",  # Use "dodge" instead of "stack"
           stat = "identity") +
  labs(
    title = "Location counts percentage"
  ) +
  scale_fill_manual(values = entity_colors_cmb, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limits to 0-100%
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ source)

combined_data <- rbind(
  transform(result_filtered, source = "With Duplicates"),
  transform(result_filtered_nodup, source = "Without Duplicates")
)

entity_colors_cmb <- c(
  "Pennsylvania" = "purple",
  "Michigan" = "green",
  "Georgia" = "orange",
  "China" = "blue",
  "Denver" = "black",
  "Maricopa County" = "chocolate",
  "United States" = "cyan",
  "Canada" = "plum",
  "Texas" = "pink",
  "Arizona" = "darkgreen",
  "DC" = "red",
  "Serbia" = "yellow",
  "Antrim County" = "brown",
  "EEUU" = "gray"
)

ggplot(combined_data) +
  geom_bar(aes(x = Date, y = percentage, fill = as.factor(entity)),
           position = "fill",
           stat = "identity") +
  labs(
    title = "Location counts percentage"
  ) +
  scale_fill_manual(values = entity_colors_cmb, name = "Entity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day", expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(~ source)
