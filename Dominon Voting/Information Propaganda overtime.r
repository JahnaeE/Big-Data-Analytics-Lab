setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/Final")
#attempt to recreate graphic
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
#we are going to assume that na = original post
tweets <- read.csv("Combined_twt.csv")

tweets <- subset(tweets, select = c(Impressions, Date, Full.Text, Url
                                    ,Thread.Entry.Type, Engagement.Type, Reach..new.))

tweets$Date <- as.POSIXct(tweets$Date, format = "%Y-%m-%d %H:%M:%S")

#manually update UTC time to EST
tweets$Date <- tweets$Date - 5*60*60
tweets$Date <- strptime(tweets$Date, format = "%Y-%m-%d %H:%M:%S")
tweets$Date <- format(tweets$Date, "%Y-%m-%d, %H:%M:%S")

tweets$time <- sub('.*,\\s', '', tweets$Date)


tweets <- tweets %>%
  mutate(Date = as.Date(Date))

tweets$Engagement.Type <- ifelse(is.na(tweets$Engagement.Type), "Original Tweet", tweets$Engagement.Type)

#subset to only be nov 7th
tweets7th <- subset(tweets, Date == as.Date("2020-11-07"))

# Count the occurrences of each engagement type for each hour
summary_data <- tweets7th %>%
  group_by(time, Engagement.Type) %>%
  summarise(count = n()) %>%
  spread(Engagement.Type, count, fill = 0)

impressions_data <- tweets7th %>%
  group_by(time) %>%
  summarise(Impressions = sum(Impressions))

Combined <- merge(x = summary_data, y = impressions_data, by = "time")

#melt data together to help with graphing
Combined_melted <- melt(summary_data, id.vars = "time")

accumulated_combined <- Combined_melted %>%
  group_by(variable) %>%
  mutate(Combined_melted = cumsum(value))


result <- merge(accumulated_combined, Combined[c("time", "Impressions")], by = "time", all.x = TRUE)

shape_mapping <- c("tweets" = 1, "quotes" = 2, "replies" = 3, "retweets" = 4)

result$time <- as.POSIXct(result$time, format = "%H:%M:%S")

result_sum <- result %>%
  group_by(time) %>%
  summarise(Combined_sum = sum(Combined_melted), Impressions = first(Impressions))

merged_result <- merge(result, result_sum[, c("time", "Combined_sum")], by = "time", all.x = TRUE)

ggplot(merged_result, aes(x = time, y = Combined_sum, color = variable, shape = variable, size = Impressions)) +
  geom_point(alpha = 0.5) +
  labs(title = "Information Propagation Over Time on November 7th",
       x = "Time",
       y = "Accumulation of tweets, quotes, replies, and retweets") +
  scale_shape_manual(values = c(15, 16, 17, 18)) + 
  theme_minimal() +
  scale_x_datetime(labels = scales::time_format("%H:%M"),
                   breaks = scales::breaks_width("4 hours"))

#For the 12th 
tweets <- read.csv("Combined_twt.csv")

tweets <- subset(tweets, select = c(Impressions, Date, Full.Text, Url
                                    ,Thread.Entry.Type, Engagement.Type, Reach..new.))

tweets$Date <- as.POSIXct(tweets$Date, format = "%Y-%m-%d %H:%M:%S")

tweets$Date <- tweets$Date - 5*60*60
tweets$Date <- strptime(tweets$Date, format = "%Y-%m-%d %H:%M:%S")
tweets$Date <- format(tweets$Date, "%Y-%m-%d, %H:%M:%S")

tweets$time <- sub('.*,\\s', '', tweets$Date)


tweets <- tweets %>%
  mutate(Date = as.Date(Date))

tweets$Engagement.Type <- ifelse(is.na(tweets$Engagement.Type), "Original Tweet", tweets$Engagement.Type)

#subset to only be nov 12th
tweets12th <- subset(tweets, Date == as.Date("2020-11-12"))



# Count the occurrences of each engagement type for each hour
summary_data12 <- tweets12th %>%
  group_by(time, Engagement.Type) %>%
  summarise(count = n()) %>%
  spread(Engagement.Type, count, fill = 0)

impressions_data12 <- tweets12th %>%
  group_by(time) %>%
  summarise(Impressions = sum(Impressions))

Combined12 <- merge(x = summary_data12, y = impressions_data12, by = "time")

#melt data together to help with graphing
Combined_melted12 <- melt(summary_data12, id.vars = "time")

accumulated_combined12 <- Combined_melted12 %>%
  group_by(variable) %>%
  mutate(Combined_melted12 = cumsum(value))


result12 <- merge(accumulated_combined12, Combined12[c("time", "Impressions")], by = "time", all.x = TRUE)

shape_mapping12 <- c("tweets" = 1, "quotes" = 2, "replies" = 3, "retweets" = 4)

result12$time <- as.POSIXct(result12$time, format = "%H:%M:%S")

result_sum12 <- result12 %>%
  group_by(time) %>%
  summarise(Combined_sum12 = sum(Combined_melted12), Impressions = first(Impressions))

merged_result12 <- merge(result12, result_sum12[, c("time", "Combined_sum12")], by = "time", all.x = TRUE)

ggplot(merged_result12, aes(x = time, y = Combined_sum12, color = variable, shape = variable, size = Impressions)) +
  geom_point(alpha = 0.5) +
  labs(title = "Information Propagation Over Time on November 12th",
       x = "Time",
       y = "Accumulation of tweets, quotes, replies, and retweets") +
  scale_shape_manual(values = c(15, 16, 17, 18)) + 
  theme_minimal() +
  scale_x_datetime(labels = scales::time_format("%H:%M"),
                   breaks = scales::breaks_width("4 hours"))
