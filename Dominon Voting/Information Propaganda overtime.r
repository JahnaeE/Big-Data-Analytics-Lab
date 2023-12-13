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
tweets$Date <- format(tweets$Date, tz = "America/New_York", usetz = TRUE)

tweets$Date <- sub(" EST", "", tweets$Date)
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

#using exact time 
ggplot(result, aes(x = time, y = Combined_melted, color = variable, shape = variable, size = Impressions)) +
  geom_point() +
  labs(title = "Information Propagation Over Time on November 7th",
       x = "Time",
       y = "Accumulation of tweets, quotes, replies, and retweets") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +  # Manually set shapes
  theme_minimal() +
  scale_x_datetime(labels = scales::time_format("%H:%M"),
                   breaks = scales::breaks_width("4 hours"))
