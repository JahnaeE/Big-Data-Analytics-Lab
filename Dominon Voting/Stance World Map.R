library(dplyr)
library(tidyr)
library(ggplot2)
library(tidygeocoder)
library(maps)
library(reshape2)
library(purrr)
library(ggrepel)
library(usmap)
library(ggspatial)
library(sqldf)
library(tigris)
library(plotly)
library(htmlwidgets)

setwd("C:/Users/13212/OneDrive/Documents/College/USF/GA Position/Dominon/Stance")
combined_results <- read.csv("complete_stance.csv")
combined_location_names <- data.frame(combined_results$Date, combined_results$Location.Name, combined_results$stance)

combined_location_names <- combined_location_names %>%
  drop_na(combined_results.Location.Name)

colnames(combined_location_names) <- c("Date", "Location_Name", "Stance")

#now including the top stance
#the stance with the highest count represents that locations overall stance
locationCounts <- combined_location_names %>%
  filter(Location_Name != "") %>% 
  group_by(Location_Name) %>%
  summarise(
    location_count = n(),
    in_favor_count = sum(Stance == "in favor"),
    against_count = sum(Stance == "against"),
    neutral_count = sum(Stance == "neutral or unsure"),
    representing_stance = case_when(
      in_favor_count >= against_count & in_favor_count >= neutral_count ~ "in favor",
      against_count >= in_favor_count & against_count >= neutral_count ~ "against",
      TRUE ~ "neutral or unsure"
    )
  ) %>%
  arrange(desc(location_count))

combined_GeoCodes <- locationCounts %>%
  select(Location_Name, location_count, representing_stance) %>%
  geocode(address = Location_Name,
          method = "arcgis")

combined_GeoCodes_Adresses <- combined_GeoCodes %>%
  reverse_geocode(lat = lat, long = long, method = 'arcgis',
                  address = address_found, full_results = TRUE) %>%
  left_join(locationCounts, by = c("Location_Name" = "Location_Name"))

world_coordinates <- map_data("world")

#map including stance
#updated colors to represent the stance for that location
ggplot(data = combined_GeoCodes, aes(long, lat, fill = representing_stance)) +
  geom_map(data = world_coordinates, map = world_coordinates, 
           aes(long, lat, map_id = region), 
           color = "white", fill = "grey", size = 0.2) +
  geom_point(alpha = 0.5, shape = 21, size = 2) +
  labs(title = "BrandWatch Location Data - World View") +
  scale_fill_manual(values = c("in favor" = "green", "against" = "red", "neutral or unsure" = "blue"))

#interactive world map including stance
world_coordinates <- map_data("world")

world_map <- plot_geo(combined_GeoCodes, locationmode = 'ISO-3') 

world_map <- world_map %>% add_markers(
  x = ~long, y = ~lat,
  text = ~paste(Location_Name, "<br />Count: ", location_count),
  size = 0.5, color = ~representing_stance,
  colors = c("in favor" = "green", "against" = "red", "neutral or unsure" = "blue"),
  opacity = 0.7, symbol = I("circle")
)

world_map <- world_map %>% layout(
  title = 'BrandWatch Location Data - World View',
  geo = list(
    showland = TRUE,
    landcolor = toRGB("gray95"),
    subunitcolor = toRGB("gray85"),
    countrycolor = toRGB("gray85"),
    countrywidth = 0.5,
    subunitwidth = 0.5
  )
)

# Display the interactive plot
world_map

#save as html 
saveWidget(world_map, file = "bdstance_mapworld.html")
