library(readxl)
library(dplyr)
#preping data for gephi
level <- read_xlsx("AAR.xlsx")
nodes <- read_xlsx("AAR - nodes.xlsx")

level <- level %>%
  mutate(Agency = ifelse(Agency == "National Urricane Center (NHC)", "NHC", Agency))

nodes <- nodes %>%
  column_to_rownames(1)

all_column_names <- colnames(nodes)

# Replace NA values with 0 in specific columns
columns_to_replace <- all_column_names
nodes[, columns_to_replace][is.na(nodes[, columns_to_replace])] <- 0

# Reshape the original tibble into long format
long_tibble <- nodes %>%
  rownames_to_column("row") %>%
  pivot_longer(cols = -row, names_to = "col", values_to = "value")

# Filter pairs where the value is 1
result_tibble <- long_tibble %>%
  filter(value == 1) %>%
  select(x = row, y = col)

level <- level %>% select(-`Document`)
#edge <- karate_d3$nodes
edge <- dplyr::summarise(dplyr::group_by(result_tibble, x, y),count =n())


Type <- rep("Dirrected", nrow(edge))
edge$Type <- Type

colnames(edge) <- c("Source", "Target", "Weight", "Type")
edge <- edge[,c("Source", "Target", "Type", "Weight")]
write.csv(level, "Updated Levels.csv", row.names = FALSE)
write.csv(edge, "AAR edgelist.csv")

#update dataset
data <- read.csv("Updated Levels.csv")
library(dplyr)
data <- data %>%
  mutate(`Splitter Z-Level` = case_when(
    Level.of.Agency == "Local" ~ 1,
    Level.of.Agency == "Regional" ~ 2,
    Level.of.Agency == "State" ~ 3,
    Level.of.Agency == "National" ~ 4
  ))

write.csv(data, "Updated Levels.csv", row.names = FALSE)

###############################
levelCEMP <- read_xlsx("Brevard County CEMP.xlsx")
nodesCEMP <- read_xlsx("Brevard County CEMP - nodes.xlsx")

nodesCEMP <- nodesCEMP[-22, ]

#deleting extra column with all na values
nodesCEMP <- nodesCEMP %>% select( -`All other County ESF primary agencies`, -`Health First Holmes Regional Medical Center`)

nodesCEMP <- nodesCEMP %>%
  column_to_rownames(1)

all_column_namesCEMP <- colnames(nodesCEMP)

# Replace NA values with 0 in specific columns
columns_to_replaceCEMP <- all_column_namesCEMP
nodesCEMP[, columns_to_replaceCEMP][is.na(nodesCEMP[, columns_to_replaceCEMP])] <- 0

# Reshape the original tibble into long format
long_tibbleCEMP <- nodesCEMP %>%
  rownames_to_column("row") %>%
  pivot_longer(cols = -row, names_to = "col", values_to = "value")

# Filter pairs where the value is 1
result_tibbleCEMP <- long_tibbleCEMP %>%
  filter(value == 1) %>%
  select(x = row, y = col)

#deleting extra agencies that dont have listed levels 
delete <- c("Local Area Commanders", "Westside Wildlife Hospital", "Coastal Transport Systems",
            "Brevard County Utility Services...75", "Public Safety Answering Point (PSAP)",
            "Private Sector Communications Providers")

# Filter out instances of these agencies from both Source and Target columns
result_tibbleCEMP <- result_tibbleCEMP %>%
  filter(!(x %in% delete) & !(y %in% delete))


levelCEMP <- levelCEMP %>% select(-`Document`)

additional_data <- data.frame(
  Agency = c("Brevard County Utility Services...23", "Pastoral Care", "Municipal Emergency Management Liaison Officers"),
  `Level of Agency` = c("Local", "Local", "Local"),
  `Type of Agency` = c("Public", "Nonprofit", "Public")
)
colnames(additional_data) <- c("Agency", "Level of Agency", "Type of Agency")

# Combine the original data with the additional rows
levelCEMP <- rbind(levelCEMP, additional_data)
edge <- dplyr::summarise(dplyr::group_by(result_tibbleCEMP, x, y),count =n())

Type <- rep("Dirrected", nrow(edge))
edge$Type <- Type

colnames(edge) <- c("Source", "Target", "Weight", "Type")
edge <- edge[,c("Source", "Target", "Type", "Weight")]

levelCEMP <- levelCEMP %>%
  mutate(`Splitter Z-Level` = case_when(
    `Level of Agency` == "Local" ~ 1,
    `Level of Agency` == "Regional" ~ 2,
    `Level of Agency` == "State" ~ 3,
    `Level of Agency` == "Federal" ~ 4,
    `Level of Agency` == "National" ~ 5
  ))
levelCEMP <- levelCEMP %>%
  filter(!(Agency %in% delete))

write.csv(levelCEMP, "Updated Levels CEMP.csv", row.names = FALSE)
write.csv(edge, "CEMP Edgelist.csv", row.names = FALSE)
