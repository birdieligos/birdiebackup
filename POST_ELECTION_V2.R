# Load required libraries
library(bigrquery)
library(curl)
library(dplyr)
library(lubridate)
library(httpuv)
library(base)
library(googledrive)
library(stringr)
library(tidyr)
unloadNamespace("plyr")


# PDI ELECTION PRECINCT TXT FILE
file_path <- "/Users/birdieligos/Downloads/pct_24GElectionPrecinctsCo19LosAngeles.tsv"

la <- read.delim(file_path, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip=4)

la <- select(la, Election.Precinct, AD, SD, CD)

colnames(la)[colnames(la) == "Election.Precinct"] <- "Precinct"

################################### RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MEASURE_A_LOS_ANGELES.csv', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# keep TOTAL only
ad31 <- ad31[ad31$TYPE == "TOTAL", ]

# replace space with _
colnames(ad31) <- gsub(" AND | ", "_", colnames(ad31))

# select columns 
ad31 <- select(ad31, PRECINCT, REGISTRATION, BALLOTS_CAST, YES, NO)

# convert to int
ad31$REGISTRATION <- as.integer(ad31$REGISTRATION)
ad31$BALLOTS_CAST <- as.integer(ad31$BALLOTS_CAST)
ad31$YES <- as.integer(ad31$YES)
ad31$NO <- as.integer(ad31$NO)

result <- ad31

result$PRECINCT <- trimws(result$PRECINCT)

###################################################### PDI FILES 

LASD1 <- read.csv("/Users/birdieligos/Documents/Reports/MA_LASD1_030525.csv", stringsAsFactors = FALSE)
LASD2 <- read.csv("/Users/birdieligos/Documents/Reports/MA_LASD2_030525.csv", stringsAsFactors = FALSE)
LASD3 <- read.csv("/Users/birdieligos/Documents/Reports/MA_LASD3_030525.csv", stringsAsFactors = FALSE)
LASD4 <- read.csv("/Users/birdieligos/Documents/Reports/MA_LASD4_030525.csv", stringsAsFactors = FALSE)
LASD5 <- read.csv("/Users/birdieligos/Documents/Reports/MA_LASD5_030525.csv", stringsAsFactors = FALSE)
SanGabriel <- read.csv("/Users/birdieligos/Documents/Reports/MA_SanGabriel_030525.csv", stringsAsFactors = FALSE)
SanFernando <- read.csv("/Users/birdieligos/Documents/Reports/MA_SanFernando_030525.csv", stringsAsFactors = FALSE)
CentralLA <- read.csv("/Users/birdieligos/Documents/Reports/MA_CentralLA_030525.csv", stringsAsFactors = FALSE)
CityOfLosAngeles <- read.csv("/Users/birdieligos/Documents/Reports/MA_CityOfLosAngeles_030525.csv", stringsAsFactors = FALSE)
LACOUNTYWIDE <- read.csv("/Users/birdieligos/Documents/Reports/MA_LACOUNTYWIDE_030725.csv", stringsAsFactors = FALSE)

# Apply transformations to each dataframe
LASD1 <- LASD1 %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "DISTRICT 1") %>% rename(PRECINCT = Description)
LASD2 <- LASD2 %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "DISTRICT 2") %>% rename(PRECINCT = Description)
LASD3 <- LASD3 %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "DISTRICT 3") %>% rename(PRECINCT = Description)
LASD4 <- LASD4 %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "DISTRICT 4") %>% rename(PRECINCT = Description)
LASD5 <- LASD5 %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "DISTRICT 5") %>% rename(PRECINCT = Description)
SanGabriel <- SanGabriel %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "SanGabriel") %>% rename(PRECINCT = Description)
SanFernando <- SanFernando %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "SanFernando") %>% rename(PRECINCT = Description)
CentralLA <- CentralLA %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "CentralLA") %>% rename(PRECINCT = Description)
CityOfLosAngeles <- CityOfLosAngeles %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "CityOfLosAngeles") %>% rename(PRECINCT = Description)
LACOUNTYWIDE <- LACOUNTYWIDE %>% filter(Total != 0) %>% select(-Total) %>% mutate(AREA = "LACOUNTYWIDE") %>% rename(PRECINCT = Description)

# Trim whitespace from PRECINCT column
LASD1$PRECINCT <- trimws(LASD1$PRECINCT)
LASD2$PRECINCT <- trimws(LASD2$PRECINCT)
LASD3$PRECINCT <- trimws(LASD3$PRECINCT)
LASD4$PRECINCT <- trimws(LASD4$PRECINCT)
LASD5$PRECINCT <- trimws(LASD5$PRECINCT)
SanGabriel$PRECINCT <- trimws(SanGabriel$PRECINCT)
SanFernando$PRECINCT <- trimws(SanFernando$PRECINCT)
CentralLA$PRECINCT <- trimws(CentralLA$PRECINCT)
CityOfLosAngeles$PRECINCT <- trimws(CityOfLosAngeles$PRECINCT)
LACOUNTYWIDE$PRECINCT <- trimws(LACOUNTYWIDE$PRECINCT)

# rbind with LACOUNTYWIDE included
combined_df <- rbind(
  LASD1, LASD2, LASD3, LASD4, LASD5,
  SanGabriel, SanFernando, CentralLA, CityOfLosAngeles,
  LACOUNTYWIDE
)

# Remove "19" prefix from PRECINCT
combined_df$PRECINCT <- sub("^19", "", combined_df$PRECINCT)

########################### MERGE RESULTS WITH AREAS

merged_df <- result %>%
  full_join(combined_df, by = "PRECINCT")

merged_df <- merged_df[merged_df$REGISTRATION != 0, ]

colnames(merged_df)[colnames(merged_df) == "REGISTRATION"] <- "REGISTERED_VOTERS"

merged_df <- merged_df[!is.na(merged_df$AREA), ]

merged_df <- select(merged_df, -PRECINCT)

merged_df <- merged_df %>%
  group_by(AREA) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE))

merged_df$AREA <- gsub("CentralLA", "Central LA", merged_df$AREA)
merged_df$AREA <- gsub("CityOfLosAngeles", "City Of Los Angeles", merged_df$AREA)
merged_df$AREA <- gsub("SanFernando", "San Fernando Valley", merged_df$AREA)
merged_df$AREA <- gsub("SanGabriel", "San Gabriel Valley", merged_df$AREA)

merged_df <- merged_df %>%
  mutate(across(everything(), ~ trimws(.)))


######## read in POLLING GEO 

poll_geo <- read.csv('/Users/birdieligos/Downloads/Copy of LA MEASURE A 724 TABS V1.xlsx - VOTE COMPARISON GEO (4).csv', stringsAsFactors = FALSE)

poll_geo <- poll_geo %>%
  mutate(across(everything(), ~ trimws(.)))

poll_geo <- poll_geo %>% 
  mutate(AREA = case_when(
    AREA == "LASD1" ~ "DISTRICT 1",
    AREA == "LASD2" ~ "DISTRICT 2",
    AREA == "LASD3" ~ "DISTRICT 3",
    AREA == "LASD4" ~ "DISTRICT 4",
    AREA == "LASD5" ~ "DISTRICT 5",
    TRUE ~ AREA
  ))


####  join 

poll_results <- full_join(merged_df, poll_geo, by = "AREA")

poll_results <- poll_results %>%
  mutate(across(-AREA, as.integer))


#######################################  WRITE RESULTS TO BIG Q

library(bigrquery)

project_id <- "slscampaigns-364520"
dataset_id <- "Measure_A"
table_id <- "POST_GE24_RESULTS_AREA"

schema <- list(
  bq_field("AREA", "STRING"),
  bq_field("REGISTERED_VOTERS", "INT64"),
  bq_field("BALLOTS_CAST", "INT64"),
  bq_field("YES", "INT64"),
  bq_field("NO", "INT64"),
  bq_field("n", "INT64"),
  bq_field("YES_PVT", "INT64"),
  bq_field("NO_PV", "INT64"),
  bq_field("UNDECIDED_PV", "INT64")
)

bq_table <- bq_table(project_id, dataset_id, table_id)

bq_table_upload(
  x = bq_table,
  values = poll_results,
  fields = schema,
  write_disposition = "WRITE_TRUNCATE"
)



############################################# DEMO TURNOUT DATA 

demo <- read.csv('/Users/birdieligos/Documents/Reports/MA_DEMOS_030625.csv', stringsAsFactors = FALSE)


demo_xtabs <- read.csv('/Users/birdieligos/Documents/Reports/MA_DEMOXTABS.csv', stringsAsFactors = FALSE)


demo_xtabs <- demo_xtabs %>%
  pivot_longer(
    cols = -DEMOGRAPHIC,
    names_to = "SECOND_DEMO",
    values_to = "COUNT"
  ) %>%
  mutate(
    TYPE = ifelse(grepl(".VT24G", SECOND_DEMO), "VOTED", "REG_VOTERS"),
    SECOND_DEMO = gsub(".VT24G", "", SECOND_DEMO),
    DEMOGRAPHIC = paste(DEMOGRAPHIC, SECOND_DEMO)
  ) %>%
  select(DEMOGRAPHIC, TYPE, COUNT) %>%
  pivot_wider(names_from = TYPE, values_from = COUNT, values_fill = list(COUNT = 0))


demo_xtabs <- demo_xtabs %>%
  mutate(DEMOGRAPHIC = ifelse(DEMOGRAPHIC %in% c("Progressive Rep", "Liberal Rep"), "Prog/Liberal Rep", DEMOGRAPHIC)) %>%
  group_by(DEMOGRAPHIC) %>%
  summarise(across(c(REG_VOTERS, VOTED), sum, na.rm = TRUE), .groups = "drop")

# rbind 

demo_total <- rbind(demo, demo_xtabs)

# polling

poll_demo <- read.csv('/Users/birdieligos/Downloads/Copy of LA MEASURE A 724 TABS V1.xlsx - VOTE COMPARISON DEMOS (3).csv', stringsAsFactors = FALSE)


# merge 

poll_demo <- poll_demo %>%
  mutate(DEMOGRAPHIC = str_trim(DEMOGRAPHIC))

demo_total <- demo_total %>%
  mutate(DEMOGRAPHIC = str_trim(DEMOGRAPHIC))

demo_total <- demo_total %>%
  mutate(DEMOGRAPHIC = str_replace_all(DEMOGRAPHIC, c("No Party Preference" = "NPP", "Democrat" = "Dem", "Republican" = "Rep")))

merged_demo <- left_join(poll_demo, demo_total, by = "DEMOGRAPHIC")


#######################################  WRITE RESULTS TO BIG Q

library(bigrquery)

project_id <- "slscampaigns-364520"
dataset_id <- "Measure_A"
table_id <- "POST_GE24_RESULTS_DEMO"

schema <- list(
  bq_field("DEMOGRAPHIC", "STRING"),
  bq_field("n", "INT64"),
  bq_field("YES_PV", "INT64"),
  bq_field("NO_PV", "INT64"),
  bq_field("UNDECIDED", "INT64"),
  bq_field("REG_VOTERS", "INT64"),
  bq_field("VOTED", "INT64")
)

bq_table <- bq_table(project_id, dataset_id, table_id)

bq_table_upload(
  x = bq_table,
  values = merged_demo,
  fields = schema,
  write_disposition = "WRITE_TRUNCATE"
)





