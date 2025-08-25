# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)

upload <- read.csv('/Users/birdieligos/Downloads/log_107910_zgi32mvgidlr26bkvch7qkllf1caw8km_2025-06-24_17_27_53.csv')

print(colnames(upload))
# Format column names
colnames(upload) <- gsub("\\.", "_", colnames(upload))

# Parse date fields
upload <- upload %>%
  mutate(
    ts   = ymd_hms(Time_Stamp__PST_),
    Date = as.Date(ts),
    Time = format(ts, "%H:%M:%S")
  ) %>% select(-Time_Stamp__PST_, -ts)

upload <- upload %>%
  mutate(
    ts = ymd_hms(Final_Reached_At),
    Final_Reach_Date = as.Date(ts),
    Final_Reach_Time = format(ts, "%H:%M:%S")
  ) %>% select(-Final_Reached_At, -ts)

upload <- upload %>%
  mutate(
    ts = ymd_hms(Created_At__Time_),
    Created_Lead_Date = as.Date(ts),
    Created_Lead_Time = format(ts, "%H:%M:%S")
  ) %>% select(-Created_At__Time_, -ts)

# Select only necessary columns
upload <- select(upload,
                 Date, Number_Dialed, Status_Name, Talk_Time,
                 Applecart_Cuomo___Rank_1st_Choice,
                 Can_we_count_on_you_to_at_least_rank_Andrew_Cuomo_,
                 Applecart_Cuomo___3_to_5_Choice, Ethnicity, Gender, Postal_Code
)

print(colnames(upload))
upload$IMPRESSIONS <- 1

upload <- upload %>%
  mutate(
    Ranked_Choice = case_when(
      str_detect(Applecart_Cuomo___Rank_1st_Choice,
                 regex("^Yes", ignore_case = TRUE)) ~ Applecart_Cuomo___Rank_1st_Choice,
      str_detect(Can_we_count_on_you_to_at_least_rank_Andrew_Cuomo_,
                 regex("^Yes", ignore_case = TRUE)) ~ Can_we_count_on_you_to_at_least_rank_Andrew_Cuomo_,
      !is.na(Applecart_Cuomo___3_to_5_Choice) &
        str_trim(Applecart_Cuomo___3_to_5_Choice) != "" ~ Applecart_Cuomo___3_to_5_Choice,
      TRUE ~ NA_character_
    )
  ) %>%
  select(-Applecart_Cuomo___Rank_1st_Choice,
         -Can_we_count_on_you_to_at_least_rank_Andrew_Cuomo_,
         -Applecart_Cuomo___3_to_5_Choice)


upload <- upload %>%
  select(-Talk_Time) %>%
  group_by(
    Date,
    Number_Dialed,
    Status_Name,
    Ethnicity,
    Gender,
    Postal_Code,
    Ranked_Choice
  ) %>%
  summarise(
    Impressions = sum(IMPRESSIONS, na.rm = TRUE),
    .groups = "drop"
  )

# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "Applecart_NYC", table = "Cuomo_June2025")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")
