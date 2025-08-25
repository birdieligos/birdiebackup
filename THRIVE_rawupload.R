# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


# Read file
upload <- read.csv('/Users/birdieligos/Downloads/log_107910_itzvi4kf5dux65l81dtgd26xejgta6ej_2025-07-24_12_16_24.csv', stringsAsFactors = FALSE)

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
print(colnames(upload))
# Select only necessary columns
upload <- select(upload,
                 Date, Number_Dialed, Status_Name, Talk_Time,
                 Thrive_Alternate_Issue_Text_Box, ThriveLA_Newsletter_Sign_up,
                 Thrive_Q1___Other_Open_Response, ThriveQ1___Priority_Issues,
                 Thrive_Q2___Scale_1_to_5_satisfaction_with_issue,
                 Thrive_Q3___Homelessness_Follow_up, Thrive_Q3___Housing_Follow_up,
                 Thrive_Q4___Public_Services_Satisfaction, q6___thrive_community_programs,
                 Q6___Thrive_Open_Response, Thrive_Alignment_Scale, Thrive_Q3___Cost_of_Living_Follow_up_Question,
                 Thrive_Q3___Public_Safety_Follow_up
)

print(str(upload))
# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "THRIVE", table = "Convoso_June25")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")
