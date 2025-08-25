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


upload <- read.csv('/Users/birdieligos/Downloads/log_107910_j07vu8h51897j140vay2g8vlt1lqx017_2025-05-23_15_54_19.csv', stringsAsFactors = FALSE)

colnames(upload) <- gsub("\\.", "_", colnames(upload))

upload <- upload %>%
  mutate(
    ts   = ymd_hms(Time_Stamp__PST_),
    Date = as.Date(ts),
    Time = format(ts, "%H:%M:%S")
  ) %>%
  select(-Time_Stamp__PST_, -ts)

upload <- select(upload, Status_Name, District, Party, Ethnicity, Gender, Age, Postal_Code, Undecided_Reasons, No_Reasons, No_on_AB_84, PDI_ID, Date, Time)

############################## write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "CCSA_2025", table = "CCSA_PATCHTHRU_CALLS")

# Append data to the BigQuery table
bq_table_upload(bq_table, upload, write_disposition = "WRITE_APPEND")