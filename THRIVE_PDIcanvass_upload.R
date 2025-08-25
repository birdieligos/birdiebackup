# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


# Read file
upload <- read.csv('/Users/birdieligos/Downloads/AllFlags(1) (13).csv', stringsAsFactors = FALSE)

upload <- upload %>%
  distinct()


upload <- upload %>%
  mutate(
    ts   = ymd_hms(FLAGENTRYDATE, tz = "UTC"),  # lubridate handles 'T'
    DATE = as.Date(ts),
    TIME = format(ts, "%H:%M:%S")
  ) %>%
  select(-FLAGENTRYDATE, -ts)

upload <- filter(upload, FLAGNAME == "Mobile Canvassing")

# Format column names
colnames(upload) <- gsub("\\.", "_", colnames(upload))

print(colnames(upload))

upload <- select(upload, PDIID, STATEID, USERNAME, RESPONSEDESCRIPTION, COUNTYASSIGNEDID, PRECINCT,
                 DATE, TIME)

# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "THRIVE", table = "PDI_Summer25")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")

