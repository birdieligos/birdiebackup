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
library(readxl)
unloadNamespace("plyr")

#################################### 
# CONVOSO
#################################### OVERWRITES
adterm <- read.csv('/Users/birdieligos/Downloads/KCAW Assembly District Sentiment - Copy of Composite Scores (1).csv', stringsAsFactors = FALSE)
adterm$DISTRICT_TYPE <- "Assembly"
sdterm <- read.csv('/Users/birdieligos/Downloads/KCAW Senate District Sentiment - Copy of ALL SCORES (1).csv', stringsAsFactors = FALSE)
sdterm$DISTRICT_TYPE <- "Senate"


names(adterm) <- toupper(names(adterm))
names(sdterm) <- toupper(names(sdterm))


district <- rbind(adterm, sdterm)

adterm <- district

names(adterm) <- toupper(gsub("\\.", "_", names(adterm)))

adterm <- adterm %>%
  mutate(TERM_LIMIT = as.Date(paste0(TERM_LIMIT, "-01-01")))
################# OVERWRITES write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "CAMPAIGN2026", table = "CAChamberAnalysis071625")

# Append data to the BigQuery table
bq_table_upload(bq_table, adterm, write_disposition = "WRITE_TRUNCATE")

