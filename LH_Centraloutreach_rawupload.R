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

####################################  
# PDI Flag Activity Log
####################################  OVERWRITES

pdi <- read.csv('/Users/birdieligos/Downloads/AllFlags (10) - AllFlags (10).csv', stringsAsFactors = FALSE)

upload <- pdi
# format
colnames(upload) <- gsub("\\.", "_", colnames(upload))

upload <- upload %>%
  select(
    -STATEID,
    -USERNAME,
    -CANDIDATEDESCRIPTION,
    -COUNTYASSIGNEDID,
    -CANVASSERNAME,
    -PHONEBANK
  )


upload$FLAGENTRYDATE <- as.Date(upload$FLAGENTRYDATE, format = "%Y-%m-%dT%H:%M:%OS")

upload <- upload %>%
  rename(
    DATE = FLAGENTRYDATE,
    CALL_TO_ACTION = SURVEYQUESTIONSHORTTEXT,
    PROJECT = MOBILEPROJECTASSIGMENT,
    OUTREACH_TYPE = FLAGNAME
  )


############## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "liberty-hill-462819", dataset = "Centralized_Outreach", table = "Activity_Log_2025")

bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")

