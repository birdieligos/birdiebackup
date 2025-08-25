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


sb <- read.csv('/Users/birdieligos/Downloads/OES_SACRAMENTO_SPANISH_042025.csv', stringsAsFactors = FALSE)
sb <- select(sb, V1_AGE, V1_GENDER, V1_ETHNICITY)
names(sb)[names(sb) == "V1_AGE"] <- "Age"
names(sb)[names(sb) == "V1_GENDER"] <- "Gender"
names(sb)[names(sb) == "V1_ETHNICITY"] <- "Ethnicity"
sb$County <- 'Sacramento'

write.csv(sb, file = '/Users/birdieligos/Documents/Reports/countyupload051925.csv', row.names = FALSE)


############################## write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "OES_2024", table = "OES_2024")

# Append data to the BigQuery table
bq_table_upload(bq_table, sb, write_disposition = "WRITE_APPEND")

