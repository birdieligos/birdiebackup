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
library(readr)
library(dplyr)
library(purrr)
library(stringr)
unloadNamespace("plyr")

########################################
########################################
# EMPOWER PEOPLE DATABASE
######################################## (OVERWRITES)
########################################

wanted_files <- c(
  "SubSectorE-Spanish.csv",
  "SubSectorE-English (1).csv",
  "SubSectorD-Spanish.csv",
  "SubSectorD-English.csv",
  "SubSectorC-Spanish.csv",
  "SubSectorC-English.csv",
  "SubSectorB-Spanish.csv",
  "SubSectorB-English (1).csv",
  "SubSectorA-Spanish (1).csv",
  "SubSectorA-English (2).csv"
)

files <- file.path(
  "/Users/birdieligos/Downloads",
  wanted_files
)

# then
combined <- files %>%
  map_dfr(~{
    info <- str_match(basename(.x),
                      "^SubSector([A-E])-(English|Spanish)")[,2:3]
    read_csv(.x, show_col_types = FALSE) %>%
      mutate(
        LANGUAGE  = info[2],
        GEOGRAPHY = paste("Sub Sector", info[1])
      )
  })


print(colnames(combined))

people <- select(combined, WIRELESSPHONENUMBER, PHONENUMBER, RES_ADDRESS1, RES_ADDRESS2, RA_ZIP, CD, SD, AD, SUPERVISORIAL, CITYCODE,
                 V1_PDIID, V1_PARTY, HOUSEPARTYTYPECODE, V1_GENDER, V1_ETHNICITY, V1_REGDATE, V1_AGE, V1_PVBM, LANGUAGE, GEOGRAPHY)

people <- people %>%
  mutate(
    V1_REGDATE = as.Date(
      V1_REGDATE,
      format = "%m/%d/%Y %I:%M:%S %p"
    )
  )

people <- people %>%
  filter(!(is.na(WIRELESSPHONENUMBER) & is.na(PHONENUMBER)))


# count NAs in V1_PDIID
na_count_V1_PDIID <- sum(is.na(people$V1_PDIID))
na_count_V1_PDIID
############################## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "DEMOGRAPHIC_DATABASE")

bq_table_upload(bq_table, people, write_disposition = "WRITE_TRUNCATE")



