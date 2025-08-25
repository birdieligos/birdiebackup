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

################## District Databases
# ad27
table <- bq_table_download("slscampaigns-364520.Soria_2024.2024_GElection_Results")

table <- table %>%
  mutate(District = case_when(
    WITHIN_AD27 == "YES" & WITHIN_SD14 == "YES" ~ "AD27 AND SD14",
    WITHIN_AD27 == "YES" & WITHIN_SD14 == "NO/NA" ~ "AD27 ONLY",
    WITHIN_SD14 == "YES" & WITHIN_AD27 == "NO/NA" ~ "SD14 ONLY",
    WITHIN_AD27 == "NO/NA" & WITHIN_SD14 == "NO/NA" ~ "OTHER",
    TRUE ~ NA_character_ # Handle any unexpected cases
  ))

write.csv(table, file = '/Users/birdieligos/Documents/Reports/testtable.csv', row.names = FALSE)
