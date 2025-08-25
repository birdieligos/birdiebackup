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

#
oes <- bq_table_download("slstrategy.OES_2024.OES_2024", bigint = "integer64")

#
data <- read.csv('/Users/birdieligos/Downloads/hubdialer_call_attempts_household_report_42815_from_03_01_2025_through_03_25_2025.csv', stringsAsFactors = FALSE)
orange <- read.csv('/Users/birdieligos/Documents/OES HUBDIALER UPLOAD FILES/Orange_English_Upload_2025-03-10.csv', stringsAsFactors = FALSE)

data <- filter(data, County == 'Orange')
data <- filter(data, Status %in% c("Human", "Do Not Call", "Moved", "Wrong Number"))

# Remove leading/trailing white space from both columns
orange$WIRELESSPHONENUMBER <- trimws(orange$WIRELESSPHONENUMBER)
data$Phone.Number <- trimws(data$Phone.Number)
orange <- orange[!(orange$WIRELESSPHONENUMBER %in% data$Phone.Number), ]
orange <- orange %>%
  mutate(WIRELESSPHONENUMBER = trimws(WIRELESSPHONENUMBER)) %>%
  anti_join(
    data %>% mutate(Phone_Number = trimws(Phone.Number)),
    by = c("WIRELESSPHONENUMBER" = "Phone_Number")
  )

write.csv(orange, file = '/Users/birdieligos/Documents/OES HUBDIALER UPLOAD FILES/ORANGETEST.csv', row.names = FALSE)


#


entirelist <- read.csv('/Users/birdieligos/Downloads/3122025_CV_SanBernTexts.csv', stringsAsFactors = FALSE)
orange <- entirelist

strikelist <- read.csv('/Users/birdieligos/Documents/Reports/CAVOL_STRIKELIST_032425.csv', stringsAsFactors = FALSE)
data <- strikelist

orange$WIRELESSPHONENUMBER <- trimws(orange$WIRELESSPHONENUMBER)
data$phone <- trimws(data$phone)
orange <- orange[!(orange$WIRELESSPHONENUMBER %in% data$Phone.Number), ]
orange <- orange %>%
  mutate(WIRELESSPHONENUMBER = trimws(WIRELESSPHONENUMBER)) %>%
  anti_join(
    data %>% mutate(phone = trimws(phone)),
    by = c("WIRELESSPHONENUMBER" = "phone")
  )


write.csv(orange, file = '/Users/birdieligos/Documents/Reports/CVsend032525.csv', row.names = FALSE)

