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

file_path <- '/Users/birdieligos/Downloads/OES_Sutter_Spanish_02152025.csv'

new <- read.csv(file_path, stringsAsFactors = FALSE)


##### USE IF FILE HAS MOBILES AND LANDLINES
new$WIRELESSPHONENUMBER[is.na(new$WIRELESSPHONENUMBER)] <- new$PHONENUMBER[is.na(new$WIRELESSPHONENUMBER)]

new <- select(new, -CITYCODE, -V1_PDIID, -V1_MIDDLENAME)

# choose county name
County <- 'Sutter'
Language <- 'Spanish'

new$COUNTYCODE <- County

# write.csv
todays_date <- format(Sys.Date(), "%Y-%m-%d")
file_path <- paste0("/Users/birdieligos/Documents/OES HUBDIALER UPLOAD FILES/", County, '_', Language, "_Upload_", todays_date, ".csv")
write.csv(new, file = file_path, row.names = FALSE)





