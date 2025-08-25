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

file_path <- '/Users/birdieligos/Downloads/OES_STANISLAUS_ENGLISH_032525.csv'

new <- read.csv(file_path, stringsAsFactors = FALSE)

print(colnames(new))

##### USE IF FILE HAS MOBILES AND LANDLINES
#new$WIRELESSPHONENUMBER[is.na(new$WIRELESSPHONENUMBER)] <- new$PHONENUMBER[is.na(new$WIRELESSPHONENUMBER)]
#new <- select(new, -CITYCODE, -V1_PDIID, -V1_MIDDLENAME)

# choose county name
County <- 'Stanislaus'
Language <- 'English'

new$COUNTYCODE <- County

# write.csv
todays_date <- format(Sys.Date(), "%Y-%m-%d")
file_path <- paste0("/Users/birdieligos/Documents/OES HUBDIALER UPLOAD FILES/", County, '_', Language, "_Upload_", todays_date, ".csv")
write.csv(new, file = file_path, row.names = FALSE)



