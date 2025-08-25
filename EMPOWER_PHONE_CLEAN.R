library(bigrquery)
library(curl)
library(dplyr)
library(lubridate)
library(plyr)
library(httpuv)
library(base)
library(googledrive)
library(stringr)
library(tidyr)
library(stringi)
library(Hmisc)


sector2 <- read.csv('/Users/birdieligos/Downloads/SECTOR 2 ENGLISH - HUBDIALER LIST.csv', stringsAsFactors = FALSE)



scrub <- read.csv('/Users/birdieligos/Downloads/ENGLISHOHONESCRUB.csv', stringsAsFactors = FALSE)


scrub$phone <- sub("^1", "", as.character(scrub$phone))

sector2 <- sector2[!(sector2$WIRELESSPHONENUMBER %in% scrub$phone), ]

write.csv(sector2, file = '/Users/birdieligos/Documents/Reports/SECTOR2PHONE_SCRUBBED_040925.csv', row.names = FALSE)

###


sector2 <- read.csv('/Users/birdieligos/Downloads/SECTOR 2 SPANISH - TEXT LIST.csv', stringsAsFactors = FALSE)



scrub <- read.csv('/Users/birdieligos/Downloads/EMPOWERTEXTSCRUB.csv', stringsAsFactors = FALSE)


scrub$phone <- sub("^1", "", as.character(scrub$phone))

sector2 <- sector2[!(sector2$WIRELESSPHONENUMBER %in% scrub$phone), ]

write.csv(sector2, file = '/Users/birdieligos/Documents/Reports/SECTOR2_SCRUBBEDSPANISH_040925.csv', row.names = FALSE)





