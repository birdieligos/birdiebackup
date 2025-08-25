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




s1 <- read.csv('/Users/birdieligos/Downloads/SECTOR 1 ENGLISH - EMPOWER_LAUNCH1_ENGLISH_021825.csv', stringsAsFactors = FALSE)
s1 <- select(s1, WIRELESSPHONENUMBER, V1_PDIID)

s2 <- read.csv('/Users/birdieligos/Downloads/SECTOR 1 SPANISH - EMPOWER_LAUNCH1_SPANISH_021825.csv', stringsAsFactors = FALSE)
s2 <- select(s2, WIRELESSPHONENUMBER, V1_PDIID)

s3 <- read.csv('/Users/birdieligos/Downloads/SECTOR 2 ENGLISH - empower_pulllist_nonspanish_031725 (1).csv', stringsAsFactors = FALSE)
s3 <- select(s3, WIRELESSPHONENUMBER, V1_PDIID)

s4 <- read.csv('/Users/birdieligos/Downloads/SECTOR 2 SPANISH - empower_pulllist_spanish_031725 (2).csv', stringsAsFactors = FALSE)
s4 <- select(s4, WIRELESSPHONENUMBER, V1_PDIID)


s <- rbind(s1, s2, s3, s4)

source <- s


scrub <- read.csv('/Users/birdieligos/Downloads/Empower_Scrub (1).csv', stringsAsFactors = FALSE)

joined <- scrub %>%
  left_join(source, by = c("phone" = "WIRELESSPHONENUMBER"))

write.csv(joined, file = '/Users/birdieligos/Documents/Reports/empowertextscrubpdiupload051925.csv', row.names = FALSE)



