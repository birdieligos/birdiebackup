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


english <- read.csv('/Users/birdieligos/Downloads/Sector 2 Text List (Original) - English_S2.csv', stringsAsFactors = FALSE)

spanish <- read.csv('/Users/birdieligos/Downloads/Sector 2 Text List (Original) - Spanish_S2 (1).csv', stringsAsFactors = FALSE)



scrub <- read.csv('/Users/birdieligos/Downloads/122230-English-Invite - Tag DNC.csv', stringsAsFactors = FALSE)

scrub <- scrub %>%
  mutate(phone = str_replace(phone, "^1", ""))

english <- english %>% 
  filter(!phone %in% scrub$phone)

spanish <- spanish %>% 
  filter(!phone %in% scrub$phone)


write.csv(english, file = '/Users/birdieligos/Documents/Reports/englishsector241825.csv', row.names = FALSE)

write.csv(spanish, file = '/Users/birdieligos/Documents/Reports/spanishsector2041825.csv', row.names = FALSE)


