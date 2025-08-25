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

spanish <- read.csv('/Users/birdieligos/Downloads/AllFlags (7).csv', stringsAsFactors = FALSE)
spanish <- select(spanish, PDIID)
spanish$ES <- 'ES'

all <- read.csv('/Users/birdieligos/Downloads/empower_90201_texting_110824.csv')

join <- left_join(all, spanish, by = c("V1_PDIID" = "PDIID"))


# Create the `english` dataframe where ES is NA
english <- filter(join, is.na(ES))

# Create the `spanish` dataframe where ES is "ES"
spanish <- filter(join, ES == "ES")


write.csv(english, '/Users/birdieligos/Documents/Reports/90201English.csv', row.names = FALSE)
write.csv(spanish, '/Users/birdieligos/Documents/Reports/90201Spanish.csv', row.names = FALSE)

