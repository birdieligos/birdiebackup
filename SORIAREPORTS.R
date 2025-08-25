library(bigrquery)
library(curl)
library(dplyr)
library(lubridate)
library(httpuv)
library(base)
library(googledrive)
library(stringr)
library(tidyr)
library(tibble)


af1 <- read.csv('/Users/birdieligos/Downloads/AllFlags_1of4 (3).csv', stringsAsFactors = FALSE)
af2 <- read.csv('/Users/birdieligos/Downloads/AllFlags_2of4 (3).csv', stringsAsFactors = FALSE)
af3 <- read.csv('/Users/birdieligos/Downloads/AllFlags_3of4 (3).csv', stringsAsFactors = FALSE)
af4 <- read.csv('/Users/birdieligos/Downloads/AllFlags_4of4 (3).csv', stringsAsFactors = FALSE)

af <- rbind(af1, af2, af3, af4)

print(colnames(af))

ss <- af[af$RESPONSEDESCRIPTION == "Strong Support", ]

ss <- select(ss, PDIID)

ss <- ss %>% distinct(PDIID)

write.csv(ss, file = '/Users/birdieligos/Documents/Reports/SoriaStrongSupporters063025.csv', row.names = FALSE)
