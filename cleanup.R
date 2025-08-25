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
library(tibble)
unloadNamespace("plyr")
library(ggplot2)


list <- read.csv('/Users/birdieligos/Downloads/\'25-\'26\ Prospective\ Families\ -\ Leads\ +\ Prospects.csv', stringsAsFactors = FALSE)

list$Phone.. <- gsub("[^0-9]", "", gsub("\\s+", "", list$Phone..))

list$Phone.. <- sub("^1", "", list$Phone..)

write.csv(list, file = '/Users/birdieligos/Documents/Reports/MilitaryCallList1.csv', row.names = FALSE)
