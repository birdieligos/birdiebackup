# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


# Read file
upload <- read.csv('/Users/birdieligos/Downloads/AD 31 propensity demos - data export.csv', stringsAsFactors = FALSE)

colnames(upload) <- gsub("\\.", "_", colnames(upload))

names(upload) <- gsub("_+", "_", names(upload))

names(upload) <- sub("_$", "_prct", names(upload))

upload <- upload %>%
  mutate(across(ends_with("_prct"),
                ~ as.numeric(gsub("%", "", .))))

upload <- upload %>%
  select(1:2, matches("^X(?!.*_H)", perl = TRUE))

print(str(head))


