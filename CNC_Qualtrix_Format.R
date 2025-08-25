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


r1 <- read.csv('/Users/birdieligos/Downloads/CNC_Environment_Call (1) - CNC_Environment_Call1 (1).csv', stringsAsFactors = FALSE)

r3 <- read.csv('/Users/birdieligos/Downloads/CNC_Environment_Call (3) - CNC_Environment_Call3.csv', stringsAsFactors = FALSE)

r2 <- read.csv('/Users/birdieligos/Downloads/CNC_Environment_Call (2) - CNC_Environment_Call2.csv', stringsAsFactors = FALSE)

r1 <- r1 %>% select(SOS, PhoneNumber, First_Name, Last_Name, Zip, Email)
r2 <- r2 %>% select(SOS, PhoneNumber, First_Name, Last_Name, Zip, Email)
r3 <- r3 %>% select(SOS, PhoneNumber, First_Name, Last_Name, Zip, Email)

cnc_qualtrix <- rbind(r1, r2, r3)

write.csv(cnc_qualtrix, file = '/Users/birdieligos/Documents/Reports/CNC_Qualtrix_list1_022225.csv', row.names = FALSE)


