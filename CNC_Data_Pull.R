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
unloadNamespace("plyr")


CA27_CALLERS <- bq_table_download("slscampaigns-364520.CNC.CA27_Callers")

write.csv(CA27_CALLERS, file = '/Users/birdieligos/Documents/Reports/CNC_CA27_Callers_012225.csv', row.names = FALSE)

CA22_CANVASS <- bq_table_download("slscampaigns-364520.CNC.CA22_productivity_merged_2024")

write.csv(CA22_CANVASS, file = '/Users/birdieligos/Documents/Reports/CNC_CA22_Canvass_012225.csv', row.names = FALSE)

CA13_CANVASS <- bq_table_download("slscampaigns-364520.CNC.CA13_productivity_merged_2024")

write.csv(CA13_CANVASS, file = '/Users/birdieligos/Documents/Reports/CNC_CA13_Canvass_012225.csv', row.names = FALSE)

CA41_CANVASS <- bq_table_download("slscampaigns-364520.CNC.CA41_productivity_merged_2024")

write.csv(CA41_CANVASS, file = '/Users/birdieligos/Documents/Reports/CNC_CA41_Canvass_012225.csv', row.names = FALSE)







