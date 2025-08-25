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


# when reading a file, this makes sure that strings remain characters instead of becoming factors
options(stringsAsFactors = FALSE)

# Load the 'oes' data from BigQuery
qb <- bq_table_download('slstrategy.Budget_System.QB_Expenses', bigint = "integer64")


qb_filtered <- qb %>% filter(TxnDate >= as.Date("2025-01-01"))



