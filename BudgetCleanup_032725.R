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

# uber

uber <- read.csv('/Users/birdieligos/Downloads/DB - UBER BUDGET 2025.xlsx - Master Budget by Workstream (3).csv', stringsAsFactors = FALSE)

uber$MAIN_ACCOUNT <- ifelse(
  grepl("^[A-Z0-9 /&]+$", uber[[1]]) &
    !grepl("\\b(CPA|VP|CFO)\\b", uber[[1]], ignore.case = FALSE) &
    !is.na(uber[[1]]),
  uber[[1]],
  NA
)

uber <- fill(uber, MAIN_ACCOUNT)

names(uber)[names(uber) == "CHART.OF.ACCOUNTS"] <- "SUB_ACCOUNT"

uber <- uber[, c("MAIN_ACCOUNT", setdiff(names(uber), "MAIN_ACCOUNT"))]

uber <- uber[!(uber$MAIN_ACCOUNT == uber$SUB_ACCOUNT & grepl("^[A-Z0-9 /&]+$", uber$SUB_ACCOUNT)), ]


# remove extra rows for now
uber <- uber[, -c(25:28)]

# clean columns
uber[, 3:24][uber[, 3:24] == ""] <- '0.00'

uber[, 3:24] <- lapply(uber[, 3:24], function(x) gsub("[$,]", "", x))


######## pviot 
col_groups <- names(uber)[3:24]
workstream_base <- gsub("\\.\\d+$", "", col_groups)
workstream_base <- gsub("[^A-Za-z0-9]", "_", workstream_base)
workstream_base <- gsub("_+", "_", workstream_base)
suffixes <- rep(c("BUDGET", "ACTUAL"), times = length(col_groups) / 2)
names(uber)[3:24] <- paste0(workstream_base, ".", suffixes)

uber <- uber[-1, ]

uber <- pivot_longer(
  uber,
  cols = 3:24,
  names_to = c("WORKSTREAM", ".value"),
  names_sep = "\\."
)

uber$WORKSTREAM <- gsub("_", " ", uber$WORKSTREAM)

uber$BUDGET <- as.numeric(uber$BUDGET)

uber$ACTUAL <- as.numeric(uber$ACTUAL)

uber <- uber[uber$SUB_ACCOUNT != "Subtotal", ]

uber$MAIN_ACCOUNT[grepl("Partner -", uber$SUB_ACCOUNT, ignore.case = TRUE)] <- "CONSULTANT - PARTNER"

budget <- uber
################################################################
# Load required library
library(bigrquery)

# Get today's date in the desired format (MMDDYY)
today_date <- format(Sys.Date(), "%m%d%y")
# Update the file name with today's date
file_path <- paste0("/Users/birdieligos/Documents/2025_BUDGETS/UBER_INTIAL.csv", today_date, ".csv")
# Save the CSV file with the updated file name
write.csv(budget, file = file_path, row.names = FALSE)

# Set up BigQuery project, dataset, and table information
project_id <- "slstrategy"
dataset_id <- "2025_BUDGETS"
table_id <- "UBER_INITIAL_2025"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists, and delete if it does
if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

# Define the schema for the table
schema <- list(
  bq_field("MAIN_ACCOUNT", "STRING"),
  bq_field("SUB_ACCOUNT", "STRING"),
  bq_field("WORKSTREAM", "STRING"),
  bq_field("BUDGET", "FLOAT64"),
  bq_field("ACTUAL", "FLOAT64")
)

# Attempt to create the BigQuery table and upload the data
tryCatch({
  # Create the table with the defined schema
  bq_table_create(table_ref, fields = schema)
  
  # Upload the data to the newly created table
  bq_table_upload(table_ref, values = budget)
  
  # Success message
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  # Enhanced error handling with specific message and logging error details
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})



################################################################### empower 

# empower

empower <- read.csv('/Users/birdieligos/Downloads/DB - EMPOWER BUDGET 2025.xlsx - Budget by Workstream.csv', stringsAsFactors = FALSE)

empower$MAIN_ACCOUNT <- ifelse(
  grepl("^[A-Z0-9 /&]+$", empower[[1]]) &
    !grepl("\\b(CPA|VP|CFO)\\b", empower[[1]], ignore.case = FALSE) &
    !is.na(empower[[1]]),
  empower[[1]],
  NA
)

empower <- fill(empower, MAIN_ACCOUNT)

names(empower)[1] <- "SUB_ACCOUNT"

empower <- empower[, c("MAIN_ACCOUNT", setdiff(names(empower), "MAIN_ACCOUNT"))]

empower <- empower[!(empower$MAIN_ACCOUNT == empower$SUB_ACCOUNT & grepl("^[A-Z0-9 /&]+$", empower$SUB_ACCOUNT)), ]

empower[, 3:16][empower[, 3:16] == ""] <- '0.00'

empower[, 3:16] <- lapply(empower[, 3:16], function(x) gsub("[$,]", "", x))

col_groups <- names(empower)[3:16]
col_groups <- toupper(col_groups)

workstream_base <- gsub("\\.\\d+$", "", col_groups)
workstream_base <- gsub("[^A-Z0-9]", "_", workstream_base)
workstream_base <- gsub("_+", "_", workstream_base)

suffixes <- rep(c("BUDGET", "ACTUAL"), times = length(col_groups) / 2)
names(empower)[3:16] <- paste0(workstream_base, ".", suffixes)

empower <- empower[-1, ]

empower <- pivot_longer(
  empower,
  cols = 3:16,
  names_to = c("WORKSTREAM", ".value"),
  names_sep = "\\."
)

empower$WORKSTREAM <- gsub("_", " ", empower$WORKSTREAM)

empower$BUDGET <- as.numeric(empower$BUDGET)

empower$ACTUAL <- as.numeric(empower$ACTUAL)

empower <- empower[empower$SUB_ACCOUNT != "Subtotal", ]

empower$MAIN_ACCOUNT[grepl("Partner -", empower$SUB_ACCOUNT, ignore.case = TRUE)] <- "CONSULTANT - PARTNER"

budget <- empower

# Load required library
library(bigrquery)

today_date <- format(Sys.Date(), "%m%d%y")
file_path <- paste0("/Users/birdieligos/Documents/2025_BUDGETS/EMPOWER_INITIAL.csv", today_date, ".csv")
write.csv(budget, file = file_path, row.names = FALSE)

project_id <- "slstrategy"
dataset_id <- "2025_BUDGETS"
table_id <- "EMPOWER_INITIAL_2025"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

schema <- list(
  bq_field("MAIN_ACCOUNT", "STRING"),
  bq_field("SUB_ACCOUNT", "STRING"),
  bq_field("WORKSTREAM", "STRING"),
  bq_field("BUDGET", "FLOAT64"),
  bq_field("ACTUAL", "FLOAT64")
)

tryCatch({
  bq_table_create(table_ref, fields = schema)
  bq_table_upload(table_ref, values = budget)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})



################################################################### dcba_one 

# dcba_one

dcba_one <- read.csv('/Users/birdieligos/Downloads/DB - DCBA BUDGET 1.xlsx - CA-23-006 Budget vs Actual (1).csv', stringsAsFactors = FALSE)


unique_vals <- unique(dcba_one[[1]])
unique_vals <- unique_vals[grepl("^[A-Z0-9 /&]+$", unique_vals)]

dcba_one$MAIN_ACCOUNT <- ifelse(
  grepl("^[A-Z0-9 /&]+$", dcba_one[[1]]) &
    !grepl("\\b(CPA|VP|CFO)\\b", dcba_one[[1]], ignore.case = FALSE) &
    !is.na(dcba_one[[1]]),
  dcba_one[[1]],
  NA
)

dcba_one <- fill(dcba_one, MAIN_ACCOUNT)

names(dcba_one)[1] <- "SUB_ACCOUNT"

dcba_one <- dcba_one[, c("MAIN_ACCOUNT", setdiff(names(dcba_one), "MAIN_ACCOUNT"))]

dcba_one <- dcba_one[!(dcba_one$MAIN_ACCOUNT == dcba_one$SUB_ACCOUNT & grepl("^[A-Z0-9 /&]+$", dcba_one$SUB_ACCOUNT)), ]

dcba_one[, 3:34][dcba_one[, 3:34] == ""] <- '0.00'

dcba_one[, 3:34] <- lapply(dcba_one[, 3:34], function(x) gsub("[$,]", "", x))

col_groups <- names(dcba_one)[3:34]
workstream_base <- gsub("\\.1$", "", col_groups)  # remove trailing .1 from Actual cols
workstream_base <- toupper(workstream_base)       # force uppercase
workstream_base <- gsub("[^A-Z0-9]", "_", workstream_base)
workstream_base <- gsub("_+", "_", workstream_base)

# infer suffixes based on alternating Budget / Actual order
suffixes <- rep(c("BUDGET", "ACTUAL"), times = length(col_groups) / 2)
names(dcba_one)[3:34] <- paste0(workstream_base, ".", suffixes)

dcba_one <- dcba_one[-1, ]

dcba_one <- pivot_longer(
  dcba_one,
  cols = 3:34,
  names_to = c("WORKSTREAM", ".value"),
  names_sep = "\\."
)

dcba_one$WORKSTREAM <- gsub("_", " ", dcba_one$WORKSTREAM)

dcba_one$BUDGET <- as.numeric(dcba_one$BUDGET)

dcba_one$ACTUAL <- as.numeric(dcba_one$ACTUAL)

dcba_one <- dcba_one[dcba_one$SUB_ACCOUNT != "Subtotal", ]

dcba_one$MAIN_ACCOUNT[grepl("Partner -", dcba_one$SUB_ACCOUNT, ignore.case = TRUE)] <- "CONSULTANT - PARTNER"

budget <- dcba_one

# Load required library
library(bigrquery)

today_date <- format(Sys.Date(), "%m%d%y")
file_path <- paste0("/Users/birdieligos/Documents/2025_BUDGETS/DCBA_ONE_INITIAL.csv", today_date, ".csv")
write.csv(budget, file = file_path, row.names = FALSE)

project_id <- "slstrategy"
dataset_id <- "2025_BUDGETS"
table_id <- "DCBA_ONE_INITIAL_2025"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

schema <- list(
  bq_field("MAIN_ACCOUNT", "STRING"),
  bq_field("SUB_ACCOUNT", "STRING"),
  bq_field("WORKSTREAM", "STRING"),
  bq_field("BUDGET", "FLOAT64"),
  bq_field("ACTUAL", "FLOAT64")
)

tryCatch({
  bq_table_create(table_ref, fields = schema)
  bq_table_upload(table_ref, values = budget)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})



################################################## DCBA TWO
# DCBA_TWO

DCBA_TWO <- read.csv('/Users/birdieligos/Downloads/DB - DCBA BUDGET 2.xlsx - CA-24-014 Budget vs Actual (1).csv', stringsAsFactors = FALSE)

unique_vals <- unique(DCBA_TWO[[1]])
unique_vals <- unique_vals[grepl("^[A-Z0-9 /&]+$", unique_vals)]

DCBA_TWO$MAIN_ACCOUNT <- ifelse(
  grepl("^[A-Z0-9 /&]+$", DCBA_TWO[[1]]) &
    !grepl("\\b(CPA|VP|CFO)\\b", DCBA_TWO[[1]], ignore.case = FALSE) &
    !is.na(DCBA_TWO[[1]]),
  DCBA_TWO[[1]],
  NA
)

DCBA_TWO <- fill(DCBA_TWO, MAIN_ACCOUNT)

names(DCBA_TWO)[1] <- "SUB_ACCOUNT"

DCBA_TWO <- DCBA_TWO[, c("MAIN_ACCOUNT", setdiff(names(DCBA_TWO), "MAIN_ACCOUNT"))]

DCBA_TWO <- DCBA_TWO[!(DCBA_TWO$MAIN_ACCOUNT == DCBA_TWO$SUB_ACCOUNT & grepl("^[A-Z0-9 /&]+$", DCBA_TWO$SUB_ACCOUNT)), ]

DCBA_TWO[, 3:10][DCBA_TWO[, 3:10] == ""] <- '0.00'

DCBA_TWO[, 3:10] <- lapply(DCBA_TWO[, 3:10], function(x) gsub("[$,]", "", x))

col_groups <- names(DCBA_TWO)[3:10]
workstream_base <- gsub("\\.1$", "", col_groups)
workstream_base <- toupper(workstream_base)
workstream_base <- gsub("[^A-Z0-9]", "_", workstream_base)
workstream_base <- gsub("_+", "_", workstream_base)

suffixes <- rep(c("BUDGET", "ACTUAL"), times = length(col_groups) / 2)
names(DCBA_TWO)[3:10] <- paste0(workstream_base, ".", suffixes)

DCBA_TWO <- DCBA_TWO[-1, ]

DCBA_TWO <- pivot_longer(
  DCBA_TWO,
  cols = 3:10,
  names_to = c("WORKSTREAM", ".value"),
  names_sep = "\\."
)

DCBA_TWO$WORKSTREAM <- gsub("_", " ", DCBA_TWO$WORKSTREAM)

DCBA_TWO$BUDGET <- as.numeric(DCBA_TWO$BUDGET)

DCBA_TWO$ACTUAL <- as.numeric(DCBA_TWO$ACTUAL)

DCBA_TWO <- DCBA_TWO[DCBA_TWO$SUB_ACCOUNT != "Subtotal", ]

DCBA_TWO$MAIN_ACCOUNT[grepl("Partner -", DCBA_TWO$SUB_ACCOUNT, ignore.case = TRUE)] <- "CONSULTANT - PARTNER"

budget <- DCBA_TWO

################################ write to big query
library(bigrquery)

today_date <- format(Sys.Date(), "%m%d%y")
file_path <- paste0("/Users/birdieligos/Documents/2025_BUDGETS/DCBA_TWO_INITIAL.csv", today_date, ".csv")
write.csv(budget, file = file_path, row.names = FALSE)

project_id <- "slstrategy"
dataset_id <- "2025_BUDGETS"
table_id <- "DCBA_TWO_INITIAL_2025"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

schema <- list(
  bq_field("MAIN_ACCOUNT", "STRING"),
  bq_field("SUB_ACCOUNT", "STRING"),
  bq_field("WORKSTREAM", "STRING"),
  bq_field("BUDGET", "FLOAT64"),
  bq_field("ACTUAL", "FLOAT64")
)

tryCatch({
  bq_table_create(table_ref, fields = schema)
  bq_table_upload(table_ref, values = budget)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})



