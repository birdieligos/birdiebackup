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


data <- bq_table_download("slstrategy.Uber.Earner_Data")

backup <- data
data <- backup

############################################################### test 
test <- filter(data, Date == '2025-02-26')
test <- test[!duplicated(test$uuid), ]

length(unique(test$uuid))
############################################################## every event column add
data <- select(data, -ATTENDED_BEFORE)

data <- data %>%
  mutate(EVENT_GROUP_R = case_when(
    str_detect(Event, "Applicants") ~ "Applicants",
    str_detect(Event, "Interviews") ~ "Interviews",
    str_detect(Event, "Voters") ~ "Voters",
    str_detect(Event, "Town Hall") ~ "Town Hall",
    str_detect(Event, "OH") ~ "Office Hours",
    str_detect(Event, "Crew Members") ~ "Crew Members",
    TRUE ~ NA_character_
  ))

# add attended before column
data <- data %>%
  arrange(uuid, EVENT_GROUP_R, Date) %>%
  group_by(uuid, EVENT_GROUP_R) %>%
  mutate(
    ATTENDED_BEFORE = if_else(
      Attended == 0,
      0L,
      as.integer(cumsum(lag(Attended, default = 0)) > 0)
    )
  ) %>%
  ungroup()

# clean up
data <- data %>%
  mutate(
    ATTENDED_BEFORE = if_else(
      uuid %in% c("uuid", "uclick_id", "fbclid", "#VALUE", "/N", "\\N"),
      0L,
      ATTENDED_BEFORE
    )
  )

# change to big query upload df name 
cmmerge_table <- data

cmmerge_table <- cmmerge_table[!duplicated(cmmerge_table), ]
################################################################ WRITE BIG Q TABLE
# Load required library
library(bigrquery)

# Get today's date in the desired format (MMDDYY)
today_date <- format(Sys.Date(), "%m%d%y")
# Update the file name with today's date
file_path <- paste0("/Users/birdieligos/Documents/Reports/UBER_Earner_Table_Backup", today_date, ".csv")
# Save the CSV file with the updated file name
write.csv(cmmerge_table, file = file_path, row.names = FALSE)

# Set up BigQuery project, dataset, and table information
project_id <- "slstrategy"
dataset_id <- "Uber"
table_id <- "Earner_Data"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists, and delete if it does
if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

# Define the schema for the table
schema <- list(
  bq_field("Attended", "INT64"),
  bq_field("Email", "STRING"),
  bq_field("Phone", "STRING"),
  bq_field("Country_Region_Name", "STRING"),
  bq_field("RSVP", "INT64"),  
  bq_field("Minutes", "FLOAT64"),
  bq_field("Name", "STRING"),
  bq_field("uuid", "STRING"),
  bq_field("city_name", "STRING"),
  bq_field("state", "STRING"),
  bq_field("flow", "STRING"),
  bq_field("loyalty_tier", "STRING"),
  bq_field("make", "STRING"),
  bq_field("model", "STRING"),
  bq_field("year", "STRING"),
  bq_field("app_rating", "STRING"),
  bq_field("courier_rating", "STRING"),
  bq_field("lifetime_completed_trips", "STRING"),
  bq_field("percent_eats", "STRING"),
  bq_field("percent_rides", "STRING"),
  bq_field("Event", "STRING"),
  bq_field("Date", "DATE"),
  bq_field("Uber_Start_Date", "DATE"),
  bq_field("End_Date", "DATE"),
  bq_field("Device", "STRING"),
  bq_field("Race_Ethnicity", "STRING"),
  bq_field("Gender", "STRING"),
  bq_field("Reason_For_Exit", "STRING"),
  bq_field("STATUS", "STRING"),
  bq_field("CM_Start_Date", "DATE"),
  bq_field("CM_End_Date", "STRING"),
  bq_field("EVENT_GROUP_R", "STRING"),
  bq_field("ATTENDED_BEFORE", "INT64")
)

# Attempt to create the BigQuery table and upload the data
tryCatch({
  # Create the table with the defined schema
  bq_table_create(table_ref, fields = schema)
  
  # Upload the data to the newly created table
  bq_table_upload(table_ref, values = cmmerge_table)
  
  # Success message
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  # Enhanced error handling with specific message and logging error details
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})


