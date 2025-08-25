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
library(googlesheets4)


# Load the 'oes' data from BigQuery
calendly <- bq_table_download("slstrategy.Uber.calendly_hours")

hours <- select(calendly, Start_Date, Event_Type_Name)

hours <- unique(hours)

feedback <- read_sheet("https://docs.google.com/spreadsheets/d/1tG4X4eTVX0yD_TLTuGUvhBKxAzvFJpuAmFBV6BvJVcI/edit?usp=sharing")

# First, do a date match join
joined <- feedback %>%
  left_join(hours, by = c("Date" = "Start_Date")) %>%
  # Then filter to keep only rows where feedback$Type exists inside hours$Event_Type_Name
  filter(str_detect(Event_Type_Name, fixed(Type, ignore_case = TRUE)))

joined <- select(joined, -...15)

joined$City <- sapply(joined$City, function(x) if (is.null(x)) NA_character_ else as.character(x))
joined$Email <- sapply(joined$City, function(x) if (is.null(x)) NA_character_ else as.character(x))
joined$Phone <- sapply(joined$City, function(x) if (is.null(x)) NA_character_ else as.character(x))
names(joined)[names(joined) == "Crew Member"] <- "Crew_Member"
joined$Cohort <- as.character(joined$Cohort)
joined$Date <- as.Date(joined$Date)

write.csv(joined, file = '/Users/birdieligos/Documents/Reports/feedback_event_merge_042525.csv', row.names = FALSE)

#################################################################
# Create BigQuery table
project_id <- "slstrategy"
dataset_id <- "UBER_2025"
table_id <- "UBER_FEEDBACK"

# Create a table reference
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists
if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

# Define the schema based on `joined`
schema <- list(
  bq_field("Date", "DATETIME"),
  bq_field("Calendar", "STRING"),
  bq_field("Rides_Eats", "STRING"),
  bq_field("Type", "STRING"),
  bq_field("Tag", "STRING"),
  bq_field("Subtag", "STRING"),
  bq_field("Feedback", "STRING"),
  bq_field("Cohort", "BOOL"),
  bq_field("State", "STRING"),
  bq_field("City", "STRING"),  # Flattened before upload
  bq_field("Commenter", "STRING"),
  bq_field("Email", "STRING"), # Flattened before upload
  bq_field("Phone", "STRING"),  # Flattened before upload
  bq_field("Crew_Member", "STRING"),
  bq_field("Event_Type_Name", "STRING")
)


# Upload the data
tryCatch({
  bq_table_create(table_ref, fields = schema)
  Sys.sleep(10)
  bq_table_upload(table_ref, values = joined)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred:", conditionMessage(e), "\n")
})

