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
data <- data %>% filter(Date != as.Date("2025-03-06"))
                        

############################################################## every event column add
data <- select(data, -ATTENDED_BEFORE, -EVENT_GROUP_R)

############################################################## new data add 
new <- read.csv('/Users/birdieligos/Downloads/UBERAPRIL0415 - Screening.csv', stringsAsFactors = FALSE, check.names = FALSE)

# combine fname lname
new$Name <- tools::toTitleCase(paste(new$Uber_FName, new$Uber_LName))
new$Uber_FName <- NULL
new$Uber_LName <- NULL

# clean
new$Uber_Start_Date <- as.Date(new$Uber_Start_Date, format = "%Y-%m-%d")

# find missing columns
setdiff(names(data), names(new))

# add missing cols
new$STATUS <- NA_character_
new$make <- NA_character_
new$model <- NA_character_
new$year <- NA_character_
new$Country_Region_Name <- 'US'
new$app_rating <- NA_character_
new$Minutes <- NA_integer_
new$courier_rating <- NA_character_
new$Device <- NA_character_
new$Race_Ethnicity <- NA_character_
new$Gender <- NA_character_
new$Reason_For_Exit <- NA_character_
new$CM_End_Date <- NA_character_
new$CM_Start_Date <- as.Date(NA)
new$End_Date <- as.Date(NA)

# event columns
new$RSVP <- 1
new$Event <- 'OH Q1 2025'
new$Date <- as.Date("2025-03-06")

########### metrics pull 

new <- select(new, uuid)

history <- filter(data, Attended == 1)

history <- history %>%
  mutate(EVENT_GROUP_R = case_when(
    str_detect(Event, "Applicants") ~ "Applicants",
    str_detect(Event, "Interviews") ~ "Interviews",
    str_detect(Event, "Voters") ~ "Voters",
    str_detect(Event, "Town Hall") ~ "Town Hall",
    str_detect(Event, "OH") ~ "Office Hours",
    str_detect(Event, "Crew Members") ~ "Crew Members",
    TRUE ~ NA_character_
  ))

history <- filter(history, EVENT_GROUP_R == 'Office Hours')

history <- select(history, uuid, Date)

# Trim whitespace
new$uuid <- trimws(new$uuid)
history$uuid <- trimws(history$uuid)

# Count office hours per uuid
oh_counts <- as.data.frame(table(history$uuid))
colnames(oh_counts) <- c("uuid", "OH_HISTORY")

# Merge counts into new
new <- merge(new, oh_counts, by = "uuid", all.x = TRUE)

# Replace NAs with 0
new$OH_HISTORY[is.na(new$OH_HISTORY)] <- 0

write.csv(new, file = '/Users/birdieligos/Documents/Reports/OH_UUID_HISTORY041525.csv', row.names = FALSE)
########################

print(colnames(data))

test <- rbind(new, data)


############################################################## go on
data <- test

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
print(sum(data$ATTENDED_BEFORE))

test <- filter(data, Date == '2025-03-06')

print(sum(test$ATTENDED_BEFORE))
       # change to big query upload df name 
cmmerge_table <- data

cmmerge_table <- cmmerge_table[!duplicated(cmmerge_table), ]

##### fix 
fix <- read.csv('/Users/birdieligos/Documents/Reports/fixlist.csv', stringsAsFactors = FALSE)

# Trim UUIDs just in case
data$uuid <- trimws(data$uuid)
fix$uuid <- trimws(fix$uuid)

fix_dedup <- fix %>%
  distinct(uuid, .keep_all = TRUE)

data <- data %>%
  left_join(fix_dedup, by = "uuid") %>%
  mutate(
    ATTENDED_BEFORE = if_else(
      Date == as.Date("2025-03-06"),
      coalesce(OH_HISTORY, 0L),
      ATTENDED_BEFORE
    )
  ) %>%
  select(-OH_HISTORY)

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


