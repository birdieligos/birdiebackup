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


# Read the CSV file and clean the column names
upload <- read.csv('/Users/birdieligos/Downloads/2025_04_04_campaign_9981_OESPHONE3325.csv', stringsAsFactors = FALSE)
colnames(upload) <- gsub("\\.", "_", colnames(upload))
colnames(upload) <- str_to_title(colnames(upload))


# Create a named vector: names are current column names, values are desired names
rename_map <- c(
  "Phone" = "Phone_Number",
  "First_name" = "First_Name",
  "Last_name" = "Last_Name",
  "Status" = "Status",
  "Were_tips_provided_" = "Were_Tips_Provided_",
  "Can_we_sign_you_up_for_emergency_service_notifications_" = "Can_we_sign_you_up_for_Emergency_Service_Notifications_",
  "What_are_some_of_your_conerns_" = "What_are_some_of_your_conerns_",
  "On_a_scale_of_1_5__how_prepared_do_you_feel_for_an_emergency_" = "On_a_scale_of_1_5__how_prepared_do_you_feel_for_an_emergency_",
  "If_1_3__what_are_the_top_reasons_that_keep_you_from_being_more_prepared_" = "If_1_3__what_are_the_top_reasons_that_keep_you_from_being_more_prepared_",
  "Have_you_seen__heard__or_received_any_information_about_how_to_prepare_for_or_recover_from_a_natural_disaster_" = "Have_you_seen__heard__or_received_any_information_about_how_to_prepare_for_or_recover_from_a_natural_disaster_",
  "Where_did_you_see_or_hear_this_information_" = "Where_did_you_see_or_hear_this_information_",
  "Which_of_the_following_things_would_help_you_feel_more_prepared_for_a_natural_disaster_" = "Which_of_the_following_things_would_help_you_feel_more_prepared_for_a_natural_disaster_",
  "Account_name" = "Account_Name",
  "Campaign_id" = "Campaign_ID"
)

# Apply renaming
colnames(upload) <- ifelse(
  colnames(upload) %in% names(rename_map),
  rename_map[colnames(upload)],
  colnames(upload)
)

# 1. Split Activity_published_at into Date and Time
upload$Date <- as.Date(upload$Activity_published_at)
upload$Time <- format(as.POSIXct(upload$Activity_published_at, tz = "UTC"), "%H:%M:%S")
upload$Activity_published_at <- NULL

# 2. Rename Contact_id to Call_ID
names(upload)[names(upload) == "Contact_id"] <- "Call_ID"

# 3. Combine User_first_name and User_last_name into Agent
upload$Agent <- paste(upload$User_first_name, upload$User_last_name)
upload$User_first_name <- NULL
upload$User_last_name <- NULL

# 4. Rename User_email to Agent_Email_ID
names(upload)[names(upload) == "User_email"] <- "Agent_Email_ID"

# 5. Create County column based on Activity_Title
upload$County <- ifelse(grepl("ORANGE", upload$Activity_title, ignore.case = TRUE), "Orange",
                        ifelse(grepl("STANISLAUS", upload$Activity_title, ignore.case = TRUE), "Stanislaus",
                               ifelse(grepl("SANTA_BARBARA", upload$Activity_title, ignore.case = TRUE), "Santa Barbara",
                                      ifelse(grepl("ALPINE", upload$Activity_title, ignore.case = TRUE), "Alpine", NA))))

cols_to_add <- c(
  "Client_ID", "Household_ID", "HUBID", "Pass__", "Address_1", "Address_2",
  "Zip", "Age", "Ethnicity", "Gender", "Party", "Agent_Session_Number",
  "Call_Duration", "Patch_Number", "Patch_Status", "Patch_Duration", "Notes",
  "What_was_the_most_important_part_of_the_information_you_saw_", 
  "Account_Name", "Which_county_should_we_sign_you_up_for_"
)

for (col in cols_to_add) {
  if (!col %in% colnames(upload)) {
    upload[[col]] <- NA
  }
}

# Define the list of expected columns
expected_columns <- c(
  "Client_ID", "Campaign_ID", "Household_ID", "HUBID", "Pass__", 
  "Phone_Number", "First_Name", "Last_Name", "Address_1", "Address_2", 
  "Zip", "Age", "County", "Ethnicity", "Gender", "Party", "Status", 
  "Call_ID", "Agent_Session_Number", "Agent", "Agent_Email_ID", "Date", 
  "Time", "Call_Duration", "Patch_Number", "Patch_Status", "Patch_Duration", 
  "Notes", "Can_we_sign_you_up_for_Emergency_Service_Notifications_", 
  "What_are_some_of_your_conerns_", 
  "On_a_scale_of_1_5__how_prepared_do_you_feel_for_an_emergency_", 
  "If_1_3__what_are_the_top_reasons_that_keep_you_from_being_more_prepared_", 
  "Have_you_seen__heard__or_received_any_information_about_how_to_prepare_for_or_recover_from_a_natural_disaster_", 
  "Where_did_you_see_or_hear_this_information_", 
  "What_was_the_most_important_part_of_the_information_you_saw_", 
  "Which_of_the_following_things_would_help_you_feel_more_prepared_for_a_natural_disaster_", 
  "Account_Name", "Were_Tips_Provided_", 
  "Which_county_should_we_sign_you_up_for_"
)

# Check for missing columns
missing_columns <- setdiff(expected_columns, colnames(upload))

# Output the missing columns
missing_columns

# changwe uplaod 
upload <- upload[, expected_columns, drop = FALSE]

##upload$Client_ID <- as.integer(upload$Client_ID)
upload$Campaign_ID <- as.integer(upload$Campaign_ID)
upload$Household_ID <- as.integer(upload$Household_ID)
upload$HUBID <- as.integer(upload$HUBID)
upload$Pass__ <- as.integer(upload$Pass__)
upload$Phone_Number <- as.integer(upload$Phone_Number)
upload$First_Name <- as.character(upload$First_Name)
upload$Last_Name <- as.character(upload$Last_Name)
upload$Address_1 <- as.character(upload$Address_1)
upload$Address_2 <- as.character(upload$Address_2)
upload$Zip <- as.integer(upload$Zip)
upload$Age <- as.integer(upload$Age)
upload$County <- as.character(upload$County)
upload$Ethnicity <- as.character(upload$Ethnicity)
upload$Gender <- as.character(upload$Gender)
upload$Party <- as.character(upload$Party)
upload$Status <- as.character(upload$Status)
upload$Call_ID <- as.integer(upload$Call_ID)
upload$Agent_Session_Number <- as.integer(upload$Agent_Session_Number)
upload$Agent <- as.character(upload$Agent)
upload$Agent_Email_ID <- as.character(upload$Agent_Email_ID)
upload$Date <- if (!inherits(upload$Date, "Date")) as.Date(upload$Date, format = "%m/%d/%Y") else upload$Date
upload$Time <- hms::as_hms(upload$Time)
upload$Call_Duration <- as.integer(upload$Call_Duration)
upload$Patch_Number <- as.character(upload$Patch_Number)
upload$Patch_Status <- as.character(upload$Patch_Status)
upload$Patch_Duration <- as.character(upload$Patch_Duration)
upload$Notes <- as.character(upload$Notes)
upload$Can_we_sign_you_up_for_Emergency_Service_Notifications_ <- as.character(upload$Can_we_sign_you_up_for_Emergency_Service_Notifications_)
upload$What_are_some_of_your_conerns_ <- as.character(upload$What_are_some_of_your_conerns_)
upload$On_a_scale_of_1_5__how_prepared_do_you_feel_for_an_emergency_ <- as.integer(upload$On_a_scale_of_1_5__how_prepared_do_you_feel_for_an_emergency_)
upload$If_1_3__what_are_the_top_reasons_that_keep_you_from_being_more_prepared_ <- as.character(upload$If_1_3__what_are_the_top_reasons_that_keep_you_from_being_more_prepared_)
upload$Have_you_seen__heard__or_received_any_information_about_how_to_prepare_for_or_recover_from_a_natural_disaster_ <- as.character(upload$Have_you_seen__heard__or_received_any_information_about_how_to_prepare_for_or_recover_from_a_natural_disaster_)
upload$Where_did_you_see_or_hear_this_information_ <- as.character(upload$Where_did_you_see_or_hear_this_information_)
upload$What_was_the_most_important_part_of_the_information_you_saw_ <- as.character(upload$What_was_the_most_important_part_of_the_information_you_saw_)
upload$Which_of_the_following_things_would_help_you_feel_more_prepared_for_a_natural_disaster_ <- as.character(upload$Which_of_the_following_things_would_help_you_feel_more_prepared_for_a_natural_disaster_)
upload$Account_Name <- as.character(upload$Account_Name)
upload$Were_Tips_Provided_ <- as.logical(upload$Were_Tips_Provided_)
upload$Which_county_should_we_sign_you_up_for_ <- as.character(upload$Which_county_should_we_sign_you_up_for_)

############################## fix tips columns 
upload <- select(upload, -Were_Tips_Provided_)

upload <- upload %>%
  mutate(Were_Tips_Provided_ = ifelse(Status %in% c("Refused", "Not home", "Wrong Number", "Completed", "Moved"), "Yes", ""))

print(unique(upload$Status))

################################# fix status
upload <- upload %>%
  mutate(Status = case_when(
    Status == "Answering Machine" ~ "Voice Mail",
    Status == "Refused" ~ "Refused/Hung Up",
    Status == "No answer" ~ "No Answer",
    Status == "Not home" ~ "Not Home",
    Status == "Wrong number" ~ "Wrong Number",
    Status == "Completed" ~ "Human",
    Status == "No language in common" ~ "Language Barrier",
    TRUE ~ Status
  ))

############################### add dial rows 


############################## write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "OES_2024", table = "OES_2024")

# Append data to the BigQuery table
bq_table_upload(bq_table, upload, write_disposition = "WRITE_APPEND")

