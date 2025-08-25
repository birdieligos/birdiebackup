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


upload <- read.csv('/Users/birdieligos/Downloads/OES_SANTABARBARA_ENGLISH_040225.csv', stringsAsFactors = FALSE)

colnames(upload) <- gsub("\\.", "_", colnames(upload))

upload <- upload[, c("Client_ID", "Campaign_ID", "Household_ID", "HUBID", "Pass__", 
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
                     "Which_county_should_we_sign_you_up_for_"), drop = FALSE]

upload$Client_ID <- as.integer(upload$Client_ID)
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


############################## write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "OES_2024", table = "OES_2024")

# Append data to the BigQuery table
bq_table_upload(bq_table, upload, write_disposition = "WRITE_APPEND")

