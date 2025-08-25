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


program <- read.csv('/Users/birdieligos/Downloads/report1733960888236.csv', stringsAsFactors = FALSE)

docs <- read.csv('/Users/birdieligos/Downloads/report1733961100332.csv', stringsAsFactors = FALSE)

ws <- read.csv('/Users/birdieligos/Downloads/Campaign History Export 101024-2024-12-11-16-11-45.csv', stringsAsFactors = FALSE)


# clean column names 
clean_column_names <- function(df) {
  colnames(df) <- colnames(df) %>%
    str_replace_all("\\.+", ".") %>%
    str_replace_all("\\.", "_") %>%
    str_to_title()
  return(df)
}

program <- clean_column_names(program)
docs <- clean_column_names(docs)
ws <- clean_column_names(ws)


# docs rework 

print(unique(docs$Empower_document_empower_document_name))

print(colnames(ws))

print(unique(docs$Document_status))

# 
docs$Empower_gateway_empower_gateway_name <- str_trim(docs$Empower_gateway_empower_gateway_name)
program$Empower_gateway_empower_gateway_name <- str_trim(program$Empower_gateway_empower_gateway_name)
colnames(ws)[colnames(ws) == "Empower_gateway"] <- "Empower_gateway_empower_gateway_name"
ws$Empower_gateway_empower_gateway_name <- str_trim(ws$Empower_gateway_empower_gateway_name)


docpro <- left_join(docs, program)

merge <- left_join(docpro, ws)
colnames(merge)

# format

merge <- merge %>%
  mutate(
    Date_ready_to_apply = mdy(Date_ready_to_apply),
    Date_needs_documents = mdy(Date_needs_documents),
    Date_in_progress = mdy(Date_in_progress),
    Date_dropped = mdy(Date_dropped),
    Date_did_not_qualify = mdy(Date_did_not_qualify),
    Date_denied_benefits = mdy(Date_denied_benefits),
    Date_applied = mdy(Date_applied),
    Empower_program_last_modified_date = mdy(Empower_program_last_modified_date),
    Empower_program_created_date = mdy(Empower_program_created_date),
    Empower_gateway_created_date = mdy(Empower_gateway_created_date),
    Vehicle_purchase_date = mdy(Vehicle_purchase_date),
    Empower_document_created_date = mdy(Empower_document_created_date),
    Empower_document_last_modified_date = mdy(Empower_document_last_modified_date)
  )

merge <- merge %>%
  select(-Household_summary_form)

# Clean Sce_rebate_level column
merge <- merge %>%
  mutate(
    Sce_rebate_level = as.integer(ifelse(Sce_rebate_level == "Unknown", NA, gsub("\\$", "", Sce_rebate_level)))
  )

merge <- merge %>%
  mutate(
    Already_purchased_vehicle = case_when(
      Already_purchased_vehicle == 1 ~ "Already Purchased",
      Already_purchased_vehicle == 0 ~ "Looking to Purchase",
      TRUE ~ as.character(Already_purchased_vehicle) # Retain any other values as-is
    )
  )


print(unique(merge$Application_status))


##### WRITE TO BIG QUERY 

# Define BigQuery location
project_id <- "slstrategy"
dataset_id <- "EmPower"
table_id <- "empower_SF_V1"

# Schema for the BigQuery table
schema <- list(
  bq_field("Empower_document_created_date", "DATE"),
  bq_field("Empower_document_last_modified_date", "DATE"),
  bq_field("Document_status", "STRING"),
  bq_field("Brought_to_event", "INT64"),
  bq_field("Status", "STRING"),
  bq_field("Empower_gateway_empower_gateway_name", "STRING"),
  bq_field("Empower_document_empower_document_name", "STRING"),
  bq_field("Sce_rebate_level", "STRING"),
  bq_field("Empower_program_last_modified_date", "DATE"),
  bq_field("Empower_program_created_date", "DATE"),
  bq_field("Not_qualify_reason", "STRING"),
  bq_field("Expected_benefit_amount", "FLOAT64"),
  bq_field("Date_received_benefits", "DATE"),
  bq_field("Date_ready_to_apply", "DATE"),
  bq_field("Date_needs_documents", "DATE"),
  bq_field("Date_in_progress", "DATE"),
  bq_field("Date_dropped", "DATE"),
  bq_field("Date_did_not_qualify", "DATE"),
  bq_field("Date_denied_benefits", "DATE"),
  bq_field("Date_applied", "DATE"),
  bq_field("Customer_status", "STRING"),
  bq_field("Benefit_amount", "FLOAT64"),
  bq_field("Application_status", "STRING"),
  bq_field("Empower_gateway_created_date", "DATE"),
  bq_field("Verify_vehicle_qualifications", "INT64"),
  bq_field("Vehicle_purchase_price", "FLOAT64"),
  bq_field("Vehicle_purchase_date", "DATE"),
  bq_field("Vehicle_new_or_used", "STRING"),
  bq_field("Total_benefits_received", "FLOAT64"),
  bq_field("Sce_verified", "INT64"),
  bq_field("Rent_or_own", "STRING"),
  bq_field("Qualifies_for_irs_used_ev_rebate", "INT64"),
  bq_field("Number_of_enrolled_programs", "INT64"),
  bq_field("Number_of_applied_programs", "INT64"),
  bq_field("Income_verified", "INT64"),
  bq_field("Household_size", "INT64"),
  bq_field("Household_income", "INT64"),
  bq_field("Heard_from", "STRING"),
  bq_field("Customer_interested_in_self_help_financi", "STRING"),
  bq_field("Ami_percentage", "FLOAT64"),
  bq_field("Already_purchased_vehicle", "STRING"),
  bq_field("Empower_program_empower_program_name", "STRING"),
  bq_field("Program_detail_program_details_name", "STRING"),
  bq_field("Member_Status", "STRING"),
  bq_field("Campaign_name", "STRING"),
  bq_field("Campaign_status", "STRING")
)

# Function to create and upload the table
upload_to_bigquery <- function(project_id, dataset_id, table_id, schema, data) {
  table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)
  
  # Delete table if it exists
  if (bq_table_exists(table_ref)) {
    bq_table_delete(table_ref)
  }
  
  # Create the table with the schema
  bq_table_create(table_ref, fields = schema)
  
  # Upload data to the table
  bq_table_upload(table_ref, values = merge)
  
  cat("Table created and data uploaded successfully!\n")
}

# Call the function
tryCatch({
  upload_to_bigquery(project_id, dataset_id, table_id, schema, merge)
}, error = function(e) {
  cat("An error occurred:", conditionMessage(e), "\n")
})


print(count(unique(program$)))


