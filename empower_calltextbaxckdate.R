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

####################################  
# IMPACTIVE ALL TIME CALL DATA 
####################################  OVERWRITES
impactive <- read.csv('/Users/birdieligos/Downloads/2025_05_24_campaign_9982_EMPOWERALLTIME052325.csv', stringsAsFactors = FALSE)

# format
colnames(upload) <- gsub("\\.", "_", colnames(upload))

print(colnames(impactive))

impactive <- impactive %>%
  rename(
    REBATE_INTEREST    = custom_field_rebate_interest,
    PROGRAM_INTEREST   = custom_field_program_interest,
    WORKSHOP_INTEREST  = custom_field_attend_workshop
  )

impactive <- impactive %>%
  select(
    -custom_field_brand_recognition,
    -custom_field_are_you_an_sce_customer
  )


impactive <- impactive %>%
  mutate(
    custom_field_call_outcome = recode(
      custom_field_call_outcome,
      "Busy"                   = "System Busy",
      "Answering Machine"      = "Answering Machine Detected",
      "Wrong number"           = "Wrong Number",
      "Not home"               = "Not Available/Call Back",
      "No answer"              = "No Answer",
      "No language in common"  = "Lanuguage Barrier",
      "Dropped"                = "Pre-Routing Drop",
      "Refused"                = "Refused/Hung Up",
      "Failed"                 = "System Busy",
      "Completed"              = "Human",
      .default = custom_field_call_outcome
    )
  )

impactive <- impactive %>%
  mutate(
    ts   = ymd_hms(stopped_at),
    Date = as.Date(ts),
    Time = format(ts, "%H:%M:%S")
  ) %>%
  select(-stopped_at, -ts)

impactive <- impactive %>%
  mutate(phone = sub("^1", "", phone))


impactive <- impactive %>%
  rename(
    Status_Name = custom_field_call_outcome,
    Talk_Time = duration,
    Lead_ID = contact_id,
    Number_Dialed = phone, 
    List_Name = activity_title
  )



impactive <- impactive %>%
  group_by(Lead_ID) %>%
  mutate(Outbound_Called_Count = dense_rank(Date)) %>%
  ungroup()

impactive <- select(impactive, Date, Lead_ID, Number_Dialed, Status_Name, Talk_Time,
                    List_Name, REBATE_INTEREST, PROGRAM_INTEREST, WORKSHOP_INTEREST, Outbound_Called_Count)

impactive$PLATFORM <- 'Impactive.io'

############## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "IMPACTIVE_REPORTRAW")

bq_table_upload(bq_table, impactive, write_disposition = "WRITE_TRUNCATE")


############################################################# 
#### HUBDIALER ALL TIME CALL DATA
############################################################# OVERWRITES

hub <- bq_table_download("slstrategy.EmPower.Empower_Calls_2024", bigint = "integer64")

hub <- hub %>%
  rename(
    Lead_ID  = PDI_ID,
    Number_Dialed = Phone_Number,
    Status_Name = Status,
    Talk_Time = Call_Duration,
    SCE_CUSTOMER = Are_you_a_SoCal_Edison_Customer_,
    WORKSHOP_INTEREST = Are_you_interested_in_regstering_for_an_upcoming_in_person_workshop_
  )


hub <- hub %>%
  mutate(
    List_Name = paste0(
      "EMPOWER_SECTOR1_",
      ifelse(Ethnicity == "SS", "SPANISH", "ENGLISH"),
      "_2024"
    )
  )


hub <- hub %>%
  mutate(
    Status_Name = recode(
      Status_Name,
      "Busy"                   = "System Busy",
      "Voice Mail"             = "Answering Machine Detected",
      "Miss"                   = "System Busy",
      "Not Home"               = "Not Available/Call Back",
      "Disconnected"           = "System Busy",
      "Do Not Call"            = "Do NOT Call",
      "Rings and Silence"     = "System Busy",
      "Not In Service"      = "System Busy",
      .default = Status_Name
    )
  )

hub$PLATFORM <- 'Hubdialer'

hub <- hub %>%
  group_by(Lead_ID) %>%
  mutate(Outbound_Called_Count = dense_rank(Date)) %>%
  ungroup()

hub <- hub %>%
  mutate(
    WORKSHOP_INTEREST = ifelse(
      WORKSHOP_INTEREST,
      "Yes",
      "No/Refuse"
    )
  )

hub <- select(hub, Date, Lead_ID, Number_Dialed, Status_Name, Talk_Time,
                    List_Name, WORKSHOP_INTEREST, Outbound_Called_Count)


############################## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "HUBDIALER_REPORTRAW")

bq_table_upload(bq_table, hub, write_disposition = "WRITE_TRUNCATE")


