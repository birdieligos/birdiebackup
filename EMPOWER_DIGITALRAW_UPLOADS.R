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

############################## 
#META REPORT
############################## OVERWRITESX
upload_sls <- read.csv('/Users/birdieligos/Downloads/SLS-empower-meta-digital-071525.csv', stringsAsFactors = FALSE)
upload_them <- read.csv('/Users/birdieligos/Downloads/THEMATIC-emPOWER-digital-meta-071525.csv', stringsAsFactors = FALSE)
upload <- rbind(upload_them, upload_sls)


colnames(upload) <- gsub("\\.", "_", colnames(upload))
colnames(upload)<-toupper(
  gsub("_+$","",
       gsub("_+","_",
            gsub("\\.","_",colnames(upload))
       )
  )
)

upload <- upload %>%
  mutate(DATE = as.Date(DAY)) %>%
  select(-DAY)


upload <- upload %>%
  rename(CPR = COST_PER_1_000_ACCOUNTS_CENTER_ACCOUNTS_REACHED)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_25 = VIDEO_PLAYS_AT_25)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_50 = VIDEO_PLAYS_AT_50)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_75 = VIDEO_PLAYS_AT_75)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_100 = VIDEO_PLAYS_AT_100)

upload <- upload %>%
  rename(CPM = CPM_COST_PER_1_000_IMPRESSIONS)

upload <- upload %>%
  rename(CPC_DESTINATION = CPC_COST_PER_LINK_CLICK)

upload <- upload %>%
  rename(CTR_DESTINATION = CTR_LINK_CLICK_THROUGH_RATE)

upload <- upload %>%
  rename(AD_GROUP_NAME = AD_SET_NAME)


upload <- upload %>%
  filter(!is.na(CAMPAIGN_NAME) & CAMPAIGN_NAME != "")
#######  OVERWRITES  write big query table 
library(bigrquery)
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "METAREPORT_RAW")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")

###################################################### 
#TIK TOK REPORT
###################################################### OVERWRITES
upload <- read.csv('/Users/birdieligos/Downloads/TikTokAds_TiktokDashReport_DSPolitical - Agency_2025-01-01-2025-12-31 (2).csv', stringsAsFactors = FALSE)

colnames(upload) <- gsub("\\.", "_", colnames(upload))
colnames(upload)<-toupper(
  gsub("_+$","",
       gsub("_+","_",
            gsub("\\.","_",colnames(upload))
       )
  )
)

upload <- upload %>%
  mutate(DATE = as.Date(BY_DAY)) %>%
  select(-BY_DAY)

upload <- upload %>%
  rename(CPR = COST_PER_1_000_PEOPLE_REACHED)

upload <- upload %>%
  rename(AMOUNT_SPENT_USD = COST)

upload <- upload %>%
  rename(LINK_CLICKS = CLICKS_DESTINATION)

upload <- upload %>%
  rename(VIDEO_AVERAGE_PLAY_TIME = AVERAGE_PLAY_TIME_PER_VIDEO_VIEW)

upload <- upload %>%
  rename(PLATFORM = PLACEMENTS_TYPES)

####### OVERWRITES write big query table 
library(bigrquery)
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "TIKTOKREPORT_RAW")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")

######################################################
#GOOGLE (YOUTUBE) REPORT
###################################################### OVERWRITES
upload <- read.csv(
  '/Users/birdieligos/Downloads/empower_dashreport (13).csv',
  skip = 2,
  stringsAsFactors = FALSE
)

cols_to_convert <- c("Video.played.to.25.", "Video.played.to.50.", "Video.played.to.75.", "Video.played.to.100.")

upload <- upload %>%
  mutate(Impr. = as.numeric(gsub(",", "", Impr.))) %>%
  mutate(across(all_of(cols_to_convert), ~ {
    pct <- as.numeric(gsub("%", "", .))
    viewers <- round(pct / 100 * Impr.)
    ifelse(is.na(pct), NA, viewers)
  }))

# column name clean
colnames(upload) <- gsub("\\.", "_", colnames(upload))
colnames(upload)<-toupper(
  gsub("_+$","",
       gsub("_+","_",
            gsub("\\.","_",colnames(upload))
       )
  )
)

# date formatting
upload <- upload %>%
  mutate(DATE = as.Date(DAY, format = "%Y-%m-%d")) %>%
  select(-DAY)

# change colnames
upload <- upload %>%
  rename(IMPRESSIONS = IMPR)

upload <- upload %>%
  rename(PLATFORM = NETWORK_WITH_SEARCH_PARTNERS)

upload <- upload %>%
  mutate(IMPRESSIONS = as.integer(gsub(",", "", IMPRESSIONS)))

# cant find reach
# upload <- upload %>%
#   rename(CPR = COST_PER_1_000_PEOPLE_REACHED)

upload <- upload %>%
  rename(AMOUNT_SPENT_USD = COST)

upload <- upload %>%
  rename(LINK_CLICKS = CLICKS)


upload <- upload %>%
  rename(CPM = AVG_CPM)

upload <- upload %>%
  select(where(~ {
    vals <- as.character(.)
    !all(is.na(vals) | vals == " --")
  }))

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_25 = VIDEO_PLAYED_TO_25)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_50 = VIDEO_PLAYED_TO_50)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_75 = VIDEO_PLAYED_TO_75)

upload <- upload %>%
  rename(VIDEO_VIEWS_AT_100 = VIDEO_PLAYED_TO_100)

####### OVERWRITES write big query table 
library(bigrquery)
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "YOUTUBEREPORT_RAW")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")

