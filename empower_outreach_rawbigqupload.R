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
library(readxl)
unloadNamespace("plyr")

#################################### 
# CONVOSO
#################################### OVERWRITES

# change path for daily call report drop 
upload <- read.csv('/Users/birdieligos/Downloads/log_107910_903mnhbxxtuy9j4z88co1vdqlb85jhuj_2025-07-15_06_11_31.csv', stringsAsFactors = FALSE)

# format
colnames(upload) <- gsub("\\.", "_", colnames(upload))

upload <- upload %>%
  mutate(
    ts   = ymd_hms(Time_Stamp__PST_),
    Date = as.Date(ts),
    Time = format(ts, "%H:%M:%S")
  ) %>%
  select(-Time_Stamp__PST_, -ts)

upload <- upload %>%
  mutate(
    ts   = ymd_hms(Final_Reached_At),
    Final_Reach_Date = as.Date(ts),
    Final_Reach_Time = format(ts, "%H:%M:%S")
  ) %>%
  select(-Final_Reached_At, -ts)

upload <- upload %>%
  mutate(
    ts   = ymd_hms(Created_At__Time_),
    Created_Lead_Date = as.Date(ts),
    Created_Lead_Time = format(ts, "%H:%M:%S")
  ) %>%
  select(-Created_At__Time_, -ts)

upload <- select(upload, Date, Lead_ID, Number_Dialed, Status_Name, Talk_Time, Cost, Outbound_Called_Count,
                 Campaign_Name,	List_Name, Final_Reach_Date, Final_Reach_Time, Created_Lead_Date,
                 Created_Lead_Time, PROGRAM_RECOGNITION, SCE_CUSTOMER, WORKSHOP_INTEREST)

upload$PLATFORM <- 'Convoso'

# ### calendly temp conversion grab 
# 
# reg <- read.csv('/Users/birdieligos/Downloads/events-export 87.csv', stringsAsFactors = FALSE)
# 
# colnames(reg) <- gsub("\\.", "_", colnames(reg))
# 
# colnames(reg) <- gsub("_+", "_", colnames(reg))
# 
# reg$Text_Reminder_Number <- gsub("\\D+", "", reg$Text_Reminder_Number)
# 
# reg$Text_Reminder_Number <- sub("^1", "", reg$Text_Reminder_Number)


# ### get converted workshops 
# upload_fix <- select(upload, -WORKSHOP_INTEREST)
# 
# reg_fix <- mutate(reg, Number_Dialed = as.numeric(Text_Reminder_Number))
# 
# reg_fix <- reg_fix %>%
#   mutate(Date = as.Date(Event_Created_Date_Time, format = "%Y-%m-%d %I:%M %p"))
# 
# reg_fix <- select(reg_fix, Number_Dialed, Date)
# 
# reg_fix$WORKSHOP_INTEREST <- 'Yes to workshop'
# 
# reg_fix <- reg_fix %>% 
#   distinct()
# 
# ### convoso merge 
# 
# convoso_upload <- left_join(upload_fix, reg_fix)
# 
# # clean 
# convoso_upload <- mutate(convoso_upload, Lead_ID = as.character(Lead_ID))
# convoso_upload <- mutate(convoso_upload, Number_Dialed = as.character(Number_Dialed))
# 
# cols <- c("PROGRAM_RECOGNITION", "SCE_CUSTOMER", "WORKSHOP_INTEREST")
# 
# for (col in cols) {
#   if (is.logical(convoso_upload[[col]])) {
#     convoso_upload[[col]] <- as.character(convoso_upload[[col]])
#   }
# }


################# OVERWRITES write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "CONVOSO_REPORTRAW")

# Append data to the BigQuery table
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")


#################################### 
#################################### 
#PDI
#################################### 
#################################### OVERWRITES

# change path for daily PDI report drop 
upload <- read.csv('/Users/birdieligos/Downloads/empower_060225canvassfinal.csv', stringsAsFactors = FALSE)

# format
colnames(upload) <- gsub("\\.", "_", colnames(upload))

colnames(upload) <- gsub("_+", "_", colnames(upload))

upload <- upload %>%
  rename(
    IMPRESSIONS = DOORSKNOCKED,
    REACH = CONTACTS,
    REBATE_INTEREST = ARE_YOU_INTERESTED_IN_APPLYING_FOR_A_REBATE_RIY
  )

upload$AUDIENCE <- "80% or Below AMI"
upload$GEOGRAPHY <- "SECTOR 2"
upload$PLATFORM <- "PDI"
upload$LANGUAGE <- "Bilingual"

print(colnames(upload))

upload <- upload %>%
  rename(PROGRAM_RECOGNITION = ARE_YOU_FAMILIAR_WITH_EMPOWER_GATEWAY_BAY)

upload <- select(upload, DATE, IMPRESSIONS, REACH, PROGRAM_RECOGNITION,
                 REBATE_INTEREST, AUDIENCE, GEOGRAPHY, LANGUAGE, PLATFORM)

############################## OVERWRITES write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "PDI_REPORTRAW")

# Append data to the BigQuery table
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")
########################################
########################################
# TEXTING DATA
######################################## 
######################################## APPENDS

# Vector of only the highlighted file paths (replace with the exact filenames you selected)
highlighted_files <- c(
  '/Users/birdieligos/Downloads/extended_report_123906 (1).xlsx',
  '/Users/birdieligos/Downloads/extended_report_123986.xlsx',
  '/Users/birdieligos/Downloads/extended_report_124134.xlsx',
  '/Users/birdieligos/Downloads/extended_report_124135.xlsx',
  '/Users/birdieligos/Downloads/extended_report_124145.xlsx'
)

process_file <- function(path) {
  text_ext <- read_excel(path, sheet = "Dialogs") %>%
    select(phone, ts)
  
  list_name <- read_excel(path, sheet = "Totals", range = "B2", col_names = FALSE)[[1,1]]
  
  text_ext %>%
    mutate(
      LIST_NAME    = list_name,
      LANGUAGE     = case_when(
        str_detect(LIST_NAME, regex("ENGLISH", ignore_case = TRUE)) ~ "English",
        str_detect(LIST_NAME, regex("SPANISH", ignore_case = TRUE)) ~ "Spanish",
        TRUE                                                         ~ "English"
      ),
      GEOGRAPHY     = "Sector 1",
      AUDIENCE      = "80% or Below AMI",
      PLATFORM      = "Teletown Hall",
      CAMPAIGN_NAME = "emPOWER Gateway",
      DATE          = as.Date(ymd_hms(ts)),
      TIME          = format(ymd_hms(ts), "%H:%M:%S")  # time only
    ) %>%
    select(-ts) %>%
    rename(NUMBER_DIALED = phone)
}

combined <- purrr::map_dfr(highlighted_files, process_file)

text_ext <- combined %>%
  select(-LIST_NAME)


############################## APPENDS write big query table 
library(bigrquery)

# Set up the BigQuery table reference
bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "TTH_REPORTRAW")

# Append data to the BigQuery table
bq_table_upload(bq_table, text_ext, write_disposition = "WRITE_APPEND")


########################################
########################################
# TINY URL TEXTING LINK CLICK DATA
######################################## (OVERWRITES)
########################################

# bring in tiny url metrics in order to get text conversions (link clicks)
url <- read.csv('/Users/birdieligos/Downloads/tinyurl_20250604_070522.csv', stringsAsFactors = FALSE)

url$Date <- as.Date(ymd_hms(url$timestamp))

url <- url %>%
  filter(str_detect(tinyurl, regex("empower", ignore_case = TRUE)))

url <- filter(url, bot == 'false')

url$PLATFORM <- 'Tiny URL'

############################## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "TINYURL_REPORTRAW")

bq_table_upload(bq_table, url, write_disposition = "WRITE_TRUNCATE")



########################################
########################################
# CALENDLY EVENT ATTENDANCE DATA
######################################## (OVERWRITES)
########################################

# bring in tiny url metrics in order to get text conversions (link clicks)
url <- read.csv('/Users/birdieligos/Downloads/events-export 89.csv', stringsAsFactors = FALSE)

url <- url %>%
  mutate(
    NUMBER_DIALED = Response.1 %>%
      str_replace_all("\\D", "") %>%   
      str_remove("^1")                
  )

url <- url %>%
  mutate(
    LANGUAGE = coalesce(
      if_else(str_detect(Question.2, regex("language preference", ignore_case = TRUE)), Response.2, NA_character_),
      if_else(str_detect(Question.3, regex("language preference", ignore_case = TRUE)), Response.3, NA_character_),
      if_else(str_detect(Question.4, regex("language preference", ignore_case = TRUE)), Response.4, NA_character_),
      if_else(str_detect(Question.5, regex("language preference", ignore_case = TRUE)), Response.5, NA_character_)
    ),
    AUDIENCE = coalesce(
      if_else(str_detect(Question.2, regex("program interest|best describes you", ignore_case = TRUE)), Response.2, NA_character_),
      if_else(str_detect(Question.3, regex("program interest|best describes you", ignore_case = TRUE)), Response.3, NA_character_),
      if_else(str_detect(Question.4, regex("program interest|best describes you", ignore_case = TRUE)), Response.4, NA_character_),
      if_else(str_detect(Question.5, regex("program interest|best describes you", ignore_case = TRUE)), Response.5, NA_character_)
    ),
    CONVERSION_TYPE = coalesce(
      if_else(str_detect(Question.2, regex("hear about us", ignore_case = TRUE)), Response.2, NA_character_),
      if_else(str_detect(Question.3, regex("hear about us", ignore_case = TRUE)), Response.3, NA_character_),
      if_else(str_detect(Question.4, regex("hear about us", ignore_case = TRUE)), Response.4, NA_character_),
      if_else(str_detect(Question.5, regex("hear about us", ignore_case = TRUE)), Response.5, NA_character_)
    )
  ) %>%
  select(-starts_with("Question."), -starts_with("Response."))


url <- url %>%
  rename(
    Geography       = Location,
  ) %>%
  rename_with(~ str_replace_all(., "[.]+", "_")) %>%
  rename_with(toupper)


url <- url %>%
  mutate(
    raw_loc = if_else(
      str_detect(GEOGRAPHY, "\\|"),
      str_extract(GEOGRAPHY, "(?<=\\| ).*$"),
      GEOGRAPHY
    ),
    GEOGRAPHY = if_else(
      str_detect(raw_loc, ","),
      str_trim(word(raw_loc, 2, sep = ",")),
      str_trim(word(raw_loc, 1, 2))
    )
  ) %>%
  select(-raw_loc)


url <- url %>%
  mutate(
    DATE = as.Date(
      parse_date_time(EVENT_CREATED_DATE_TIME, "Y-m-d I:M p")
    )
  )

url <- url %>%
  rename(CONVERSIONS = MARKED_AS_NO_SHOW) %>%
  mutate(
    CONVERSIONS = case_when(
      CONVERSIONS == "No" & CANCELED == FALSE ~ 2L,
      CONVERSIONS == "No"                      ~ 1L,
      TRUE                                     ~ 0L
    )
  )

url <- url %>%
  rename(AD_NAME = EVENT_TYPE_NAME)

url <- url %>%
  mutate(
    START_DT = parse_date_time(START_DATE_TIME, "Y-m-d I:M p"),
    AD_NAME  = paste0(format(START_DT, "%Y-%m-%d"), " ", AD_NAME)
  ) %>%
  select(-START_DT)

url <- select(url, AD_NAME, GEOGRAPHY,  CONVERSIONS, LANGUAGE, AUDIENCE, NUMBER_DIALED,
              CONVERSION_TYPE, DATE)

url$PLATFORM <- 'Calendly'

# clean 
############################## OVERWRITES write big query table 
library(bigrquery)

bq_table <- bq_table(project = "slstrategy", dataset = "EMPOWER_2025", table = "CALENDLY_REPORTRAW")

bq_table_upload(bq_table, url, write_disposition = "WRITE_TRUNCATE")



