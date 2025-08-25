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


############################################### CLOCKIFY 

# Load the 'oes' data from BigQuery
clockify <- bq_table_download('slstrategy.Budget_System.Clockify_Detail_Report', bigint = "integer64")

# temp clockify 
clockify <- read.csv('/Users/birdieligos/Downloads/Clockify_Time_Report_Detailed_01_01_2025-03_31_2025 (2).csv', stringsAsFactors = FALSE)


################### PREP CLOCKIFY FOR BUDGET MERGE

names(clockify) <- names(clockify) %>%
  gsub(pattern = "\\.+", replacement = "_", .) %>%
  gsub(pattern = "[._]+$", replacement = "", .) %>%
  toupper()

clockify <- clockify %>% select(-DCBA_TAGS)

names(clockify) <- gsub("_+", "_", names(clockify))

clockify <- select(clockify, GROUP, CLIENT, START_DATE, COST_AMOUNT_USD, TAGS)


# GROUP
clockify <- clockify %>%
  group_by(GROUP, TAGS, CLIENT, START_DATE) %>%
  summarise(COST_AMOUNT = sum(COST_AMOUNT_USD, na.rm = TRUE)) %>%
  filter(COST_AMOUNT != 0)

# REMOVE DOUBLED TAGGED OR NO ATGGED ITEMS
clockify <- clockify %>%
  filter(!grepl(",", TAGS) & TAGS != "")

# RENAME TAGS TO WORKSTREWAM
clockify <- clockify %>%
  rename(WORKSTREAM = TAGS)


# UPPERCASE
clockify <- clockify %>%
  mutate(WORKSTREAM = toupper(WORKSTREAM))

clockify <- clockify %>%
  mutate(CLIENT = toupper(CLIENT))


# RENAME 
clockify <- clockify %>%
  rename(SUB_ACCOUNT = GROUP)

clockify <- clockify %>%
  rename(DATE = START_DATE)

clockify <- clockify %>%
  rename(ACTUAL = COST_AMOUNT)


################################################################# QUICKBOOKS EXPENSES

# temp quickbooks
qb <- read.csv('/Users/birdieligos/Downloads/Street Level Strategy LLC_Transaction List by Date - birdie (2).csv', stringsAsFactors = FALSE)

# CLEAN

qb <- qb[-c(1:2), ]
names(qb) <- qb[1, ]
qb <- qb[-1, ]
rownames(qb) <- NULL

names(qb) <- names(qb) %>%
  gsub("[ /]+", "_", .) %>%
  gsub("_+", "_", .) %>%
  gsub("[._]+$", "", .) %>%
  toupper()

qb <- select(qb, DATE, AMOUNT, CUSTOMER, ITEM_SPLIT_ACCOUNT_FULL_NAME)

# SPLIT ACCOPUNT INTO MAIN AND SUB ACCOUNT
qb <- qb %>%
  separate(ITEM_SPLIT_ACCOUNT_FULL_NAME, into = c("MAIN_ACCOUNT", "SUB_ACCOUNT"), sep = ":", fill = "right", extra = "merge") %>%
  mutate(across(c(MAIN_ACCOUNT, SUB_ACCOUNT), ~ trimws(.)))

# SPLIT CUSTOMER INTO CLIENT AND WORKSTREAM
qb <- qb %>%
  separate(CUSTOMER, into = c("CLIENT", "WORKSTREAM"), sep = ":", fill = "right", extra = "merge") %>%
  mutate(across(c(CLIENT, WORKSTREAM), ~ trimws(.)))


# FORMAT
qb <- qb %>%
  mutate(MAIN_ACCOUNT = toupper(MAIN_ACCOUNT))

qb <- qb %>%
  mutate(SUB_ACCOUNT = ifelse(MAIN_ACCOUNT == "CREW MEMBER STIPEND", "Crew Member Stipend", SUB_ACCOUNT))

qb <- qb %>%
  mutate(WORKSTREAM = toupper(WORKSTREAM))

qb <- qb %>%
  mutate(CLIENT = toupper(CLIENT))

qb <- qb %>%
  rename(ACTUAL = AMOUNT)

# REMOVE ROWS WHERE CLIENT IS BLANK
qb <- qb %>%
  filter(CLIENT != "")

qb <- select(qb, -MAIN_ACCOUNT)


# COMBINE COSTS

cost <- rbind(qb, clockify)

# UPDATE VALUES TO MATCH BUDGET BEFORE MERGE 

cost <- cost %>%
  mutate(CLIENT = ifelse(grepl("DCBA", CLIENT), "LA - DCBA", CLIENT))

workstream_map <- tribble(
  ~RAW_WORKSTREAM,                           ~STANDARDIZED_WORKSTREAM,
  "EMPOWER GEN ACCOUNT MGMT",               "GENERAL PROGRAM MANAGEMENT",
  "UBER CREW GEN ACCOUNT MGMT",             "GENERAL PROGRAM MANAGEMENT",
  "UBER CAN GEN ACCOUNT MGMT",              "GENERAL PROGRAM MANAGEMENT",
  "CA VOLS GEN ACCOUNT MGMT",               "GENERAL PROGRAM MANAGEMENT",
  "CA VOLS - GEN. ACCOUNT MANAGEMENT",      "GENERAL PROGRAM MANAGEMENT",
  "DCBA - GEN. ACCOUNT MANAGEMENT",         "GENERAL PROGRAM MANAGEMENT",
  "UBER - GEN. ACCOUNT MANAGEMENT",         "GENERAL PROGRAM MANAGEMENT",
  "OES - GEN. ACCOUNT MANAGEMENT",          "GENERAL PROGRAM MANAGEMENT",
  "EMPOWER - GEN. ACCOUNT MANAGEMENT",      "GENERAL PROGRAM MANAGEMENT",
  "UBER CREW MEMBER MGMT",                  "CREW MEMBER MGMT",
  "UBER CAN CREW MEMBER MGMT",              "CREW MEMBER MGMT",
  "UBER CREW MEMBER MANAGEMENT",            "CREW MEMBER MGMT",
  "UBER CREW PRODUCT SESSIONS",             "PRODUCT SESSIONS",
  "UBER CAN PRODUCT SESSIONS",              "PRODUCT SESSIONS",
  "UBER PRODUCT SESSIONS",                  "PRODUCT SESSIONS",
  "UBER CREW TOWNHALLS/SOCIALS",            "TOWNHALLS SOCIALS",
  "UBER TOWN HALL/SOCIAL",                  "TOWNHALLS SOCIALS",
  "UBER CREW OFFICE HOURS",                 "OFFICE HOURS",
  "UBER OFFICE HOURS",                      "OFFICE HOURS",
  "UBER CREW OHFU SESSIONS",                "OHFU SESSIONS",
  "UBER OHFU SESSIONS",                     "OHFU SESSIONS",
  "UBER CREW WEBSITE",                      "WEBSITE",
  "UBER WEBSITE",                           "WEBSITE",
  "UBER APP CYCLE",                         "APP CYCLE",
  "EMPOWER EVENTS",                         "EVENTS",
  "EMPOWER - EVENTS",                       "EVENTS",
  "EMPOWER MARKETING",                      "MARKETING",
  "EMPOWER - MARKETING",                    "MARKETING",
  "EMPOWER WORKSHOPS",                      "WORKSHOPS",
  "EMPOWER - WORKSHOPS",                    "WORKSHOPS",
  "EMPOWER CBO PROGRAM",                    "CBO PROGRAM",
  "EMPOWER - CBO PROGRAM",                  "CBO PROGRAM",
  "EMPOWER - CASE MANAGEMENT",              "CASE MANAGEMENT",
  "DCBA - CONSUMER PROTECTIONS",            "CONSUMER PROTECTIONS",
  "DCBA - RENT STABILZATION",              "RENT STABILIZATION TENANT PROTECTION",
  "DCBA - RIGHT TO COUNCIL",                "RIGHT TO COUNCIL STAY HOUSED",
  "DCBA - OIA",                              "OIA",
  "DCBA - VIDEO OUTREACH PROGRAM",          "VIDEO OUTREACH",
  "DCBA - VITA 2.0",                         "VITA 2.0",
  "VITA",                                    "VITA",
  "RIGHT TO COUNCIL",                        "RIGHT TO COUNCIL STAY HOUSED",
  "RENT STABILIZATION",                      "RENT STABILIZATION TENANT PROTECTION",
  "CONSUMER PROTECTIONS",                    "CONSUMER PROTECTIONS",
  "OIA",                                     "OIA",
  "EMPOWER - CANVASS",                       "CANVASS",
  "DCBA",                                    "GENERAL PROGRAM MANAGEMENT",
  "CA VOLS JAN CCAD - LB",                  "JAN CCAD",
  "CA VOLS FEB CCAD - FRESNO",                  "FEB CCAD",
  "CA VOLS APR CCAD - LA",                  "APR CCAD",
  "CA VOLS MAR CCAD - SB",                  "MAR CCAD",
  "CA VOLS JUN CCAD - BAY AREA",             "JUN CCAD",
  "EMPOWER MARKETING",                       "MARKETING",
  "Digital/Social Media Consultant",           "Digital/Social Media - Consultant"
  
  # Add more as needed...
)


cost <- cost %>%
  left_join(workstream_map, by = c("WORKSTREAM" = "RAW_WORKSTREAM")) %>%
  mutate(WORKSTREAM = coalesce(STANDARDIZED_WORKSTREAM, WORKSTREAM)) %>%
  select(-STANDARDIZED_WORKSTREAM)

# PREPARE FOR MERGE 
cost <- cost %>%
  mutate(across(everything(), ~ str_trim(.)))

cost <- cost %>%
  mutate(ACTUAL = gsub(",", "", ACTUAL))

cost <- cost %>%
  mutate(ACTUAL = as.numeric(ACTUAL))

cost <- cost %>%
  mutate(WORKSTREAM = ifelse(WORKSTREAM == "" | is.na(WORKSTREAM), "GENERAL PROGRAM MANAGEMENT", WORKSTREAM))

cost <- cost %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Partner", "Partner - Clockify Expense", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Data", "Data & Analytics Staff Labor", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Recruiting", "Recruiting Staff", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Digital/Social Media Consultant", "Digital/Social Media - Consultant", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Digital/Social Media Consultant", "Digital/Social Media - Ad Placement", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Digital/Social Media Consultant", "Digital/Social Media - Translation", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Digital/Social Media Consultant", "Digital/Social Media - Graphic Design", SUB_ACCOUNT))

cost <- cost %>%
  mutate(SUB_ACCOUNT = ifelse(SUB_ACCOUNT == "Digital/Social Media Consultant", "Digital/Social Media - Video Production", SUB_ACCOUNT))




