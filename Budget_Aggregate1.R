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


# 
options(stringsAsFactors = FALSE)

# load intial budgets 

### UBER US
UBER     <- bq_table_download("slstrategy.2025_BUDGETS.UBER_INITIAL_2025", bigint = "integer64")
UBER$CLIENT <- 'UBER - NATIONAL CREW PROGRAM'

### EMPOWER 2025
EMPOWER  <- bq_table_download("slstrategy.2025_BUDGETS.EMPOWER_INITIAL_2025", bigint = "integer64")
EMPOWER$CLIENT <- 'EMPOWER SOUTH LA'

### DCBA ONE
DCBA_ONE <- bq_table_download("slstrategy.2025_BUDGETS.DCBA_ONE_INITIAL_2025", bigint = "integer64")
DCBA_ONE$CLIENT <- 'LA - DCBA'


### DCBA TWO
DCBA_TWO <- bq_table_download("slstrategy.2025_BUDGETS.DCBA_TWO_INITIAL_2025", bigint = "integer64")
DCBA_TWO$CLIENT <- 'LA - DCBA'


# RBIND ALL BUDGETS 

rbind <- rbind(UBER, EMPOWER, DCBA_ONE, DCBA_TWO)
rbind$WORKSTREAM <- gsub("2 0", "2.0", rbind$WORKSTREAM)

# PREPARE FOR MERGE

budget_rbind <- rbind

budget_rbind <- select(budget_rbind, -ACTUAL)

budget_rbind <- budget_rbind %>%
  mutate(WORKSTREAM = ifelse(grepl("GEN ACCOUNT MANAGEMENT", WORKSTREAM), "GENERAL PROGRAM MANAGEMENT", WORKSTREAM))

budget_rbind <- budget_rbind %>%
  mutate(WORKSTREAM = ifelse(grepl("GEN PROGRAM MANAGEMENT", WORKSTREAM), "GENERAL PROGRAM MANAGEMENT", WORKSTREAM))

budget_rbind <- budget_rbind %>%
  mutate(WORKSTREAM = ifelse(WORKSTREAM == "VITA 2024", "VITA 2.0", WORKSTREAM))

budget_rbind <- budget_rbind %>%
  mutate(across(-BUDGET, ~ str_trim(.)))

###################################### PROJECT OUT BUDGET 

dates_2025 <- seq.Date(from = as.Date("2025-01-01"), to = as.Date("2025-12-31"), by = "day")


daily_budget <- budget_rbind %>%
  mutate(BUDGET = as.numeric(BUDGET)) %>%
  mutate(DAY_COUNT = length(dates_2025)) %>%
  rowwise() %>%
  mutate(DATE = list(dates_2025), BUDGET = round(BUDGET / DAY_COUNT, 2)) %>%
  unnest(cols = c(DATE)) %>%
  select(-DAY_COUNT)

###################################################################### ACTUALS
############################################### CLOCKIFY 

# Load the 'oes' data from BigQuery
clockify <- bq_table_download('slstrategy.Budget_System.Clockify_Detail_Report', bigint = "integer64")

# temp clockify 
#clockify <- read.csv('/Users/birdieligos/Downloads/Clockify_Time_Report_Detailed_01_01_2025-03_31_2025 (2).csv', stringsAsFactors = FALSE)

names(clockify) <- gsub("_+", "_", names(clockify))

################### PREP CLOCKIFY FOR BUDGET MERGE

names(clockify) <- names(clockify) %>%
  gsub(pattern = "\\.+", replacement = "_", .) %>%
  gsub(pattern = "[._]+$", replacement = "", .) %>%
  toupper()

clockify <- clockify %>% select(-DCBA_TAGS)

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


################################### BRING IN ACTUALS 

budget_actual <- daily_budget %>%
  full_join(cost, by = c("DATE", "CLIENT", "WORKSTREAM", "SUB_ACCOUNT"), all())

print(unique(budget_actual$WORKSTREAM))

### CHECK UNMATCHES AND REMOVE
na_clients <- budget_actual %>%
  filter(is.na(BUDGET)) %>%
  distinct(CLIENT) %>%
  pull(CLIENT)

print(na_clients)

#budget_actual <- budget_actual %>%
  #filter(!CLIENT %in% na_clients)

# CLEAN 
budget_actual <- budget_actual %>%
  filter(!(BUDGET == 0 & ACTUAL == 0))

budget_actual <- budget_actual %>%
  mutate(
    BUDGET = ifelse(is.na(BUDGET), 0, BUDGET),
    ACTUAL = ifelse(is.na(ACTUAL), 0, ACTUAL)
  )

budget_actual <- budget_actual %>%
  filter(!(BUDGET == 0 & ACTUAL == 0))

###################################################################################### write to big query
library(bigrquery)

today_date <- format(Sys.Date(), "%m%d%y")
file_path <- paste0("/Users/birdieligos/Documents/2025_BUDGETS/DCBA_TWO_INITIAL.csv", today_date, ".csv")
write.csv(budget_actual, file = file_path, row.names = FALSE)

project_id <- "slstrategy"
dataset_id <- "2025_BUDGETS"
table_id <- "2025_TOTAL_BVA"
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

schema <- list(
  bq_field("MAIN_ACCOUNT", "STRING"),
  bq_field("SUB_ACCOUNT", "STRING"),
  bq_field("WORKSTREAM", "STRING"),
  bq_field("BUDGET", "FLOAT64"),
  bq_field("ACTUAL", "FLOAT64"),
  bq_field("CLIENT", "STRING"),
  bq_field("DATE", "DATE")
)

tryCatch({
  bq_table_create(table_ref, fields = schema)
  bq_table_upload(table_ref, values = budget_actual)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred during the table creation or data upload.\n")
  cat("Error details:", conditionMessage(e), "\n")
})









