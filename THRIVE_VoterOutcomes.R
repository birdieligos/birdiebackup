# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


af <- read.csv('/Users/birdieligos/Downloads/AllFlags(1) (12).csv', stringsAsFactors = FALSE)

work <- select(af, PDIID, STATEID, COUNTYASSIGNEDID, PRECINCT)


# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "THRIVE", table = "Registered_Voter_DB")
bq_table_upload(bq_table, work, write_disposition = "WRITE_TRUNCATE")

### call demos 
regc <- read.csv('/Users/birdieligos/Downloads/allregvotgers071725.csv', stringsAsFactors = FALSE)
print(colnames(regc))

work <- select(regc, WIRELESSPHONENUMBER, RES_ADDRESS1, RES_ADDRESS2, RA_ZIP, BASEPRECINCT, CITYCODE, V1_LASTNAME, 
               V1_FIRSTNAME, V1_TITLECODE, V1_PDIID, V1_PARTY, V1_GENDER, V1_ETHNICITY, V1_AGE)


# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "THRIVE", table = "AllRegCallers")
bq_table_upload(bq_table, work, write_disposition = "WRITE_TRUNCATE")

#####
# reg voters 
tbl <- bq_table("slscampaigns-364520", "THRIVE", "Registered_Voter_DB")
reg <- bq_table_download(tbl, bigint = "integer64")

# CALL CONVOSO DATA
tbl <- bq_table("slscampaigns-364520", "THRIVE", "Convoso_June25")
call <- bq_table_download(tbl, bigint = "integer64")

# CALLING DATA PDI
tbl <- bq_table("slscampaigns-364520", "THRIVE", "AllRegCallers")
rcall <- bq_table_download(tbl, bigint = "integer64")


callid <- left_join(call, rcall, by=c("Number_Dialed"="WIRELESSPHONENUMBER"))

# newsletter report 
news <- callid[, 14:30]
news <- filter(news, ThriveLA_Newsletter_Sign_up == TRUE)
news <- select(news, -Thrive_Alternate_Issue_Text_Box)

write.csv(news, file = '/Users/birdieligos/Documents/Reports/ThriveNewsletter071825.csv', row.names = FALSE)

#
data <- select(callid, Date, V1_PDIID, ThriveQ1___Priority_Issues)

reg$PDIID         <- trimws(reg$PDIID)
data$V1_PDIID     <- trimws(data$V1_PDIID)
reg_data          <- left_join(reg, data, by = c("PDIID" = "V1_PDIID"))

reg_data <- reg_data[!is.na(reg_data$ThriveQ1___Priority_Issues) & reg_data$ThriveQ1___Priority_Issues != "", ]

reg_data$PRECINCT_CLEAN <- sub("^19900+0*", "", reg_data$PRECINCT)

reg_data$PRECINCT_CLEAN <- as.character(paste0("900", as.character(reg_data$PRECINCT_CLEAN)))


# Upload to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "THRIVE", table = "VoterOutcomes")
bq_table_upload(bq_table, reg_data, write_disposition = "WRITE_TRUNCATE")




