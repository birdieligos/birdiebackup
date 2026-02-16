# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


# Read All Flags export
upload <- read.csv('/Users/sanjanalad/Downloads/AllFlags_Rodriguez_021125.csv', stringsAsFactors = FALSE)

upload <- upload %>%
  distinct()

upload <- upload %>%
  mutate(
    ts   = ymd_hms(FLAGENTRYDATE, tz = "UTC"),
    DATE = as.Date(ts),
    TIME = format(ts, "%H:%M:%S")
  ) %>%
  select(-FLAGENTRYDATE, -ts)

upload <- filter(upload, FLAGNAME == "Mobile Canvassing")

# Format column names
colnames(upload) <- gsub("\\.", "_", colnames(upload))

print(colnames(upload))

upload <- select(upload, PDIID, STATEID, USERNAME, RESPONSECODE, RESPONSEDESCRIPTION,
                 CANVASSERNAME, MOBILEPROJECTASSIGMENT, COUNTYASSIGNEDID, PRECINCT,
                 DATE, TIME)

# Upload All Flags to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "SignatureGathering_2026", table = "Rodriguez_AllFlags")
bq_table_upload(bq_table, upload, write_disposition = "WRITE_TRUNCATE")


# Read Universe Contact List
universe <- read.csv('/Users/sanjanalad/Downloads/RodriguezrUniverseContactList_021126.csv', stringsAsFactors = FALSE)

universe <- select(universe, V1_PDIID, V1_LASTNAME, V1_FIRSTNAME, V1_PARTY, V1_GENDER,
                   V1_ETHNICITY, V1_AGE, RES_ADDRESS1, RES_ADDRESS2, RA_ZIP, BASEPRECINCT)

# Upload Universe to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "SignatureGathering_2026", table = "Rodriguez_Universe")
bq_table_upload(bq_table, universe, write_disposition = "WRITE_TRUNCATE")


# Join All Flags with Universe on PDIID
upload$PDIID <- trimws(upload$PDIID)
universe$V1_PDIID <- trimws(universe$V1_PDIID)

joined <- left_join(upload, universe, by = c("PDIID" = "V1_PDIID"))

# Upload joined data to BigQuery
bq_table <- bq_table(project = "slscampaigns-364520", dataset = "SignatureGathering_2026", table = "Rodriguez_SignatureData")
bq_table_upload(bq_table, joined, write_disposition = "WRITE_TRUNCATE")


# ---- VALIDATION CHECKS ----

# Check Rodriguez_AllFlags table
tbl <- bq_table("slscampaigns-364520", "SignatureGathering_2026", "Rodriguez_AllFlags")
allflags <- bq_table_download(tbl)
print(paste("AllFlags rows:", nrow(allflags)))
print(head(allflags))

# Check Rodriguez_SignatureData table (the joined one)
tbl2 <- bq_table("slscampaigns-364520", "SignatureGathering_2026", "Rodriguez_SignatureData")
sigdata <- bq_table_download(tbl2)
print(paste("SignatureData rows:", nrow(sigdata)))
print(paste("Response codes:", paste(unique(sigdata$RESPONSECODE), collapse = ", ")))

# Check a sample PDIID from All Flags
print("Sample PDIIDs from All Flags:")
print(head(upload$PDIID, 10))

# Check a sample V1_PDIID from Universe
print("Sample V1_PDIIDs from Universe:")
print(head(universe$V1_PDIID, 10))

# Check if ANY match
matched <- sum(upload$PDIID %in% universe$V1_PDIID)
print(paste("Matched rows:", matched, "out of", nrow(upload)))

# Check join quality - how many have address data
has_address <- sum(!is.na(joined$RES_ADDRESS1))
print(paste("Rows with address:", has_address, "out of", nrow(joined)))

