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

################## District Databases
# ad27
ad27 <- bq_table_download("slscampaigns-364520.Soria_2024.Returned_Ballots_Tracker")
# Create a named vector for ZIP to CITY mapping
zip_to_city <- c(
  "93722" = "Fresno", "93723" = "Fresno", "93727" = "Fresno", "93726" = "Fresno",
  "93711" = "Fresno", "93630" = "Kerman", "93606" = "Biola", "93706" = "Fresno",
  "93660" = "San Joaquin", "93608" = "Cantua Creek", "93627" = "Caruthers",
  "93668" = "Tranquillity", "93622" = "Firebaugh", "93640" = "Mendota",
  "93620" = "Dos Palos", "93609" = "Burrel", "93652" = "Raisin City",
  "93656" = "Riverdale", "93624" = "Five Points", "93210" = "Coalinga",
  "93234" = "Huron", "93637" = "Madera", "93638" = "Madera",
  "93610" = "Chowchilla", "95334" = "Livingston", "95341" = "Merced",
  "95348" = "Merced", "95340" = "Merced", "95322" = "Gustine",
  "95301" = "Atwater", "95333" = "Le Grand", "93635" = "Los Banos",
  "95317" = "Cressey", "95365" = "Planada", "95343" = "Merced",
  "95388" = "Winton", "95312" = "Ballico", "95303" = "Atwater",
  "95315" = "Delhi", "95380" = "Turlock", "95374" = "Stevinson",
  "93665" = "South Dos Palos", "93705" = "Fresno", "93737" = "Fresno"
)

# Add CITY column to ad27 using the mapping
ad27 <- ad27 %>%
  mutate(CITY = zip_to_city[as.character(ZIP)])

# ADD county column 
ad27 <- ad27 %>%
  rename(COUNTY = PRECINCT_SHAPE)

ad27 <- select(ad27, -ZIP)

ad27 <- ad27 %>%
  select(PARTY, RACE, AGE, GENDER, PRIMARY_VOTE, PROPENSITY, 
         PDI_IDEOLOGY, CITY, COUNTY, RETURN_STATUS, VOTER_COUNTS, RETURNS_COUNT, PRECINCT)

# sd14
sd14 <- bq_table_download("slscampaigns-364520.Soria_2024.SD14_Post_Election_Database")

colnames(sd14)[colnames(sd14) == "VOTED"] <- "RETURN_STATUS"

sd14$RETURN_STATUS <- ifelse(sd14$RETURN_STATUS == "DID NOT VOTE", "NO RETURN", 
                             ifelse(sd14$RETURN_STATUS == "VOTED", "TRUE", sd14$RETURN_STATUS))

sd14$VOTER_COUNTS <- 1

sd14$RETURNS_COUNT <- ifelse(sd14$RETURN_STATUS == "TRUE", 1, 0)

sd14 <- select(sd14, -PDIID, -STATEID, -COUNTYASSIGNEDID, -EARLY_VOTE_STATUS)

# get precinct detail
# Clean and convert PRECINCT column in ad27
ad27 <- ad27 %>%
  mutate(
    PRECINCT = gsub("[A-Za-z]", "", PRECINCT),                          # Remove letters
    PRECINCT = gsub("^10000000|^1000000|^100000|^24|^20", "", PRECINCT),   # Remove specific leading patterns
    PRECINCT = as.integer(PRECINCT)                                    # Convert to integer
  )

# Clean and convert PRECINCT column in sd14
sd14 <- sd14 %>%
  mutate(
    PRECINCT = gsub("[A-Za-z]", "", PRECINCT),                          # Remove letters
    PRECINCT = gsub("^10000000|^1000000|^100000|^24|^20", "", PRECINCT),        # Remove specific leading patterns
    PRECINCT = as.integer(PRECINCT)                                    # Convert to integer
  )

# Rename PRECINCT to Precinct in ad27
ad27 <- ad27 %>%
  rename(Precinct = PRECINCT)

# Rename PRECINCT to Precinct in sd14
sd14 <- sd14 %>%
  rename(Precinct = PRECINCT)


# 
sd14$Precinct <- as.character(sd14$Precinct)
sd14precincts$Precinct <- as.character(sd14precincts$Precinct)
ad27$Precinct <- as.character(ad27$Precinct)
ad27precincts$Precinct <- as.character(ad27precincts$Precinct)

# Left join both sd14precincts and ad27precincts to ad27
ad27 <- ad27 %>%
  left_join(ad27precincts, by = "Precinct") %>%
  left_join(sd14precincts, by = "Precinct")

# Left join both sd14precincts and ad27precincts to sd14
sd14 <- sd14 %>%
  left_join(ad27precincts, by = "Precinct") %>%
  left_join(sd14precincts, by = "Precinct")

# Replace NA values with "NO/NA" in ad27
ad27 <- ad27 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "NO/NA", .)))

# Replace NA values with "NO/NA" in sd14
sd14 <- sd14 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "NO/NA", .)))

# # Remove Precinct column from ad27
# ad27 <- ad27 %>%
#   select(-Precinct)
# 
# # Remove Precinct column from sd14
# sd14 <- sd14 %>%
#   select(-Precinct)

################################################## registration and voted df
# sd14$DISTRICT <- 'SD14'

sd14 <- select(sd14, -PDI_IDEOLOGY, -PRIMARY_VOTE, -PROPENSITY)

sd14 <- sd14 %>%
  group_by(across(-c(VOTER_COUNTS, RETURNS_COUNT))) %>%
  summarise(
    VOTER_COUNTS = sum(VOTER_COUNTS, na.rm = TRUE),
    RETURNS_COUNT = sum(RETURNS_COUNT, na.rm = TRUE),
    .groups = "drop"
  )

# ad27$DISTRICT <- 'AD27'

ad27 <- select(ad27, -PDI_IDEOLOGY, -PRIMARY_VOTE, -PROPENSITY)
ad27 <- ad27 %>%
  group_by(across(-c(VOTER_COUNTS, RETURNS_COUNT))) %>%
  summarise(
    VOTER_COUNTS = sum(VOTER_COUNTS, na.rm = TRUE),
    RETURNS_COUNT = sum(RETURNS_COUNT, na.rm = TRUE),
    .groups = "drop"
  )
############################################## rbind

register <- rbind(sd14, ad27)


# Remove duplicate rows in register
register <- register %>%
  distinct()

register <- register %>%
  group_by(across(-c(VOTER_COUNTS, RETURNS_COUNT))) %>%
  summarise(
    VOTER_COUNTS = sum(VOTER_COUNTS, na.rm = TRUE),
    RETURNS_COUNT = sum(RETURNS_COUNT, na.rm = TRUE),
    .groups = "drop"
  )



# Set BigQuery project, dataset, and table details
project_id <- "slscampaigns-364520"
dataset_id <- "Soria_2024"
table_id <- "Senate_Analysis_Register_Data"

# Define the schema
schema <- list(
  list(name = "Precinct", type = "INT64"),
  list(name = "PARTY", type = "STRING"),
  list(name = "RACE", type = "STRING"),
  list(name = "AGE", type = "STRING"),
  list(name = "GENDER", type = "STRING"),
  list(name = "CITY", type = "STRING"),
  list(name = "COUNTY", type = "STRING"),
  list(name = "RETURN_STATUS", type = "STRING"),
  list(name = "WITHIN_AD27", type = "STRING"),
  list(name = "WITHIN_SD14", type = "STRING"),
  list(name = "VOTER_COUNTS", type = "INT64"),
  list(name = "RETURNS_COUNT", type = "INT64")
)

# Upload the dataframe to BigQuery
bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = register,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED", # Creates the table if it doesn't exist
  write_disposition = "WRITE_TRUNCATE"     # Overwrites the table if it exists
)

################################################# get precincts 
# get 27 precincts 
# county
ad27precincts <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - AD27PRECINCT.csv', stringsAsFactors = FALSE)

# pdi
#ad27precincts <- ad27 %>% distinct(PRECINCT)

# ad27precincts <- ad27precincts %>%
#   mutate(
#     PRECINCT = gsub("[A-Za-z]", "", PRECINCT),                
#     PRECINCT = gsub("^100000|^24|^20", "", PRECINCT)         
#   )
# 
# ad27precincts <- ad27precincts %>%
#   mutate(
#     PRECINCT = gsub("^0" ,"", PRECINCT)         
#   )

ad27precincts <- ad27precincts %>%
  distinct()

ad27precincts$WITHIN_AD27 <- 'YES'

ad27precincts <- ad27precincts %>%
  mutate(Precinct = as.numeric(trimws(PRECINCT))) %>%
  select(-PRECINCT)

# get sd 14 precincts
sd14precincts <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - SD14PRECINCT (2).csv', stringsAsFactors = FALSE)

#pdi
sd14precincts <- sd14 %>% distinct(PRECINCT, COUNTY)

sd14precincts <- sd14precincts %>%
  rename(County = COUNTY)

sd14precincts <- sd14precincts %>%
  mutate(
    PRECINCT = gsub("[A-Za-z]", "", PRECINCT),
    PRECINCT = gsub("^100000|^24|^20", "", PRECINCT)
  )

sd14precincts <- sd14precincts %>%
  mutate(
    PRECINCT = gsub("^0" ,"", PRECINCT)
  )

sd14precincts <- sd14precincts %>%
  mutate(
    PRECINCT = gsub("^0" ,"", PRECINCT)
  )

sd14precincts <- sd14precincts %>%
  distinct()

sd14precincts$WITHIN_SD14 <- 'YES'

sd14precincts <- sd14precincts %>%
  mutate(Precinct = as.numeric(trimws(PRECINCT))) %>%
  select(-PRECINCT)

########### Election results 
senate <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - SENATE.csv', stringsAsFactors = FALSE)
cd13 <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - CD13.csv', stringsAsFactors = FALSE)
cd21 <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - CD21.csv', stringsAsFactors = FALSE)
cd21 <- cd21 %>%
  filter(!is.na(County) & County != 0 & County != "")
ad27 <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - AD27 (8).csv', stringsAsFactors = FALSE)
pres <- read.csv('/Users/birdieligos/Downloads/Soria Senate Analysis Data - PRES.csv', stringsAsFactors = FALSE)
pres <- select(pres, County, Precinct, Times.Cast, Registered..Voters, TRUMP, HARRIS)

########## run all races
# Define a function to process each dataframe
process_dataframe <- function(df, ad27precincts, sd14precincts, race_name) {
  df <- df %>%
    rename_with(~ gsub("\\.+", ".", .)) %>%
    rename_with(~ gsub("\\.", "_", .)) %>%
    mutate(
      Times_Cast = as.integer(gsub(",", "", Times_Cast)),
      Registered_Voters = as.integer(gsub(",", "", Registered_Voters)),
      Precinct = as.numeric(trimws(Precinct))
    ) %>%
    left_join(ad27precincts, by = "Precinct") %>%
    left_join(sd14precincts, by = c("Precinct" = "Precinct", "County" = "County")) %>%
    mutate(
      WITHIN_AD27 = ifelse(is.na(WITHIN_AD27), "NO/NA", WITHIN_AD27),
      WITHIN_SD14 = ifelse(is.na(WITHIN_SD14), "NO/NA", WITHIN_SD14)
    ) %>%
    rename(REP = 5, DEM = 6) %>% # Rename column 5 to REP and column 6 to DEM
    mutate(
      REP = as.integer(gsub(",", "", REP)), # Remove commas and convert to integer
      DEM = as.integer(gsub(",", "", DEM))  # Remove commas and convert to integer
    ) %>%
    mutate(RACE = race_name) # Add RACE column
  return(df)
}

# Apply the function to each dataframe
presfinal <- process_dataframe(pres, ad27precincts, sd14precincts, "PRESIDENT")
senatefinal <- process_dataframe(senate, ad27precincts, sd14precincts, "SENATE")
cd13final <- process_dataframe(cd13, ad27precincts, sd14precincts, "CD13")
cd21final <- process_dataframe(cd21, ad27precincts, sd14precincts, "CD21")
ad27final <- process_dataframe(ad27, ad27precincts, sd14precincts, "AD27")

# combine race results
final_combined <- rbind(presfinal, senatefinal, cd13final, cd21final, ad27final)

########################## write big query table
library(bigrquery)

# Set project ID and dataset details
project_id <- "slscampaigns-364520"  
dataset_id <- "Soria_2024"  
table_id <- "2024_GElection_Results"    

# Specify the schema
schema <- list(
  list(name = "County", type = "STRING"),
  list(name = "Precinct", type = "INT64"),
  list(name = "Times_Cast", type = "INT64"),
  list(name = "Registered_Voters", type = "INT64"),
  list(name = "REP", type = "INT64"),
  list(name = "DEM", type = "INT64"),
  list(name = "WITHIN_AD27", type = "STRING"),
  list(name = "WITHIN_SD14", type = "STRING"),
  list(name = "RACE", type = "STRING")
)

# Write to BigQuery
bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = final_combined,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED", # Creates the table if it doesn’t exist
  write_disposition = "WRITE_TRUNCATE"     # Overwrites the table if it exists
)





