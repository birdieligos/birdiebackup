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

######################################################## ad 31 results
ad31 <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNOAD31.csv', stringsAsFactors = FALSE)

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]


# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes, 
               JOAQUIN.ARAMBULA..DEM.., SOLOMON.VERDUZCO..REP..)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

print(sum(ad31$Times_Cast))

####################################### ADD IN FRESNO CITY COUNCIL DISTRICT 1

cc1 <- read.csv('/Users/birdieligos/Downloads/Report-(1_16_2025) (7).csv', stringsAsFactors = FALSE)

cc1 <- select(cc1, Description)

cc1 <- cc1 %>% rename(Precinct = Description)

cc1$CC1 <- 'YES'


cc1$Precinct <- gsub("^1", "", cc1$Precinct)
cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
cc1$Precinct <- as.integer(cc1$Precinct)
cc1 <- cc1[cc1$Precinct != 93330013, ]

######### JOIN AD31 RESULTS WITH CITY OF FRESNO D1 PRECINCTS TO SEE OVERLAP
test <- left_join(ad31, cc1, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% # Rename Precinct.x to Precinct
  select(-Precinct.y)              # Remove Precinct.y

test$CC1[is.na(test$CC1)] <- "NO"

test$AD31 <- 'YES'
test$RACE <- 'AD31'

### bring in city 
# city <- read.csv('/Users/birdieligos/Documents/PRECINCT COUNT REPORTS/AD31 PRECINCT AND CITY.csv', stringsAsFactors = FALSE)
# 
# city <- select(city, Description, City)
# city <- city %>% rename(Precinct = Description)
# city$Precinct <- gsub("^0+", "", city$Precinct)
# city$Precinct <- as.integer(city$Precinct)
# 
# cc1 <- city
# cc1$Precinct <- gsub("^1", "", cc1$Precinct)
# cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
# cc1$Precinct <- as.integer(cc1$Precinct)
# cc1 <- cc1[cc1$Precinct != 93330013, ]
# city <- cc1
# 
# test <- left_join(test, city)

######################################################################## NO ON 33
ad31 <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP33.csv', stringsAsFactors = FALSE)

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]


# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

colnames(ad31) <- gsub("^NO\\.\\.", "SOLOMON.VERDUZCO..REP..", colnames(ad31))
colnames(ad31) <- gsub("^YES\\.\\.", "JOAQUIN.ARAMBULA..DEM..", colnames(ad31))
# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes, 
               JOAQUIN.ARAMBULA..DEM.., SOLOMON.VERDUZCO..REP..)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

print(sum(ad31$Times_Cast))

############################# ADD IN FRESNO CITY COUNCIL DISTRICT 1


cc1 <- read.csv('/Users/birdieligos/Downloads/Report-(1_16_2025) (7).csv', stringsAsFactors = FALSE)

cc1 <- select(cc1, Description)

cc1 <- cc1 %>% rename(Precinct = Description)

cc1$CC1 <- 'YES'


cc1$Precinct <- gsub("^1", "", cc1$Precinct)
cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
cc1$Precinct <- as.integer(cc1$Precinct)
cc1 <- cc1[cc1$Precinct != 93330013, ]


################## ADD IN AD31
ad31precincts <- read.csv('/Users/birdieligos/Downloads/Untitled spreadsheet - Sheet1 (1).csv', stringsAsFactors = FALSE)

ad31precincts <- select(ad31precincts, Precinct)

ad31precincts$AD31 <- 'YES'

ad31precincts$Precinct <- gsub("^1", "", ad31precincts$Precinct)
ad31precincts$Precinct <- gsub("^0+", "", ad31precincts$Precinct)
ad31precincts$Precinct <- as.integer(ad31precincts$Precinct)
ad31precincts <- ad31precincts[ad31precincts$Precinct != 93330013, ]

### trim ws 
ad31 <- ad31 %>% mutate(Precinct = str_trim(Precinct))
ad31precincts <- ad31precincts %>% mutate(Precinct = str_trim(Precinct))
cc1 <- cc1 %>% mutate(Precinct = str_trim(Precinct))

######### JOIN AD31 RESULTS WITH CITY OF FRESNO D1 PRECINCTS TO SEE OVERLAP
test1 <- left_join(ad31, cc1, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   
test1 <- left_join(test1, ad31precincts, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   

#test1 <- right_join(test1, ad31precincts)

test1$CC1[is.na(test1$CC1)] <- "NO"
test1$AD31[is.na(test1$AD31)] <- "NO"

test1$RACE <- 'NO ON PROP 33'

test1 <- test1[!(test1$AD31 == 'NO' & test1$CC1 == 'NO'), ]

############################## bind all races 
test <- rbind(test1, test)            # Remove Precinct.y

######################################################################## PROP 36
ad31 <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP 36.csv', stringsAsFactors = FALSE)

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]


# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

colnames(ad31) <- gsub("^NO\\.\\.", "SOLOMON.VERDUZCO..REP..", colnames(ad31))
colnames(ad31) <- gsub("^YES\\.\\.", "JOAQUIN.ARAMBULA..DEM..", colnames(ad31))
# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes, 
               JOAQUIN.ARAMBULA..DEM.., SOLOMON.VERDUZCO..REP..)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

print(sum(ad31$Times_Cast))

############################# ADD IN FRESNO CITY COUNCIL DISTRICT 1


cc1 <- read.csv('/Users/birdieligos/Downloads/Report-(1_16_2025) (7).csv', stringsAsFactors = FALSE)

cc1 <- select(cc1, Description)

cc1 <- cc1 %>% rename(Precinct = Description)

cc1$CC1 <- 'YES'


cc1$Precinct <- gsub("^1", "", cc1$Precinct)
cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
cc1$Precinct <- as.integer(cc1$Precinct)
cc1 <- cc1[cc1$Precinct != 93330013, ]


################## ADD IN AD31
ad31precincts <- read.csv('/Users/birdieligos/Downloads/Untitled spreadsheet - Sheet1 (1).csv', stringsAsFactors = FALSE)

ad31precincts <- select(ad31precincts, Precinct)

ad31precincts$AD31 <- 'YES'

ad31precincts$Precinct <- gsub("^1", "", ad31precincts$Precinct)
ad31precincts$Precinct <- gsub("^0+", "", ad31precincts$Precinct)
ad31precincts$Precinct <- as.integer(ad31precincts$Precinct)
ad31precincts <- ad31precincts[ad31precincts$Precinct != 93330013, ]

### trim ws 
ad31 <- ad31 %>% mutate(Precinct = str_trim(Precinct))
ad31precincts <- ad31precincts %>% mutate(Precinct = str_trim(Precinct))
cc1 <- cc1 %>% mutate(Precinct = str_trim(Precinct))

######### JOIN AD31 RESULTS WITH CITY OF FRESNO D1 PRECINCTS TO SEE OVERLAP
test1 <- left_join(ad31, cc1, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   
test1 <- left_join(test1, ad31precincts, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   

#test1 <- right_join(test1, ad31precincts)

test1$CC1[is.na(test1$CC1)] <- "NO"
test1$AD31[is.na(test1$AD31)] <- "NO"

test1$RACE <- 'PROP 36'

test1 <- test1[!(test1$AD31 == 'NO' & test1$CC1 == 'NO'), ]

############################## bind all races 
test <- rbind(test1, test)  

######################################################################## PROP 3
ad31 <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP 3.csv', stringsAsFactors = FALSE)

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]


# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

colnames(ad31) <- gsub("^NO\\.\\.", "SOLOMON.VERDUZCO..REP..", colnames(ad31))
colnames(ad31) <- gsub("^YES\\.\\.", "JOAQUIN.ARAMBULA..DEM..", colnames(ad31))
# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes, 
               JOAQUIN.ARAMBULA..DEM.., SOLOMON.VERDUZCO..REP..)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

print(sum(ad31$Times_Cast))

############################# ADD IN FRESNO CITY COUNCIL DISTRICT 1


cc1 <- read.csv('/Users/birdieligos/Downloads/Report-(1_16_2025) (7).csv', stringsAsFactors = FALSE)

cc1 <- select(cc1, Description)

cc1 <- cc1 %>% rename(Precinct = Description)

cc1$CC1 <- 'YES'


cc1$Precinct <- gsub("^1", "", cc1$Precinct)
cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
cc1$Precinct <- as.integer(cc1$Precinct)
cc1 <- cc1[cc1$Precinct != 93330013, ]


################## ADD IN AD31
ad31precincts <- read.csv('/Users/birdieligos/Downloads/Untitled spreadsheet - Sheet1 (1).csv', stringsAsFactors = FALSE)

ad31precincts <- select(ad31precincts, Precinct)

ad31precincts$AD31 <- 'YES'

ad31precincts$Precinct <- gsub("^1", "", ad31precincts$Precinct)
ad31precincts$Precinct <- gsub("^0+", "", ad31precincts$Precinct)
ad31precincts$Precinct <- as.integer(ad31precincts$Precinct)
ad31precincts <- ad31precincts[ad31precincts$Precinct != 93330013, ]

### trim ws 
ad31 <- ad31 %>% mutate(Precinct = str_trim(Precinct))
ad31precincts <- ad31precincts %>% mutate(Precinct = str_trim(Precinct))
cc1 <- cc1 %>% mutate(Precinct = str_trim(Precinct))

######### JOIN AD31 RESULTS WITH CITY OF FRESNO D1 PRECINCTS TO SEE OVERLAP
test1 <- left_join(ad31, cc1, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   
test1 <- left_join(test1, ad31precincts, by = join_by(Precinct), keep = TRUE) %>%
  rename(Precinct = Precinct.x) %>% 
  select(-Precinct.y)   

#test1 <- right_join(test1, ad31precincts)

test1$CC1[is.na(test1$CC1)] <- "NO"
test1$AD31[is.na(test1$AD31)] <- "NO"

test1$RACE <- 'PROP 3'

test1 <- test1[!(test1$AD31 == 'NO' & test1$CC1 == 'NO'), ]

############################## bind all races 
test <- rbind(test1, test)  


############################################################################### bring in city 
city <- read.csv('/Users/birdieligos/Documents/PRECINCT COUNT REPORTS/FRESNO CCD1 UPDATED.csv', stringsAsFactors = FALSE)
# 
city <- select(city, Description, City)
city <- city %>% rename(Precinct = Description)
city$Precinct <- gsub("^0+", "", city$Precinct)
city$Precinct <- as.integer(city$Precinct)

cc1 <- city
cc1$Precinct <- gsub("^1", "", cc1$Precinct)
cc1$Precinct <- gsub("^0+", "", cc1$Precinct)
cc1$Precinct <- as.integer(cc1$Precinct)
cc1 <- cc1[cc1$Precinct != 93330013, ]
city <- cc1

#test <- left_join(test, city)

########################################################################## check precincts matches 
city$Precinct <- trimws(city$Precinct)
city$Precinct <- as.integer(city$Precinct)
test$Precinct <- trimws(test$Precinct)
test$Precinct <- as.integer(test$Precinct)

test2 <- merge(test, city)
########################################################################## WRITE BIG QUERY TABLE 
# Create BigQuery table
project_id <- "slscampaigns-364520"
dataset_id <- "Perea_2026"
table_id <- "POST_ELECTION_2024"

# Create a table reference
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists
table_exists <- bq_table_exists(table_ref)

# If the table exists, delete it
if (table_exists) {
  bq_table_delete(table_ref)
}

# Define the schema based on the columns of the test dataframe
schema <- list(
  bq_field("Precinct", "INT64"),
  bq_field("Times_Cast", "INT64"),
  bq_field("Registered_Voters", "INT64"),
  bq_field("Undervotes", "INT64"),
  bq_field("Overvotes", "INT64"),
  bq_field("JOAQUIN_ARAMBULA_DEM_", "INT64"),
  bq_field("SOLOMON_VERDUZCO_REP_", "INT64"),
  bq_field("CC1", "STRING"),
  bq_field("AD31", "STRING"),
  bq_field("RACE", "STRING")
)

# Attempt to create the BigQuery table and upload the data
tryCatch({
  # Create the table with the defined schema
  bq_table_create(table_ref, fields = schema)
  
  Sys.sleep(10)  # Sleep for 10 seconds before uploading data
  
  # Upload the data to the newly created table
  bq_table_upload(table_ref, values = test)
  
  # Print a success message if the table creation and data upload are successful
  cat("Table created and data uploaded successfully!\n")
}, 
error = function(e) {
  # Print error message if an error occurs
  cat("An error occurred:", conditionMessage(e), "\n")
})




