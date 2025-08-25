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

# sd14
sd14 <- bq_table_download("slscampaigns-364520.Soria_2024.SD14_Post_Election_Database")


# Rename columns in ad27
ad27 <- ad27 %>%
  rename("Registered Voters" = VOTER_COUNTS, 
         "Voted" = RETURNS_COUNT)

# Remove unnecessary columns
ad27 <- ad27 %>%
  select(-PRECINCT_SHAPE, -SUPPORTID, -DATE_RETURNED, -RETURN_STATUS)

# ### add zip
# zip_to_city <- c(
#   "93722" = "Fresno", "93723" = "Fresno", "93727" = "Fresno", "93726" = "Fresno",
#   "93711" = "Fresno", "93630" = "Kerman", "93606" = "Biola", "93706" = "Fresno",
#   "93660" = "San Joaquin", "93608" = "Cantua Creek", "93627" = "Caruthers",
#   "93668" = "Tranquillity", "93622" = "Firebaugh", "93640" = "Mendota",
#   "93620" = "Dos Palos", "93609" = "Burrel", "93652" = "Raisin City",
#   "93656" = "Riverdale", "93624" = "Five Points", "93210" = "Coalinga",
#   "93234" = "Huron", "93637" = "Madera", "93638" = "Madera",
#   "93610" = "Chowchilla", "95334" = "Livingston", "95341" = "Merced",
#   "95348" = "Merced", "95340" = "Merced", "95322" = "Gustine",
#   "95301" = "Atwater", "95333" = "Le Grand", "93635" = "Los Banos",
#   "95317" = "Cressey", "95365" = "Planada", "95343" = "Merced",
#   "95388" = "Winton", "95312" = "Ballico", "95303" = "Atwater",
#   "95315" = "Delhi", "95380" = "Turlock", "95374" = "Stevinson",
#   "93665" = "South Dos Palos", "93705" = "Fresno", "93737" = "Fresno"
# )
# 
# 
# ad27 <- ad27 %>%
#   mutate(CITY = zip_to_city[as.character(ZIP)])
# 
### add COUNTY to ad27

ad27$COUNTY <- ifelse(grepl("^10", ad27$PRECINCT), "Fresno",
                      ifelse(grepl("^20", ad27$PRECINCT), "Madera",
                             ifelse(grepl("^24", ad27$PRECINCT), "Merced", NA)))



setdiff(names(sd14), names(ad27)) # Columns in sd14 but not in ad14
setdiff(names(ad27), names(sd14)) # Columns in ad14 but not in sd14

# clean 

sd14 <- select(sd14, -COUNTYASSIGNEDID, -PDIID, -STATEID, -CITY)

sd14$REGISTERED_VOTERS <- 1

sd14 <- sd14 %>%
  mutate(VOTED = ifelse(VOTED == "VOTED", 1, 0))

sd14 <- sd14 %>%
  group_by(across(-c(VOTED, REGISTERED_VOTERS))) %>%
  summarise(
    VOTED = sum(VOTED, na.rm = TRUE),
    REGISTERED_VOTERS = sum(REGISTERED_VOTERS, na.rm = TRUE),
    .groups = "drop"
  )

sd14 <- sd14 %>%
  mutate(across(where(is.character), str_trim))

# precinct list
sd14_unique <- sd14

sd14_unique <- select(sd14_unique, PRECINCT, COUNTY)

sd14_unique <- sd14_unique[!duplicated(sd14_unique), ]

sd14_unique$SD <- 'SD14'

sd14_unique <- sd14_unique[!duplicated(sd14_unique), ]

# ad27

ad27 <- mutate(ad27, VOTED = as.integer(Voted))
ad27 <- mutate(ad27, REGISTERED_VOTERS = as.integer(`Registered Voters`))

ad27 <- select(ad27, -ZIP, -Voted, -`Registered Voters`)

ad27 <- ad27 %>%
  group_by(across(-c(VOTED, REGISTERED_VOTERS))) %>%
  summarise(
    VOTED = sum(VOTED, na.rm = TRUE),
    REGISTERED_VOTERS = sum(REGISTERED_VOTERS, na.rm = TRUE),
    .groups = "drop"
  )


ad27 <- ad27 %>%
  mutate(across(where(is.character), str_trim))

# precinct list
ad27_unique <- ad27

ad27_unique <- select(ad27_unique, PRECINCT, COUNTY)

ad27_unique <- ad27_unique[!duplicated(ad27_unique), ]

ad27_unique$AD <- 'AD27'

ad27_unique <- ad27_unique[!duplicated(ad27_unique), ]

#### merge 

merge_districts <- full_join(sd14_unique, ad27_unique, by = c("PRECINCT", "COUNTY"))

merge_districts <- merge_districts %>%
  mutate(
    AD = ifelse(is.na(AD), "NOT AD27", AD),
    SD = ifelse(is.na(SD), "NOT SD14", SD)
  )


merged_df <- rbind(ad27, sd14)

merged_df <- merged_df %>% distinct()

merged_df <- merged_df %>%
  mutate(
    COUNTY = trimws(COUNTY),
    PRECINCT = trimws(PRECINCT)
  )

merge_districts <- merge_districts %>%
  mutate(
    COUNTY = trimws(COUNTY),
    PRECINCT = trimws(PRECINCT)
  )


merge <- left_join(merged_df, merge_districts, by = c("PRECINCT", "COUNTY"))

#
merge <- merge %>%
  mutate(
    DISTRICT_OVERLAP = case_when(
      AD == "AD27" & SD == "SD14" ~ "SD14/AD27 OVERLAP",
      SD == "SD14" & AD == "NOT AD27" ~ "SD14 excludes AD27",
      SD == "NOT SD14" & AD == "AD27" ~ "AD27 excludes SD14",
      TRUE ~ NA_character_
    )
  )

merge$STAND_ALONE_DISTRICT <- merge$DISTRICT_OVERLAP

ad27 <- filter(merge, AD == 'AD27')
ad27$STAND_ALONE_DISTRICT <- 'AD27'


sd14 <- filter(merge, SD == 'SD14')
sd14$STAND_ALONE_DISTRICT <- 'SD14'

merge <- rbind(sd14, ad27, merge)

merge <- merge %>% distinct()

###################### Set BigQuery project, dataset, and table details
project_id <- "slscampaigns-364520"
dataset_id <- "Soria_2024"
table_id <- "SD14_AD27_OVERLAP_022325"

# Define the schema
schema <- list(
  list(name = "PRECINCT", type = "STRING"),
  list(name = "PARTY", type = "STRING"),
  list(name = "RACE", type = "STRING"),
  list(name = "AGE", type = "STRING"),
  list(name = "GENDER", type = "STRING"),
  list(name = "PRIMARY_VOTE", type = "STRING"),
  list(name = "PROPENSITY", type = "STRING"),
  list(name = "PDI_IDEOLOGY", type = "STRING"),
  list(name = "EARLY_VOTE_STATUS", type = "STRING"),
  list(name = "CITY", type = "STRING"),
  list(name = "COUNTY", type = "STRING"),
  list(name = "SD", type = "STRING"),
  list(name = "AD", type = "STRING"),
  list(name = "DISTRICT_OVERLAP", type = "STRING"),
  list(name = "STAND_ALONE_DISTRICT", type = "STRING"),
  list(name = "REGISTERED_VOTERS", type = "INT64"),
  list(name = "VOTED", type = "INT64")
)

# Upload the dataframe to BigQuery
bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = merge,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED", 
  write_disposition = "WRITE_TRUNCATE"     # Overwrites the table if it exists
)



