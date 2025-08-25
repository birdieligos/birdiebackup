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
ad27_database <- bq_table_download('slscampaigns-364520.Soria_2024.Registrar_Voters_Database')
ad27_database <- select(ad27_database, PDIID, PARTY, RACE, AGE, GENDER, PRIMARY_VOTE)
ad27_database$DISTRICT <- 'AD27'

ad74_database <- bq_table_download('slscampaigns-364520.Duncan_2024.Registrar_Voters_Database') 
ad74_database <- select(ad74_database, PDIID, PARTY, RACE, AGE, GENDER, PRIMARY_VOTE)
ad74_database$DISTRICT <- 'AD74'

ad76_database <- bq_table_download('slscampaigns-364520.Patel_2024.Registrar_Voters_Database') 
ad76_database <- select(ad76_database, PDIID, PARTY, RACE, AGE, GENDER, PRIMARY_VOTE)
ad76_database$DISTRICT <- 'AD76'

districtdemo <- rbind(ad27_database, ad74_database, ad76_database)
districtdemo <- distinct(districtdemo, PDIID, .keep_all = TRUE)
districtdemo <- districtdemo %>% mutate(PDIID = str_trim(PDIID))
########################### all flags 

#### soria
af1 <- read.csv('/Users/birdieligos/Downloads/AllFlags_1of4 (1).csv', stringsAsFactors = FALSE)
af2 <- read.csv('/Users/birdieligos/Downloads/AllFlags_2of4 (1).csv', stringsAsFactors = FALSE)
af3 <- read.csv('/Users/birdieligos/Downloads/AllFlags_3of4 (1).csv', stringsAsFactors = FALSE)
af4 <- read.csv('/Users/birdieligos/Downloads/AllFlags_4of4 (1).csv', stringsAsFactors = FALSE)

ad27AF <- rbind(af1, af2, af3, af4)

### duncan
af1 <- read.csv('/Users/birdieligos/Downloads/AllFlags_1of5.csv', stringsAsFactors = FALSE)
af2 <- read.csv('/Users/birdieligos/Downloads/AllFlags_2of5.csv', stringsAsFactors = FALSE)
af3 <- read.csv('/Users/birdieligos/Downloads/AllFlags_3of5.csv', stringsAsFactors = FALSE)
af4 <- read.csv('/Users/birdieligos/Downloads/AllFlags_4of5.csv', stringsAsFactors = FALSE)
af5 <- read.csv('/Users/birdieligos/Downloads/AllFlags_5of5.csv', stringsAsFactors = FALSE)

ad74AF <- rbind(af1, af2, af3, af4, af5)

###  patel
af1 <- read.csv('/Users/birdieligos/Downloads/AllFlags_1of4 (2).csv', stringsAsFactors = FALSE)
af2 <- read.csv('/Users/birdieligos/Downloads/AllFlags_2of4 (2).csv', stringsAsFactors = FALSE)
af3 <- read.csv('/Users/birdieligos/Downloads/AllFlags_3of4 (2).csv', stringsAsFactors = FALSE)
af4 <- read.csv('/Users/birdieligos/Downloads/AllFlags_4of4 (2).csv', stringsAsFactors = FALSE)
ad76AF <- rbind(af1, af2, af3, af4)
# ALLFLAGS RBIND 
allflags <- rbind(ad27AF, ad74AF, ad76AF)


##########################################  CREATE TARGET UNIVERSE DATAFRAME
# UNIVERSE IDS
universe <- filter(allflags, RESPONSEDESCRIPTION %in% c("No Answer", "Undecided", "Voice Mail", 'Strong Support', 'No', 'Already Voted', 
                                                        'Lean Support', 'Refused Survey', 'Language Barrier', 'Poll Vote', "Women's Right", 
                                                        'Moved', 'Maybe', 'Human - Non Contact', 'Left Message', 'Lean Oppose', 'Contacted', 
                                                        'Not Home', 'Bad Number (Home)'))
universe <- select(universe, PDIID)
universe$PDIID <- str_trim(universe$PDIID)
universe <- distinct(universe, PDIID, .keep_all = TRUE)
universe$TARGET_UNIVERSE <- 'TARGET UNIVERSE'

##########################################  CREATE ID UNIVERSE DATAFRAME

flagged <- filter(allflags, RESPONSEDESCRIPTION %in% c("Strong Support", 'Lean Support', "Undecided", 'Lean Oppose', 'Strong Oppose'))
flagged <- select(flagged, PDIID)
flagged$PDIID <- str_trim(flagged$PDIID)
flagged <- distinct(flagged, PDIID, .keep_all = TRUE)
flagged$SUPPORT_ID <- 'SUPPORT ID'

################ VOTED 24G

voted <- read.csv('/Users/birdieligos/Downloads/ADEM_VOTEDG24_041025.csv', stringsAsFactors = FALSE)
voted <- select(voted, PDIID)
voted <- voted %>% mutate(VOTED24G = "VOTED")
voted <- voted %>% mutate(PDIID = str_trim(PDIID))
voted <- distinct(voted, PDIID, .keep_all = TRUE)

############################################################## WORK TAB 1
# save
backup <- districtdemo
# go
districtdemo <- backup
#


districtvote <- left_join(districtdemo, voted)

district_voteflag <- left_join(districtvote, flagged)

district_voteflagtarget <- left_join(district_voteflag, universe)

# FINAL
final <- district_voteflagtarget

final <- final %>%
  mutate(
    VOTED24G = if_else(is.na(VOTED24G), "DID NOT VOTE", VOTED24G),
    SUPPORT_ID = if_else(is.na(SUPPORT_ID), "NO ID", SUPPORT_ID),
    TARGET_UNIVERSE = if_else(is.na(TARGET_UNIVERSE), "NON PRIORITY TARGET", TARGET_UNIVERSE)
  )

final <- final %>%
  mutate(VOTED24G = if_else(VOTED24G == "VOTED", 1, 0))

# summary_table <- tibble(
#   Group = c("Target Universe", "Support ID Group", "All Voters"),
#   Turnout = c(
#     mean(final[TARGET_UNIVERSE == "TARGET UNIVERSE"], na.rm = TRUE),
#     mean(final[SUPPORT_ID != "NO ID"], na.rm = TRUE),
#     mean(final, na.rm = TRUE)
#   )
# )

# Subset for Support ID Group (voters with a support ID)
support_df <- final %>%
  filter(SUPPORT_ID != "NO ID") %>%
  mutate(GROUP = "Support ID Group")

# Subset for Target Universe (voters in the target universe)
target_df <- final %>%
  filter(TARGET_UNIVERSE == "TARGET UNIVERSE") %>%
  mutate(GROUP = "Target Universe")

# All voters (no filtering)
all_df <- final %>%
  mutate(GROUP = "All Voters")

# Combine all three subsets by rows
combined_df <- bind_rows(support_df, target_df, all_df)

combined_df <- combined_df %>%
  mutate(VOTED24G = if_else(
    VOTED24G == 0,
    "DID NOT VOTE",
    if_else(VOTED24G == 1, "VOTED", NA_character_)
  ))

combined_df <- select(combined_df, -PDIID, -SUPPORT_ID, -TARGET_UNIVERSE)

########################## WRITE TAB 1 BIG Q TABLE
library(bigrquery)

project_id <- "slscampaigns-364520"  
dataset_id <- "GENERAL24_ANALYSIS"  
table_id <- "ADEM_PE_TAB1_041225"    

schema <- list(
  list(name = "PARTY", type = "STRING"),
  list(name = "RACE", type = "STRING"),
  list(name = "AGE", type = "STRING"),
  list(name = "GENDER", type = "STRING"),
  list(name = "PRIMARY_VOTE", type = "STRING"),
  list(name = "DISTRICT", type = "STRING"),
  list(name = "VOTED24G", type = "STRING"),
  list(name = "GROUP", type = "STRING")
)

bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = combined_df,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED", 
  write_disposition = "WRITE_TRUNCATE"    
)


############################################################### TAB 2
# reset tables
demo2 <- backup
af2 <- allflags
# go

################ SUPPORT IDS

flagged <- filter(af2, RESPONSEDESCRIPTION %in% c("Strong Support", 'Lean Support', "Undecided", 'Lean Oppose', 'Strong Oppose'))

flagged <- select(flagged, PDIID, FLAGENTRYDATE, RESPONSECODE)

flagged <- flagged %>%
  mutate(DATE = as.Date(ymd_hms(FLAGENTRYDATE)))

flagged <- flagged %>%
  mutate(`SUPPORT_ID` = case_when(
    `RESPONSECODE` %in% c("SS", "LS") ~ "SUPPORT",
    `RESPONSECODE` == "U" ~ "UNDECIDED",
    `RESPONSECODE` %in% c("SO", "LO") ~ "OPPOSE",
    TRUE ~ `RESPONSECODE`
  ))

flagged <- select(flagged, -RESPONSECODE, -FLAGENTRYDATE)

flagged$TYPE <- 'SUPPORT ID'

############################################### POLLING DATA 

soria <- read.csv('/Users/birdieligos/Downloads/SORIAPOLL41225 - DATA SHEET (1).csv', stringsAsFactors = FALSE)

duncan <- read.csv('/Users/birdieligos/Downloads/DUNCANPOLL41225 - DATA SHEET (1).csv', stringsAsFactors = FALSE)
duncan <- duncan %>% select(-c(13:31))

patel <- read.csv('/Users/birdieligos/Downloads/PATELPOLL41225 - DATA SHEET.csv', stringsAsFactors = FALSE)


### clean 

names(soria)  <- gsub("\\.", "_", names(soria))
names(duncan) <- gsub("\\.", "_", names(duncan))
names(patel)  <- gsub("\\.", "_", names(patel))

soria <- soria %>% filter(!is.na(DATE) & DATE != "")
duncan <- duncan %>% filter(!is.na(DATE) & DATE != "")
patel <- patel %>% filter(!is.na(DATE) & DATE != "")

soria <- soria %>%
  mutate(across(4:12, ~ ifelse(. %in% c("n/a", ""), NA, .))) %>%
  mutate(across(4:12, as.integer))

duncan <- duncan %>%
  mutate(across(4:12, ~ ifelse(. %in% c("n/a", ""), NA, .))) %>%
  mutate(across(4:12, as.integer))

patel <- patel %>%
  mutate(across(4:12, ~ ifelse(. %in% c("n/a", ""), NA, .))) %>%
  mutate(across(4:12, as.integer))


### pivot 

soria_long <- soria %>%
  pivot_longer(cols = 4:12, names_to = "Party_Vote", values_to = "VOTERS") %>%
  separate(Party_Vote, into = c("PARTY", "SUPPORT_ID"), sep = "_")

duncan_long <- duncan %>%
  pivot_longer(cols = 4:12, names_to = "Party_Vote", values_to = "VOTERS") %>%
  separate(Party_Vote, into = c("PARTY", "SUPPORT_ID"), sep = "_")

patel_long <- patel %>%
  pivot_longer(cols = 4:12, names_to = "Party_Vote", values_to = "VOTERS") %>%
  separate(Party_Vote, into = c("PARTY", "SUPPORT_ID"), sep = "_")


polling <- rbind(soria_long, duncan_long, patel_long)
polling$TYPE <- 'POLLING'

polling <- polling %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))

print(unique(polling$DATE))
#################### DEMOS 
# save 
universe <- demo2
# go 


demos <- universe %>%
  pivot_longer(
    cols = c("RACE", "AGE", "GENDER", "PRIMARY_VOTE"),
    values_to = "DEMO",
    names_to = NULL
  )

print(unique(demos$DEMO))


#### COMBINE DEMO AND FLAGGED

flagged <- flagged %>% mutate(PDIID = str_trim(PDIID))
demos <- demos %>% mutate(PDIID = str_trim(PDIID))

combined_df <- left_join(flagged, demos, by = "PDIID")

combined_df <- combined_df %>% filter(!is.na(PARTY))

combined_df$VOTERS <- 1

combined_df <- select(combined_df, -PDIID)

combined_df <- combined_df %>%
  dplyr::group_by(DATE, SUPPORT_ID, TYPE, PARTY, DISTRICT, DEMO) %>%
  dplyr::summarize(VOTERS = sum(VOTERS, na.rm = TRUE)) %>%
  dplyr::ungroup()


print(unique(polling$DATE))

print(colnames(combined_df))


library(dplyr)
library(tidyr)

# 1. make sure DATE is Date
combined_df <- combined_df %>% mutate(DATE = as.Date(DATE))

# 2. district → poll‐date lookup
poll_dates_df <- bind_rows(
  tibble(DISTRICT="AD27",
         POLL_DATE=as.Date(c("2024-06-24","2024-09-18","2024-09-26","2024-10-08","2024-10-20"))),
  tibble(DISTRICT="AD74",
         POLL_DATE=as.Date(c("2024-08-20","2024-09-23","2024-10-07","2024-10-24"))),
  tibble(DISTRICT="AD76",
         POLL_DATE=as.Date(c("2024-06-25","2024-09-25","2024-10-09","2024-10-23")))
)

# 3. distinct groups
groups_df <- combined_df %>% distinct(SUPPORT_ID, TYPE, PARTY, DISTRICT, DEMO)

# 4. only expand each group with its own poll dates
expanded <- groups_df %>% inner_join(poll_dates_df, by="DISTRICT")

# 5. cumulative VOTERS up to each district‑specific POLL_DATE
result <- expanded %>%
  rowwise() %>%
  mutate(VOTERS = sum(
    combined_df$VOTERS[
      combined_df$SUPPORT_ID==SUPPORT_ID &
        combined_df$TYPE      ==TYPE      &
        combined_df$PARTY     ==PARTY     &
        combined_df$DEMO      ==DEMO      &
        combined_df$DISTRICT  ==DISTRICT  &
        combined_df$DATE     <=POLL_DATE
    ], na.rm=TRUE
  )) %>%
  ungroup()

# then proceed with your bind_rows/summarise steps as before
print(unique(soria$DATE))
print(unique(duncan$DATE))
print(unique(patel$DATE))


################### BIND POLLING AND SUPPORT 

result_all <- result %>%
  dplyr::select(-DEMO) %>%
  dplyr::group_by(SUPPORT_ID, TYPE, PARTY, DISTRICT, POLL_DATE) %>%
  dplyr::summarize(VOTERS = sum(VOTERS, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(DEMO = "ALL PARTY")

result_final <- bind_rows(result, result_all)

polling <- polling %>% 
  rename(POLL_DATE = DATE)

tab2 <- rbind(result_final, polling)

print(unique(tab2))

print(colnames(tab2))
#################################################### poll name
# helper for ordinal suffixes
ordinal_suffix <- function(x) {
  ifelse(x %% 10 == 1 & x %% 100 != 11, "st",
         ifelse(x %% 10 == 2 & x %% 100 != 12, "nd",
                ifelse(x %% 10 == 3 & x %% 100 != 13, "rd", "th")))
}

# extract day as integer
days <- as.integer(format(tab2$POLL_DATE, "%d"))

# create POLL_NAME like "Poll: Sep 1st"
tab2$POLL_NAME <- paste0(
  "Poll: ",
  format(tab2$POLL_DATE, "%b "),  # abbreviated month
  days,
  ordinal_suffix(days)
)

################################################################## PIVOT SUPPORT IDS
# 
# tab2 <- tab2 %>%
#   pivot_wider(
#     names_from   = SUPPORT_ID,
#     values_from  = VOTERS,
#     values_fill  = 0
#   )
# 


############################################################### WRITE TAB 2 BIG Q TABLE
library(bigrquery)

project_id <- "slscampaigns-364520"  
dataset_id <- "GENERAL24_ANALYSIS"  
table_id <- "ADEM_PE_TAB2_041225_TEST"    

schema <- list(
  list(name = "POLL_DATE", type = "DATE"),
  list(name = "TYPE", type = "STRING"),
  list(name = "PARTY", type = "STRING"),
  list(name = "DISTRICT", type = "STRING"),
  list(name = "DEMO", type = "STRING"),
  list(name = "SUPPORT_ID", type = "STRING"),
  list(name = "VOTERS", type = "INT64"),
  list(name = "POLL_NAME", type = "STRING")
)

#  list(name = "SUPPORT", type = "INT64"),
#  list(name = "UNDECIDED", type = "INT64"),
#   list(name = "OPPOSE", type = "INT64"),

bq_table_upload(
  x = bq_table(project_id, dataset_id, table_id),
  values = tab2,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED", 
  write_disposition = "WRITE_TRUNCATE"    
)




