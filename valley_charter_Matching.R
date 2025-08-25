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


enroll <- read.csv('/Users/birdieligos/Downloads/MS PS Data 5.15.2025 - match pull (1).csv', stringsAsFactors = FALSE)

app <- read.csv('/Users/birdieligos/Downloads/MS SM Data 5.19.25 - matchpull.csv', stringsAsFactors = FALSE)


merge <- rbind(enroll, app)

merge$phone_number <- gsub("[^0-9]", "", merge$phone_number)
merge$phone2 <- gsub("[^0-9]", "", merge$phone2)

merge <- merge %>%
  tidyr::extract(
    street1,
    into = c("House_Number", "Street_Name", "Street_Name_Type", "Apt_Num"),
    regex = "^(\\d+)\\s+([A-Za-z]+)\\s+([A-Za-z]+)(?:,\\s*(Apt\\s*\\d+))?$"
  ) %>%
  dplyr::mutate(
    Street_Name      = stringr::str_to_upper(Street_Name),
    Street_Name_Type = stringr::str_to_title(Street_Name_Type),
    House_Number     = stringr::str_trim(House_Number),
    Apt_Num          = stringr::str_trim(Apt_Num)
  )

merge <- merge %>%
  tidyr::separate(
    Apt_Num,
    into = c("Apt_Type", "Apt_Number"),
    sep = "\\s+",
    fill = "right",
    remove = TRUE
  ) %>%
  dplyr::mutate(
    Apt_Number = as.character(Apt_Number)
  )

###### L2 min

l2min <- read.csv('/Users/birdieligos/Downloads/L2_min_052025.csv', stringsAsFactors = FALSE)


l2min$FirstName <- str_to_title(l2min$FirstName)
l2min$MiddleName <- str_to_title(l2min$MiddleName)
l2min$LastName <- str_to_title(l2min$LastName)
l2min$MATCHED <- 'YES'
l2min$Landline_Phone_Number <- trimws(l2min$Landline_Phone_Number)
l2min$Cell_Phone <- trimws(l2min$Cell_Phone)
l2min$EmailAddresses_EmailAddress <- trimws(l2min$EmailAddresses_EmailAddress)
merge$phone_number <- trimws(merge$phone_number)
merge$phone2 <- trimws(merge$phone2)
merge$email <- trimws(merge$email)

print(colnames(l2min))


# merge 1
l2min_phone1 <- select(l2min, LALCONSUMERID, Individual_ID, Cell_Phone, MATCHED)

merge1 <- merge %>% left_join(l2min_phone1, by = c("phone_number" = "Cell_Phone"))

merge1_yes <- merge1 %>% filter(MATCHED == "YES")
merge1_no  <- merge1 %>% filter(is.na(MATCHED))

# merge 2
l2min_phone1 <- select(l2min, LALCONSUMERID, Individual_ID, Cell_Phone, MATCHED)
l2min_phone1 <- l2min_phone1 %>% filter(!is.na(Cell_Phone))
merge1_no <- select(merge1_no, -LALCONSUMERID, -Individual_ID, -MATCHED)

merge2 <- merge1_no %>% left_join(l2min_phone1, by = c("phone2" = "Cell_Phone"))
merge2_yes <- merge2 %>% filter(MATCHED == "YES")
merge2_no  <- merge2 %>% filter(is.na(MATCHED))

# merge 3 
l2min_phone1 <- select(l2min, LALCONSUMERID, Individual_ID, EmailAddresses_EmailAddress, MATCHED)
l2min_phone1 <- l2min_phone1 %>% filter(!is.na(EmailAddresses_EmailAddress))
merge2_no <- select(merge2_no, -LALCONSUMERID, -Individual_ID, -MATCHED)

merge3 <- merge2_no %>% left_join(l2min_phone1, by = c("email" = "EmailAddresses_EmailAddress"))
merge3_yes <- merge3 %>% filter(MATCHED == "YES")
merge3_no  <- merge3 %>% filter(is.na(MATCHED))


# matching round 1 done 
matchall1 <- rbind(merge1_yes, merge2_yes)
unmatched <- merge3_no


# build out all L2 data 

l2min <- read.csv('/Users/birdieligos/Downloads/L2_min_052025.csv', stringsAsFactors = FALSE)


l2min$FirstName <- str_to_title(l2min$FirstName)
l2min$MiddleName <- str_to_title(l2min$MiddleName)
l2min$LastName <- str_to_title(l2min$LastName)
l2min$MATCHED <- 'YES'
l2min$Landline_Phone_Number <- trimws(l2min$Landline_Phone_Number)
l2min$Cell_Phone <- trimws(l2min$Cell_Phone)
l2min$EmailAddresses_EmailAddress <- trimws(l2min$EmailAddresses_EmailAddress)

# l2 max

l2_max <- read.csv('/Users/birdieligos/Downloads/L2_builtout_052025.csv', stringsAsFactors = FALSE)


l2_maxcut <- select(l2_max, LALCONSUMERID, Individual_ID, FirstName, LastName, Address_City, Address_Zip, Address_ZipPlus4, Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress)
l2_maxcut$FirstName <- str_to_title(l2_maxcut$FirstName)
l2_maxcut$LastName <- str_to_title(l2_maxcut$LastName)
l2_maxcut$Address_City <- str_to_title(l2_maxcut$Address_City)
l2_maxcut$Address_Zip <- str_to_title(l2_maxcut$Address_Zip)
l2_maxcut$Address_ZipPlus4 <- str_to_title(l2_maxcut$Address_ZipPlus4)
l2_maxcut$MATCHED <- 'YES'


# unmatched 

nomatch <- unmatched

nomatch <- nomatch %>%
  tidyr::separate(zipcode, into = c("Address_Zip", "Address_ZipPlus4"), sep = "-", remove = TRUE) %>%
  dplyr::mutate(dplyr::across(c(Address_Zip, Address_ZipPlus4), ~ stringr::str_trim(.)))

nomatch <- nomatch %>%
  dplyr::mutate(city = stringr::str_to_upper(city))

nomatch <- nomatch %>%
  dplyr::mutate(
    dplyr::across(
      c(city, state, Address_Zip, Address_ZipPlus4, Parent.1.Name, Parent.1.Last.Name),
      ~ stringr::str_trim(.)
    )
  )

l2_maxcut <- l2_maxcut %>%
  dplyr::mutate(
    Address_Zip       = as.character(Address_Zip),
    Address_ZipPlus4  = as.character(Address_ZipPlus4)
  )

nomatch <- select(nomatch, -LALCONSUMERID, -Individual_ID, -MATCHED)
#### merge round 2
l2_maxcut <- l2_maxcut %>%
  dplyr::mutate(
    Address_Zip       = as.character(Address_Zip),
    Address_ZipPlus4  = as.character(Address_ZipPlus4)
  )

match_test <- complete3_no %>%
  dplyr::left_join(
    l2_maxcut,
    by = c(
      "Parent.1.Name"      = "FirstName",
      "Parent.1.Last.Name" = "LastName",
      "city"               = "Address_City",
      "Address_Zip"        = "Address_Zip"
    )
  )



"Parent.1.Name"      = "FirstName",
"Parent.1.Last.Name" = "LastName",
"city"               = "Address_City",
"Address_Zip"        = "Address_Zip",
"Address_ZipPlus4"   = "Address_ZipPlus4"

complete1 <- match_test
complete1_yes <- complete1 %>% filter(MATCHED == "YES")
complete1_no  <- complete1 %>% filter(is.na(MATCHED))
complete1_no <- select(complete1_no, -LALCONSUMERID, -Individual_ID, -MATCHED)


complete2 <- match_test
complete2_yes <- complete2 %>% filter(MATCHED == "YES")
complete2_no  <- complete2 %>% filter(is.na(MATCHED))
complete2_no <- select(complete2_no, -LALCONSUMERID, -Individual_ID, -MATCHED)
complete2_yes <- complete2_yes %>%
  dplyr::select(-Address_ZipPlus4.y) %>%
  dplyr::rename(Address_ZipPlus4 = Address_ZipPlus4.x)




complete3 <- match_test
complete3_yes <- complete3 %>% filter(MATCHED == "YES")
complete3_no  <- complete3 %>% filter(is.na(MATCHED))
complete3_no <- select(complete3_no, -LALCONSUMERID, -Individual_ID, -MATCHED)
complete3_yes <- complete3_yes %>%
  dplyr::mutate(Address_ZipPlus4 = dplyr::coalesce(Address_ZipPlus4.x, Address_ZipPlus4.y)) %>%
  dplyr::select(-Address_ZipPlus4.x, -Address_ZipPlus4.y)


matchround2 <- rbind(complete3_yes, complete2_yes, complete1_yes)

complete3_no <- complete3_no %>%
  dplyr::mutate(Address_ZipPlus4 = dplyr::coalesce(Address_ZipPlus4.x, Address_ZipPlus4.y)) %>%
  dplyr::select(-Address_ZipPlus4.x, -Address_ZipPlus4.y)

###### match round 3

l2max_cut2 <- select(l2_max, LALCONSUMERID, FamilyID, Individual_ID, FirstName, LastName,
                     Address_AddressLine, Address_HouseNumber, Address_StreetName,
                     Address_City, Address_Zip, Address_ZipPlus4, Address_ApartmentNum, Address_ApartmentType,
                     EmailAddresses_EmailAddress, Landline_Phone_Number, Cell_Phone)

l2max_cut2$MATCH <- 'YES'


###################################################################### PEOPLE MATCHING
library(dplyr)
library(purrr)
library(tibble)
library(stringr)

enroll <- read.csv('/Users/birdieligos/Downloads/MS PS Data 5.15.2025 - match pull (1).csv', stringsAsFactors = FALSE)

app <- read.csv('/Users/birdieligos/Downloads/MS SM Data 5.19.25 - matchpull.csv', stringsAsFactors = FALSE)


merge <- rbind(enroll, app)

merge$phone_number <- gsub("[^0-9]", "", merge$phone_number)
merge$phone2 <- gsub("[^0-9]", "", merge$phone2)

merge <- merge %>%
  tidyr::extract(
    street1,
    into = c("House_Number", "Street_Name", "Street_Name_Type", "Apt_Num"),
    regex = "^(\\d+)\\s+([A-Za-z]+)\\s+([A-Za-z]+)(?:,\\s*(Apt\\s*\\d+))?$"
  ) %>%
  dplyr::mutate(
    Street_Name      = stringr::str_to_upper(Street_Name),
    Street_Name_Type = stringr::str_to_title(Street_Name_Type),
    House_Number     = stringr::str_trim(House_Number),
    Apt_Num          = stringr::str_trim(Apt_Num)
  )

merge <- merge %>%
  tidyr::separate(
    Apt_Num,
    into = c("Apt_Type", "Apt_Number"),
    sep = "\\s+",
    fill = "right",
    remove = TRUE
  ) %>%
  dplyr::mutate(
    Apt_Number = as.character(Apt_Number)
  )

merge <- merge %>%
  tidyr::separate(zipcode, into = c("Address_Zip", "Address_ZipPlus4"), sep = "-", remove = TRUE) %>%
  dplyr::mutate(dplyr::across(c(Address_Zip, Address_ZipPlus4), ~ stringr::str_trim(.)))

merge <- merge %>%
  dplyr::mutate(city = stringr::str_to_upper(city))

merge <- merge %>%
  dplyr::mutate(
    dplyr::across(
      c(city, state, Address_Zip, Address_ZipPlus4, Parent.1.Name, Parent.1.Last.Name),
      ~ stringr::str_trim(.)
    )
  )

######## l2

l2max_cut2 <- select(l2_max, FamilyID, Individual_ID, FirstName, LastName,
                     Address_AddressLine, Address_HouseNumber, Address_StreetName,
                     Address_City, Address_Zip, Address_ZipPlus4, Address_ApartmentNum, Address_ApartmentType,
                     EmailAddresses_EmailAddress, Landline_Phone_Number, Cell_Phone)

l2max_cut2$MATCH <- 'YES'

# FORMAT 
l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    Address_Zip       = as.character(Address_Zip),
    Address_ZipPlus4  = as.character(Address_ZipPlus4)
  )

l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    dplyr::across(
      c(FirstName, LastName),
      ~ stringr::str_to_title(.)
    )
  )

l2max_cut2$Landline_Phone_Number <- trimws(l2max_cut2$Landline_Phone_Number)
l2max_cut2$Cell_Phone <- trimws(l2max_cut2$Cell_Phone)
l2max_cut2$EmailAddresses_EmailAddress <- trimws(l2max_cut2$EmailAddresses_EmailAddress)


# l2 min xtra 

l2min <- read.csv('/Users/birdieligos/Downloads/L2_min_052025.csv', stringsAsFactors = FALSE)
l2min <- select(l2min, Individual_ID, Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress)

l2max_cut2 <- left_join(l2max_cut2, l2min, by = "Individual_ID")
##
final1_no <- merge


####### contact info 1 to 1 matching first 
# ————————————————————————
# A) Add surrogate keys
# ————————————————————————

final1_no <- final1_no %>%
  mutate(RowID = row_number())


# ————————————————————————
# A) PHONE / EMAIL MATCHES FIRST
# ————————————————————————

library(dplyr)
library(purrr)
library(stringr)

# 1. Define each 1-key phone/email map
pe_maps <- list(
  c("phone_number" = "Landline_Phone_Number.x"),
  c("phone_number" = "Landline_Phone_Number.y"),
  c("phone_number" = "Cell_Phone.x"),
  c("phone_number" = "Cell_Phone.y"),
  
  c("phone2"       = "Landline_Phone_Number.x"),
  c("phone2"       = "Landline_Phone_Number.y"),
  c("phone2"       = "Cell_Phone.x"),
  c("phone2"       = "Cell_Phone.y"),
  
  c("email"        = "EmailAddresses_EmailAddress.x"),
  c("email"        = "EmailAddresses_EmailAddress.y")
)

# 2. Helper to trim whitespace
trim_df <- function(df, cols) df %>% mutate(across(all_of(cols), str_trim))

# 3. Build your exact matches at MATCH_CONF = 1
pe_matches <- map_dfr(pe_maps, function(by_map) {
  geo_key <- names(by_map)[1]
  l2_key  <- unname(by_map)[1]
  
  # a) drop NA/blanks on both sides
  geo_clean <- final1_no %>%
    filter(!is.na(.data[[geo_key]]), .data[[geo_key]] != "") %>%
    trim_df(geo_key)
  
  l2_clean  <- l2max_cut2 %>%
    filter(!is.na(.data[[l2_key]]),  .data[[l2_key]]  != "") %>%
    trim_df(l2_key)
  
  # b) inner-join on the single key
  inner_join(geo_clean, l2_clean, by = by_map, relationship = "many-to-many") %>%
    transmute(
      RowID,
      Individual_ID,
      MATCH_COMBO = paste(geo_key, l2_key, sep = " + "),
      MATCH_LEVEL = 1L,
      MATCH_CONF  = 1
    )
})

# 4. Remove those matched records from both tables
matched_rows <- unique(pe_matches$RowID)
matched_ids  <- unique(pe_matches$Individual_ID)

final1_rem <- final1_no  %>% filter(!RowID        %in% matched_rows)
l2max_rem   <- l2max_cut2 %>% filter(!Individual_ID %in% matched_ids)

# ————————————————————————
# B) FALL BACK TO EXISTING ADDRESS-BASED LOGIC
# ————————————————————————

library(dplyr)
library(purrr)
library(stringr)
library(tibble)

# 5. Trim whitespace helper (already defined)
trim_df <- function(df, keys) df %>% mutate(across(all_of(keys), ~ str_trim(.)))

# 6. build summary of every combo (optional)
#    — you can leave this as is, or apply the same nonblank logic if you want precise counts

# 1. Define your var_map first
var_map <- c(
  "Parent.1.Last.Name" = "LastName",
  "House_Number"       = "Address_HouseNumber",
  "Street_Name"        = "Address_StreetName",
  "city"               = "Address_City",
  "Address_Zip"        = "Address_Zip",
  "Address_ZipPlus4"   = "Address_ZipPlus4",
  "Apt_Number"         = "Address_ApartmentNum",
  "email"              = "EmailAddresses_EmailAddress.x"
)

# 2. Build all non-empty subsets of final1_rem keys from var_map
valid_subsets <- unlist(
  lapply(seq_along(var_map), function(k)
    combn(names(var_map), k, simplify = FALSE)
  ),
  recursive = FALSE
)

# 3. Filter only subsets that actually exist in final1_rem
valid_subsets <- keep(valid_subsets, ~ all(.x %in% names(final1_rem)))

# 4. Then calculate max_len safely
max_len <- max(map_int(valid_subsets, length))
# 7. produce one-row-per-match at every level without matching on blanks/NA
max_len <- max(map_int(valid_subsets, length))

matchplay <- map_dfr(valid_subsets, function(keys) {
  by_map <- setNames(var_map[keys], keys)
  
  # a) filter out any rows with blank/NA in any of the key columns
  geo_clean <- final1_rem %>%
    trim_df(keys) %>%
    filter(if_all(all_of(keys), ~ !is.na(.) & . != ""))
  
  l2_clean  <- l2max_rem %>%
    trim_df(unname(by_map)) %>%
    filter(if_all(all_of(unname(by_map)), ~ !is.na(.) & . != ""))
  
  # b) join only those completely nonblank rows
  inner_join(geo_clean, l2_clean, by = by_map, relationship = "many-to-many") %>%
    transmute(
      RowID,
      Individual_ID,
      MATCH_COMBO = paste(keys, collapse = " + "),
      MATCH_LEVEL = length(keys),
      MATCH_CONF  = length(keys) / max_len
    )
})

final_match <- rbind(pe_matches, matchplay)


### MATCHING DONE 
best_matches <- final_match %>%
  group_by(RowID) %>%
  slice_max(MATCH_CONF, with_ties = FALSE) %>%
  ungroup()

library(dplyr)

# 1. Look at the overall distribution of MATCH_CONF
conf_dist <- best_matches %>%
  count(MATCH_CONF) %>%
  arrange(desc(MATCH_CONF)) %>%
  mutate(pct = n / sum(n) * 100)

print(conf_dist)

################################################### MATCHING OVER
################################################### 
# GEO LEO LONG ADD AND DISTANCE COLUMNS
################################################### 

l2_max <- read.csv('/Users/birdieligos/Downloads/L2_builtout_052025.csv', stringsAsFactors = FALSE)
l2_geo <- select(l2_max, FamilyID, Individual_ID, Latitude, Longitude)

library(dplyr)
library(geosphere)
library(recipes)

# 1. School location
school_loc <- c(lon = -118.44425, lat = 34.20051)

# 2. Enrolled household coords (from your best_matches + L2 file)
enrolled_ids <- select(best_matches, Individual_ID)

# Step 2: find enrolled FamilyIDs via matching Individual_IDs
enrolled_families <- left_join(enrolled_ids, l2_geo)

enrolled_families <- select(enrolled_families, FamilyID)

# Step 3: pull lat/lon for those FamilyIDs (one row per family)
enrolled_coords <- l2_geo %>%
  filter(FamilyID %in% enrolled_families$FamilyID) %>%
  distinct(FamilyID, Longitude, Latitude) %>%
  select(lon = Longitude, lat = Latitude)


# 3. Distance to school (in miles)
l2_geo <- l2_geo %>%
  mutate(
    dist_to_school = distHaversine(
      cbind(Longitude, Latitude),
      school_loc
    ) / 1609.344  # meters → miles
  )

# 4. Compute distance to nearest enrolled household
target_mat <- as.matrix(l2_geo %>% select(lon = Longitude, lat = Latitude))
enroll_mat <- as.matrix(enrolled_coords %>% select(lon, lat))

dist_matrix <- distm(target_mat, enroll_mat, fun = distHaversine)

l2_geo <- l2_geo %>%
  mutate(
    dist_to_enrolled = apply(dist_matrix, 1, min) / 1000
  )
str(target_mat)
str(enroll_mat)

### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# GEO MATCHING MATCH CONF 
### ### ### ### ### ### ### ### ### ### ### ### ### ### 

l2max_cut2 <- l2_max

l2max_cut2 <- select(l2max_cut2, Individual_ID, Address_HouseNumber, Address_StreetName,
                     Address_City, Address_Zip, Address_ZipPlus4, Address_ApartmentNum)

library(dplyr)
library(purrr)
library(tibble)
library(stringr)

var_map <- c(
  House_Number      = "Address_HouseNumber",
  Street_Name       = "Address_StreetName",
  city              = "Address_City",
  Address_Zip       = "Address_Zip",
  Address_ZipPlus4  = "Address_ZipPlus4",
  Apt_Number        = "Address_ApartmentNum"
)

all_subsets <- unlist(
  lapply(seq_along(var_map), function(k) combn(names(var_map), k, simplify = FALSE)),
  recursive = FALSE
)

valid_subsets <- keep(all_subsets, ~ "city" %in% .x)

trim_df <- function(df, cols) df %>% mutate(across(all_of(cols), str_trim))

final1_no  <- trim_df(final1_no,   names(var_map))
l2max_cut2 <- trim_df(l2max_cut2,  unname(var_map))

summary_geo_tbl <- map_dfr(valid_subsets, function(keys) {
  by_map <- setNames(var_map[keys], keys)
  joined <- final1_no %>% left_join(l2max_cut2, by = by_map, relationship = "many-to-many")
  tibble(
    combo       = paste(keys, collapse = " + "),
    n_matched   = sum(!is.na(joined$Individual_ID)),
    n_unmatched = sum(is.na(joined$Individual_ID)),
    total       = nrow(joined)
  )
})

max_len <- max(map_int(valid_subsets, length))

geo_match <- map_dfr(valid_subsets, function(keys) {
  by_map <- setNames(var_map[keys], keys)
  final1_no %>%
    left_join(l2max_cut2, by = by_map) %>%
    filter(!is.na(Individual_ID)) %>%
    mutate(
      MATCH_COMBO = paste(keys, collapse = " + "),
      MATCH_LEVEL = length(keys),
      MATCH_CONF  = length(keys) / max_len
    )
})

################################################### 
# CENTROID MODEL FORMAT STARTS HEREE 
################################################### 

# consumer
l2_demo <- select(l2_max,	Individual_ID, FamilyID,	Residence_HHParties_Description,
                  AgeRange, Childrens_General, Grandchildren_Int, Senior_Adult_In_HH, Young_Adult_In_HH, 
                  Marital_Status, Occupation_Group, Ethnic_Group, Hispanic_Country_Code,	Assimilation_Codes,
                  Language_Code,	Religion_Code,	Education_of_Person, Social_Ranking_Index_by_Individual,
                  Social_Ranking_Index_by_Area,	Likely_Income_Ranking_by_Area,	Likely_Educational_Attainment_Ranking_by_Area,
                  Homeowner_Probability_Model, CRA_Income_Classification_Code, Generations_In_HH, Inferred_Age,
                  Gender)


# A) join demos
l2_geodemo <- l2_geo %>%
  left_join(l2_demo)

Mode <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Build one‐row‐per‐household summary with marital, age, gender, geo, demo features
household_df <- l2_geodemo %>%
  group_by(FamilyID) %>%
  summarise(
    # Geography
    dist_to_school   = min(dist_to_school,   na.rm = TRUE),
    dist_to_enrolled = min(dist_to_enrolled, na.rm = TRUE),
    
    # Household size & adult counts
    num_members       = n(),
    num_adults        = sum(Inferred_Age >= 18, na.rm = TRUE),
    
    # Age‐subgroup flags
    parent_age_flag   = any(Inferred_Age >= 28 & Inferred_Age <= 55, na.rm = TRUE),
    senior_age_flag   = any(Inferred_Age >= 55, na.rm = TRUE),
    
    # Marital composition among parent‐aged
    n_parents         = sum(Inferred_Age >= 28 & Inferred_Age <= 55, na.rm = TRUE),
    parent_n_single   = sum(Inferred_Age >= 28 & Inferred_Age <= 55 & Marital_Status=="Single",  na.rm=TRUE),
    parent_n_married  = sum(Inferred_Age >= 28 & Inferred_Age <= 55 & Marital_Status=="Married", na.rm=TRUE),
    parent_pct_single = if_else(n_parents>0, parent_n_single/n_parents, NA_real_),
    parent_pct_married= if_else(n_parents>0, parent_n_married/n_parents, NA_real_),
    
    # Marital composition among seniors
    n_seniors         = sum(Inferred_Age >= 55, na.rm = TRUE),
    senior_n_single   = sum(Inferred_Age >= 55 & Marital_Status=="Single",  na.rm=TRUE),
    senior_n_married  = sum(Inferred_Age >= 55 & Marital_Status=="Married", na.rm=TRUE),
    senior_pct_single = if_else(n_seniors>0, senior_n_single/n_seniors, NA_real_),
    senior_pct_married= if_else(n_seniors>0, senior_n_married/n_seniors, NA_real_),
    
    # Gender composition & single‐mother flag
    n_female          = sum(Gender=="Female", na.rm=TRUE),
    n_male            = sum(Gender=="Male",   na.rm=TRUE),
    pct_female        = n_female/num_members,
    pct_male          = n_male/num_members,
    mixed_gender      = (n_female>0 & n_male>0),
    single_mother     = (parent_n_single>0 & n_male==0 & n_female>0),
    
    # Socioeconomic medians
    med_social_ind    = median(Social_Ranking_Index_by_Individual,            na.rm=TRUE),
    med_income_area   = median(Likely_Income_Ranking_by_Area,                na.rm=TRUE),
    med_educ_area     = median(Likely_Educational_Attainment_Ranking_by_Area,na.rm=TRUE),
    
    # Key demo modes
    Ethnicity         = Mode(Ethnic_Group),
    Homeowner         = Mode(Homeowner_Probability_Model),
    Religion          = Mode(Religion_Code),
    Languages         = Mode(Language_Code),
    Generations       = Mode(Generations_In_HH),
    
    .groups = "drop"
  )

household_df <- household_df %>%
  mutate(enrolled_flag = FamilyID %in% enrolled_families)


################################################### 
# CENTROID MODEL STARTS HEREE 
################################################### 

### match list 
matchids <- select(best_matches, Individual_ID)
matchids$enrolled_flag <- 1
l2tiny <- select(l2_max, FamilyID, Individual_ID)
matchids_fam <- left_join(matchids, l2tiny)
matchids_fam <- select(matchids_fam, FamilyID)
matchids_fam <- matchids_fam %>% 
  distinct()

household_df <- household_df %>%
  mutate(
    enrolled_flag = if_else(
      FamilyID %in% matchids_fam$FamilyID,
      1L,
      0L
    )
  )


# C) Proceed with centroid scoring or supervised model on household_df

library(dplyr)
library(recipes)

# 1) Build preprocessing recipe for household_df
rec_centroid <- recipe(enrolled_flag ~ ., data = household_df) %>%
  update_role(FamilyID, new_role = "id") %>%
  step_impute_median(all_numeric_predictors()) %>%    # fill any remaining NA nums
  step_unknown(all_nominal_predictors()) %>%          # mark NAs in factors as “unknown”
  step_dummy(all_nominal_predictors()) %>%            # one-hot encode all categories
  step_zv(all_predictors()) %>%                      # drop zero-variance cols
  step_normalize(all_numeric_predictors())            # center & scale numeric

# 2) Prep + bake into design matrix
prepped     <- prep(rec_centroid, training = household_df)
X_hh_matrix <- bake(prepped, new_data = household_df) %>%
  select(-FamilyID, -enrolled_flag) %>%
  as.matrix()

# 3) Compute centroid of your seed set (enrolled_flag == 1)
seed_idx    <- which(household_df$enrolled_flag == 1)
centroid    <- colMeans(X_hh_matrix[seed_idx, , drop = FALSE], na.rm = TRUE)

# 4) Compute Euclidean distance of every household to that centroid
dists       <- sqrt(rowSums((X_hh_matrix - centroid)^2))

# 5) Convert to similarity [0–1]
similarity  <- 1 - (dists - min(dists)) / (max(dists) - min(dists))

# 6) Attach scores and bucket
results_hh <- household_df %>%
  mutate(
    similarity       = similarity,
    overall_score    = round(similarity * 100),          # 0–100 score
    percentile_score = round(percent_rank(similarity) * 100),
    decile           = ntile(similarity, 10),            # 1 (lowest) … 10 (highest)
    persona_tier     = case_when(
      decile >=  8 ~ "Highly Likely",
      decile >=  4 ~ "Somewhat Likely",
      TRUE         ~ "Unlikely"
    )
  )

# 7) Inspect distribution
table(results_hh$decile)
summary(results_hh$overall_score)

# 8) Extract top targets
top_10pct <- results_hh %>%
  filter(decile == 10) %>%
  arrange(desc(overall_score))

top_5pct  <- results_hh %>%
  filter(overall_score >= quantile(overall_score, 0.95, na.rm = TRUE)) %>%
  arrange(desc(overall_score))

# 9) View results
head(results_hh)
head(top_10pct)
head(top_5pct)

print(colnames(top_10pct))
top_targets_output_demo <-select(top_10pct, enrolled_flag, percentile_score, persona_tier, FamilyID, dist_to_school, dist_to_enrolled, parent_age_flag,
                            senior_age_flag, single_mother, Ethnicity, Languages, Generations)

l2output <- select(l2_max, FamilyID, Individual_ID, Salutation, FirstName, LastName, Address_AddressLine, Address_ExtraAddressLine, Address_City, 
                   Address_State, Address_Zip,	Address_ZipPlus4, Inferred_Age, Gender, Inferred_HH_Rank, Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress)


top_target_output <- left_join(top_targets_output_demo, l2output)

top_target_output <- left_join(top_target_output, best_geo_match)

top_target_output <- top_target_output %>%
  rename(
    lookalike_tier = persona_tier,
    likely_parent  = parent_age_flag,
    likely_single_mother  = single_mother,
    lookalike_score  = percentile_score,
    likely_senior  = senior_age_flag,
    neighbor_score  = MATCH_CONF
  )

top_target_output <- filter(top_target_output, enrolled_flag == 0)

top_target_output <- select(top_target_output, -enrolled_flag)

write.csv(top_target_output, file = '/Users/birdieligos/Documents/Reports/VCH_toptargetsR1_052325.csv', row.names = FALSE)

print(count(unique(top_target_output$FamilyID)))

################################################### 
# buckets
################################################### 

scored <- results_hh

scored <- select(scored, FamilyID, dist_to_school, dist_to_enrolled, num_adults, parent_age_flag, senior_age_flag, 
                 parent_pct_single, n_seniors, senior_pct_single, single_mother, enrolled_flag, similarity,
                 overall_score, percentile_score, decile, persona_tier)

l2_demo_plus <- select(l2_max, Individual_ID, FamilyID, Marital_Status, 
                       Ethnic_Group, Language_Code, 
                       Religion_Code,Inferred_Age, Homeowner_Probability_Model,
                       Generations_In_HH, Purchase_Mortgage_Date,
                       Date_of_Last_Move, Moved_InOut_State, Range_of_Last_Move, Gender, 
                       Working_Woman, Length_Of_Residence_Code, Home_Purchase_Date,
                       Home_Purchase_Year, Presence_Of_CC, Estimated_Income_Code,
                       Inferred_HH_Rank)


merge <- right_join(scored, l2_demo_plus)


write.csv(merge, file = '/Users/birdieligos/Documents/Reports/allfamilies_scored_demos_clean2.csv', row.names = FALSE)


################################################### 
# single mother bucket
################################################### 
single_mother <- filter(merge, single_mother == 'TRUE')

single_mother <- single_mother %>%
  filter(Generations_In_HH != "1 Generation - 1 Adult")

single_mother <- single_mother %>%
  filter(decile != 1)

# remove HH above 3
single_mother <- single_mother %>%
  group_by(FamilyID) %>%
  filter(max(num_adults, na.rm = TRUE) <= 3) %>%
  ungroup()

single_mother <- single_mother %>%
  group_by(FamilyID) %>%
  filter(!any(persona_tier == "Unlikely")) %>%
  ungroup()

single_mother <- single_mother %>%
  mutate(
    dist_to_school = round(dist_to_school, 3),
    dist_to_enrolled = round(dist_to_enrolled, 3)
  )

single_mother <- single_mother %>%
  group_by(FamilyID) %>%
  filter(
    !(
      max(num_adults, na.rm = TRUE) >= 2 &&
        sum(Inferred_Age >= 22 & Inferred_Age <= 50, na.rm = TRUE) >= 2 &&
        any(
          outer(
            Inferred_Age[Inferred_Age >= 22 & Inferred_Age <= 50],
            Inferred_Age[Inferred_Age >= 22 & Inferred_Age <= 50],
            FUN = function(a, b) abs(a - b) <= 5 & a != b
          )
        )
    )
  ) %>%
  ungroup()
         

single_mother <- single_mother %>%
  select(
    Lookalike_Tier = persona_tier,
    Lookalike_Score = overall_score,
    Lookalike_Decile = decile,
    FamilyID,
    Individual_ID,
    Adults_in_HH = num_adults,
    Age = Inferred_Age,
    Gender = Gender,
    Language = Language_Code,
    Generations_In_HH = Generations_In_HH,
    Distance_To_School = dist_to_school,
    Distance_To_Enrolled = dist_to_enrolled
  )

# l2 contact data
l2_contact <- select(l2_max, FirstName, LastName, FamilyID, Individual_ID, Address_AddressLine, Address_ExtraAddressLine, Address_City, Address_Zip,
                     Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress)

l2_contact <- l2_contact %>%
  rename(
    First_Name     = FirstName,
    Last_Name      = LastName,
    Family_ID      = FamilyID,
    Landline_Phone = Landline_Phone_Number,
    Cell_Phone     = Cell_Phone,
    Email          = EmailAddresses_EmailAddress,
    Address_Line1  = Address_AddressLine,
    Address_Line2  = Address_ExtraAddressLine,
    City          = Address_City,
    Zip_Code       = Address_Zip
  )

l2_contact <- l2_contact %>%
  mutate(
    First_Name = str_to_title(First_Name),
    Last_Name  = str_to_title(Last_Name)
  )

# add in single moms
test <- left_join(single_mother, l2_contact)

l2_contact <- test
# back to formatting

l2_contact <- l2_contact %>%
  group_by(Family_ID) %>%
  mutate(Family_ID = paste0(paste(sort(unique(Last_Name)), collapse = "/"), " Family")) %>%
  ungroup()

l2_contact <- l2_contact %>%
  rename(Family_Name = Family_ID)

# add in neihgborhood score 
best_geo_match <- geo_match %>%
  group_by(Individual_ID) %>%
  slice_max(MATCH_CONF, n = 1, with_ties = FALSE) %>%
  ungroup()

best_geo_match <- select(best_geo_match, Individual_ID, MATCH_CONF)

best_geo_match <- best_geo_match %>%
  rename(Neighborhood_Score = MATCH_CONF)

l2_contact <- left_join(l2_contact, best_geo_match)

l2_contact <- l2_contact %>%
  group_by(Family_Name) %>%
  filter(!any(Neighborhood_Score == 1, na.rm = TRUE)) %>%
  ungroup()

l2_contact <- select(l2_contact, Lookalike_Tier, Lookalike_Decile, Lookalike_Score, Neighborhood_Score, Distance_To_School,
                     Distance_To_Enrolled, Family_Name, Adults_in_HH, First_Name, Last_Name, Age, Gender, Language,
                     Landline_Phone, Cell_Phone, Email, City, Zip_Code, Address_Line1, Address_Line2)


l2_contact <- l2_contact %>%
  arrange(
    Lookalike_Tier,
    desc(Lookalike_Decile),
    desc(Lookalike_Score),
    desc(Neighborhood_Score),
    Family_Name,
    Distance_To_School,
    Distance_To_Enrolled
  )


l2_contact <- l2_contact %>%
  mutate(Neighborhood_Score = (Neighborhood_Score * 100))

write.csv(l2_contact, file = '/Users/birdieligos/Documents/Reports/Single_Mothers.csv', row.names = FALSE)

################################################### 
# moved recently bucket
################################################### 
new_la <- merge %>%
  mutate(Date_of_Last_Move = as.Date(Date_of_Last_Move, "%Y-%m-%d"))

new_la <- new_la %>%
  filter(Date_of_Last_Move >= as.Date("2023-01-01"))

new_la <- new_la %>%
  filter(Generations_In_HH != "1 Generation - 1 Adult")

new_la <- new_la %>%
  filter(decile != 1)

new_la <- new_la %>%
  group_by(FamilyID) %>%
  filter(!any(persona_tier == "Unlikely")) %>%
  ungroup()

new_la <- new_la %>%
  mutate(
    dist_to_school   = round(dist_to_school, 3),
    dist_to_enrolled = round(dist_to_enrolled, 3)
  )


new_la <- new_la %>%
  select(
    Lookalike_Tier       = persona_tier,
    Lookalike_Score      = overall_score,
    Lookalike_Decile     = decile,
    FamilyID,
    Individual_ID,
    Adults_in_HH         = num_adults,
    Age                  = Inferred_Age,
    Gender               = Gender,
    Language             = Language_Code,
    Generations_In_HH    = Generations_In_HH,
    Distance_To_School   = dist_to_school,
    Distance_To_Enrolled = dist_to_enrolled,
    Moved_InOut_State    = Moved_InOut_State,
    single_mother = single_mother   
  )

# l2 contact data
l2_contact <- select(l2_max, FirstName, LastName, FamilyID, Individual_ID, Address_AddressLine, Address_ExtraAddressLine, Address_City, Address_Zip,
                     Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress, Inferred_HH_Rank)

l2_contact <- l2_contact %>%
  rename(
    First_Name     = FirstName,
    Last_Name      = LastName,
    Family_ID      = FamilyID,
    Landline_Phone = Landline_Phone_Number,
    Cell_Phone     = Cell_Phone,
    Email          = EmailAddresses_EmailAddress,
    Address_Line1  = Address_AddressLine,
    Address_Line2  = Address_ExtraAddressLine,
    City          = Address_City,
    Zip_Code       = Address_Zip
  )

l2_contact <- l2_contact %>%
  mutate(
    First_Name = str_to_title(First_Name),
    Last_Name  = str_to_title(Last_Name)
  )

# add in single moms
test <- left_join(new_la, l2_contact)

l2_contact <- test

# back to formatting
l2_contact <- l2_contact %>%
  group_by(Family_ID) %>%
  mutate(Family_ID = paste0(paste(sort(unique(Last_Name)), collapse = "/"), " Family")) %>%
  ungroup()

l2_contact <- l2_contact %>%
  rename(Family_Name = Family_ID)

# add in neihgborhood score 
best_geo_match <- geo_match %>%
  group_by(Individual_ID) %>%
  slice_max(MATCH_CONF, n = 1, with_ties = FALSE) %>%
  ungroup()

best_geo_match <- select(best_geo_match, Individual_ID, MATCH_CONF)

best_geo_match <- best_geo_match %>%
  rename(Neighborhood_Score = MATCH_CONF)

l2_contact <- left_join(l2_contact, best_geo_match)

l2_contact <- l2_contact %>%
  group_by(Family_Name) %>%
  filter(!any(Neighborhood_Score == 1, na.rm = TRUE)) %>%
  ungroup()

l2_contact <- select(l2_contact, Lookalike_Tier, Lookalike_Decile, Lookalike_Score, Neighborhood_Score, Distance_To_School,
                     Distance_To_Enrolled, Family_Name, Adults_in_HH, First_Name, Last_Name, Age, Gender, Language,
                     Landline_Phone, Cell_Phone, Email, City, Zip_Code, Address_Line1, Address_Line2, single_mother, Moved_InOut_State,
                     Inferred_HH_Rank)


l2_contact <- l2_contact %>%
  arrange(
    Lookalike_Tier,
    desc(Lookalike_Decile),
    desc(Lookalike_Score),
    desc(Neighborhood_Score),
    Family_Name,
    Distance_To_School,
    Distance_To_Enrolled
  )


l2_contact <- l2_contact %>%
  mutate(Neighborhood_Score = (Neighborhood_Score * 100))

#### break out subgroups
Instate <- filter(l2_contact, Moved_InOut_State == "In State")
Instate <- select(Instate, -Moved_InOut_State)
write.csv(Instate, file = '/Users/birdieligos/Documents/Reports/new_LA_instate.csv', row.names = FALSE)

outstate <- filter(l2_contact, Moved_InOut_State == "Out of State")
outstate <- select(outstate, -Moved_InOut_State)
write.csv(outstate, file = '/Users/birdieligos/Documents/Reports/new_LA_outstate.csv', row.names = FALSE)

movedsm <- filter(l2_contact, single_mother == TRUE)
movedsm <- select(movedsm, -single_mother)
write.csv(movedsm, file = '/Users/birdieligos/Documents/Reports/new_LA_singlemothers.csv', row.names = FALSE)

################################################### 
# supportive grandparents bucket
################################################### 
support_gp <- merge %>%
  mutate(senior_age_flag == TRUE)

support_gp <- support_gp %>%
  filter(Generations_In_HH != "1 Generation - 1 Adult", Generations_In_HH != "2 Generations - Adult/Child")

support_gp <- support_gp %>%
  group_by(FamilyID) %>%
  filter(!any(Gender == "Male" & Inferred_Age < 55)) %>%
  ungroup()

# remove HH above 3
support_gp <- support_gp %>%
  group_by(FamilyID) %>%
  filter(max(num_adults, na.rm = TRUE) <= 3) %>%
  ungroup()

support_gp <- support_gp %>%
  filter(decile != 1)

support_gp <- support_gp %>%
  group_by(FamilyID) %>%
  filter(!any(persona_tier == "Unlikely")) %>%
  ungroup()

support_gp <- support_gp %>%
  mutate(
    dist_to_school   = round(dist_to_school, 3),
    dist_to_enrolled = round(dist_to_enrolled, 3)
  )

support_gp <- support_gp %>%
  select(
    Lookalike_Tier       = persona_tier,
    Lookalike_Score      = overall_score,
    Lookalike_Decile     = decile,
    FamilyID,
    Individual_ID,
    Adults_in_HH         = num_adults,
    Age                  = Inferred_Age,
    Gender               = Gender,
    Language             = Language_Code,
    Generations_In_HH    = Generations_In_HH,
    Distance_To_School   = dist_to_school,
    Distance_To_Enrolled = dist_to_enrolled,
    Moved_InOut_State    = Moved_InOut_State,
    single_mother        = single_mother
  )

# l2 contact data
l2_contact <- select(l2_max, FirstName, LastName, FamilyID, Individual_ID, Address_AddressLine, Address_ExtraAddressLine, Address_City, Address_Zip,
                     Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress, Inferred_HH_Rank)

l2_contact <- l2_contact %>%
  rename(
    First_Name     = FirstName,
    Last_Name      = LastName,
    Family_ID      = FamilyID,
    Landline_Phone = Landline_Phone_Number,
    Cell_Phone     = Cell_Phone,
    Email          = EmailAddresses_EmailAddress,
    Address_Line1  = Address_AddressLine,
    Address_Line2  = Address_ExtraAddressLine,
    City           = Address_City,
    Zip_Code       = Address_Zip
  ) %>%
  mutate(
    First_Name = str_to_title(First_Name),
    Last_Name  = str_to_title(Last_Name)
  )

# add in support group
test <- left_join(support_gp, l2_contact)

l2_contact <- test

# back to formatting
l2_contact <- l2_contact %>%
  group_by(Family_ID) %>%
  mutate(Family_ID = paste0(paste(sort(unique(Last_Name)), collapse = "/"), " Family")) %>%
  ungroup()

l2_contact <- l2_contact %>%
  rename(Family_Name = Family_ID)

# add in neighborhood score 
best_geo_match <- geo_match %>%
  group_by(Individual_ID) %>%
  slice_max(MATCH_CONF, n = 1, with_ties = FALSE) %>%
  ungroup()

best_geo_match <- best_geo_match %>%
  select(Individual_ID, MATCH_CONF) %>%
  rename(Neighborhood_Score = MATCH_CONF)

l2_contact <- left_join(l2_contact, best_geo_match)

l2_contact <- l2_contact %>%
  group_by(Family_Name) %>%
  filter(!any(Neighborhood_Score == 1, na.rm = TRUE)) %>%
  ungroup()

l2_contact <- l2_contact %>%
  select(Lookalike_Tier, Lookalike_Decile, Lookalike_Score, Neighborhood_Score, Distance_To_School,
         Distance_To_Enrolled, Family_Name, Adults_in_HH, First_Name, Last_Name, Age, Gender, Language,
         Landline_Phone, Cell_Phone, Email, City, Zip_Code, Address_Line1, Address_Line2, single_mother, Inferred_HH_Rank) %>%
  arrange(
    Lookalike_Tier,
    desc(Lookalike_Decile),
    desc(Lookalike_Score),
    desc(Neighborhood_Score),
    Family_Name,
    Distance_To_School,
    Distance_To_Enrolled
  ) %>%
  mutate(Neighborhood_Score = Neighborhood_Score * 100)

support_gmagpa <- l2_contact

write.csv(support_gmagpa, file = '/Users/birdieligos/Documents/Reports/supportive_grantparents.csv', row.names = FALSE)
################################################### 
# everybody bucket
################################################### 
all <- merge

all <- all %>%
  filter(Generations_In_HH != "1 Generation - 1 Adult")

all <- all %>%
  mutate(
    dist_to_school   = round(dist_to_school, 3),
    dist_to_enrolled = round(dist_to_enrolled, 3)
  )

all <- all %>%
  select(
    Lookalike_Tier       = persona_tier,
    Lookalike_Score      = overall_score,
    Lookalike_Decile     = decile,
    FamilyID,
    Individual_ID,
    Adults_in_HH         = num_adults,
    Age                  = Inferred_Age,
    Gender               = Gender,
    Language             = Language_Code,
    Generations_In_HH    = Generations_In_HH,
    Distance_To_School   = dist_to_school,
    Distance_To_Enrolled = dist_to_enrolled,
    Moved_InOut_State    = Moved_InOut_State,
    single_mother        = single_mother
  )

# l2 contact data
l2_contact <- select(l2_max, FirstName, LastName, FamilyID, Individual_ID, Address_AddressLine, Address_ExtraAddressLine, Address_City, Address_Zip,
                     Landline_Phone_Number, Cell_Phone, EmailAddresses_EmailAddress, Inferred_HH_Rank, Religion_Code, Ethnic_Code)

l2_contact <- l2_contact %>%
  rename(
    First_Name     = FirstName,
    Last_Name      = LastName,
    Family_ID      = FamilyID,
    Landline_Phone = Landline_Phone_Number,
    Cell_Phone     = Cell_Phone,
    Email          = EmailAddresses_EmailAddress,
    Address_Line1  = Address_AddressLine,
    Address_Line2  = Address_ExtraAddressLine,
    City           = Address_City,
    Zip_Code       = Address_Zip
  ) %>%
  mutate(
    First_Name = str_to_title(First_Name),
    Last_Name  = str_to_title(Last_Name)
  )


# back to formatting
l2_contact <- l2_contact %>%
  group_by(Family_ID) %>%
  mutate(Family_ID = paste0(paste(sort(unique(Last_Name)), collapse = "/"), " Family")) %>%
  ungroup()

l2_contact <- l2_contact %>%
  rename(Family_Name = Family_ID)

# add in neighborhood score 
best_geo_match <- geo_match %>%
  group_by(Individual_ID) %>%
  slice_max(MATCH_CONF, n = 1, with_ties = FALSE) %>%
  ungroup()

best_geo_match <- best_geo_match %>%
  select(Individual_ID, MATCH_CONF) %>%
  rename(Neighborhood_Score = MATCH_CONF)

l2_contact <- left_join(l2_contact, best_geo_match)

l2_contact <- l2_contact %>%
  group_by(Family_Name) %>%
  filter(!any(Neighborhood_Score == 1, na.rm = TRUE)) %>%
  ungroup()

l2_contact <- l2_contact %>%
  select(Lookalike_Tier, Lookalike_Decile, Lookalike_Score, Neighborhood_Score, Distance_To_School,
         Distance_To_Enrolled, Family_Name, Adults_in_HH, First_Name, Last_Name, Age, Gender, Language, Religion_Code, Ethnic_Code,
         Landline_Phone, Cell_Phone, Email, City, Zip_Code, Address_Line1, Address_Line2, single_mother, Moved_InOut_State, Inferred_HH_Rank) %>%
  arrange(
    Lookalike_Tier,
    desc(Lookalike_Decile),
    desc(Lookalike_Score),
    desc(Neighborhood_Score),
    Family_Name,
    Distance_To_School,
    Distance_To_Enrolled
  ) %>%
  mutate(Neighborhood_Score = Neighborhood_Score * 100)

everyone <- left_join(all, l2_contact)

write.csv(everyone, file = '/Users/birdieligos/Documents/Reports/allL2datarankedandscored.csv', row.names = FALSE)

################################################### 
# buckets end
################################################### '
################################################### 
# buckets end
################################################### '

################################################### 
# buckets end
################################################### 







nomatch3 <- complete3_no


# Trim whitespace
nomatch3 <- nomatch3 %>%
  dplyr::mutate(
    dplyr::across(
      c(Parent.1.Name, Parent.1.Last.Name, city, Street_Name, Parent.2.Name, Parent.2.Last.Name),
      ~ stringr::str_trim(.)
    )
  )

l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    dplyr::across(
      c(FirstName, LastName, Address_City, Address_StreetName),
      ~ stringr::str_trim(.)
    )
  )

l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    dplyr::across(
      c(FirstName, LastName),
      ~ stringr::str_to_title(.)
    )
  )

nomatch3 <- nomatch3 %>%
  dplyr::mutate(
    dplyr::across(
      c(Parent.1.Name, Parent.1.Last.Name, Parent.2.Name, Parent.2.Last.Name),
      ~ stringr::str_to_title(.)
    )
  )


l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    Address_Zip       = as.character(Address_Zip),
    Address_ZipPlus4  = as.character(Address_ZipPlus4)
  )

nomatch3 <- nomatch3 %>%
  tidyr::separate(
    Apt_Num,
    into = c("Apt_Type", "Apt_Number"),
    sep = "\\s+",
    fill = "right",
    remove = TRUE
  ) %>%
  dplyr::mutate(
    Apt_Number = as.character(Apt_Number)
  )


matchplay <- nomatch3 %>%
  dplyr::left_join(
    l2max_cut2,
    by = c(
      "Parent.1.Last.Name" = "LastName",
      "Street_Name" = "Address_StreetName",
      "city" = "Address_City",
      "House_Number" = "Address_HouseNumber"
    )
  )


final1 <- matchplay
final1_yes <- final1 %>% filter(MATCH == "YES")
final1_no  <- final1 %>% filter(is.na(MATCH))
final1_no <- select(final1, -LALCONSUMERID, -Individual_ID, -MATCH)
final1_no <- final1_no %>%
  dplyr::select(-dplyr::ends_with(".y")) %>%
  dplyr::rename_with(~ stringr::str_remove(., "\\.x$"), dplyr::ends_with(".x"))





###################################################################### PEOPLE MATCHING
# FORMAT 
l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    Address_Zip       = as.character(Address_Zip),
    Address_ZipPlus4  = as.character(Address_ZipPlus4)
  )

l2max_cut2 <- l2max_cut2 %>%
  dplyr::mutate(
    dplyr::across(
      c(FirstName, LastName),
      ~ stringr::str_to_title(.)
    )
  )

l2max_cut2$Landline_Phone_Number <- trimws(l2max_cut2$Landline_Phone_Number)
l2max_cut2$Cell_Phone <- trimws(l2max_cut2$Cell_Phone)
l2max_cut2$EmailAddresses_EmailAddress <- trimws(l2max_cut2$EmailAddresses_EmailAddress)


final1_no <- merge

library(dplyr)
library(purrr)
library(tibble)
library(stringr)

# 1. x→y column mappings
var_map <- c(
  "Parent.1.Last.Name" = "LastName",
  "Parent.2.Last.Name" = "LastName",
  "House_Number"       = "Address_HouseNumber",
  "Street_Name"        = "Address_StreetName",
  "city"               = "Address_City",
  "Address_Zip"        = "Address_Zip",
  "Address_ZipPlus4"   = "Address_ZipPlus4",
  "Apt_Number"         = "Address_ApartmentNum",
  "email"              = "EmailAddresses_EmailAddress"
)

# 2. require exactly one of the two last-name keys
alt_last <- c("Parent.1.Last.Name","Parent.2.Last.Name")

# 3. build all non-empty subsets of var_map names
all_subsets <- unlist(
  lapply(seq_along(var_map),
         function(k) combn(names(var_map), k, simplify = FALSE)
  ),
  recursive = FALSE
)

# 4. keep only those subsets with exactly one alt_last
valid_subsets <- keep(all_subsets, ~ sum(.x %in% alt_last) == 1)

# 5. helper to trim whitespace on key columns
trim_df <- function(df, keys) {
  df %>% mutate(across(all_of(keys), ~ str_trim(.)))
}

final1_no  <- trim_df(final1_no,  names(var_map))
l2max_cut2 <- trim_df(l2max_cut2, unname(var_map))

# 6. build summary of every combo
summary_tbl <- map_dfr(valid_subsets, function(keys) {
  by_map <- setNames(var_map[keys], keys)
  joined <- final1_no %>%
    left_join(l2max_cut2, by = by_map, relationship = "many-to-many")
  tibble(
    combo       = paste(keys, collapse = " + "),
    n_matched   = sum(!is.na(joined$LALCONSUMERID)),
    n_unmatched = sum( is.na(joined$LALCONSUMERID)),
    total       = nrow(joined)
  )
})

# 7. produce one-row-per-match at every level
max_len <- max(map_int(valid_subsets, length))

matchplay_all <- map_dfr(valid_subsets, function(keys) {
  by_map <- setNames(var_map[keys], keys)
  final1_no %>%
    left_join(l2max_cut2, by = by_map) %>%
    # keep only actual matches
    filter(!is.na(LALCONSUMERID)) %>%
    mutate(
      MATCH_COMBO  = paste(keys, collapse = " + "),
      MATCH_LEVEL  = length(keys),
      MATCH_CONF   = length(keys) / max_len
    )
})

# === OUTPUTS ===
# - summary_tbl    : one row per combo, with match counts
# - matchplay_all  : one row per match instance, with MATCH_LEVEL & MATCH_CONF

write.csv(summary_tbl, file = '/Users/birdieligos/Documents/Reports/Enrolled_L2MatchSummary.csv', row.names = FALSE)




####
m1 <- matchall1
m1 <- select(m1, Type, Grade, DOB, Student.Age, email, Parent.1.Name, Parent.1.Last.Name, 
             Individual_ID)

m2 <- matchround2
m2 <- select(m3, Type, Grade, DOB, Student.Age, email, Parent.1.Name, Parent.1.Last.Name, 
             Individual_ID)

merge <- rbind(m1, m2)

l2_max_id <- select(l2_max, Individual_ID, FamilyID)

merge <- merge %>%
  mutate(Individual_ID = str_trim(Individual_ID))

l2_max_id <- l2_max_id %>%
  mutate(Individual_ID = str_trim(Individual_ID))

final_merge <- merge %>%
  left_join(l2_max_id, by = "Individual_ID")

m3 <- final1_yes
m3 <- select(m3, Type, Grade, DOB, Student.Age, email, Parent.1.Name, Parent.1.Last.Name, 
             Individual_ID, FamilyID)

final_bind <- rbind(final_merge, m3)

final_bind <- select(final_bind, -Individual_ID)
final_bind <- final_bind %>%
  dplyr::distinct()

forsurematches <- final_bind


############################################# MATCHING SCORE THRESHOLD
## cutting down
matchplay_low <- matchplay_all %>%
  dplyr::filter(MATCH_CONF < 0.65)

### above .65 confidence

matchplay_high <- matchplay_all %>%
  dplyr::filter(MATCH_CONF >= 0.65)


matchplay_high <- matchplay_high %>%
  dplyr::mutate(
    Parent.1.Name      = dplyr::if_else(
      is.na(Parent.1.Name) | Parent.1.Name == "",
      Parent.2.Name,
      Parent.1.Name
    ),
    Parent.1.Last.Name = dplyr::if_else(
      is.na(Parent.1.Last.Name) | Parent.1.Last.Name == "",
      Parent.2.Last.Name,
      Parent.1.Last.Name
    )
  )

matchplay_high <- matchplay_high %>%
  dplyr::distinct()
matchplay_high <- matchplay_high[, !grepl("\\.x$", names(matchplay_high))]
names(matchplay_high) <- gsub("\\.y$", "", names(matchplay_high))
names(matchplay_high) <- sub("\\.1$", "", names(matchplay_high))
matchplay_high <- matchplay_high[, !duplicated(names(matchplay_high))]



matchplay_high_full <- matchplay_high %>%
  dplyr::group_by(Type, Grade, DOB, Student.Age, email, Parent.1.Name, Parent.1.Last.Name, FamilyID) %>%
  dplyr::slice_max(MATCH_CONF, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

m4 <- select(matchplay_high_full, Type, Grade, DOB, Student.Age, email, Parent.1.Name, Parent.1.Last.Name, FamilyID)


final_bind <- rbind(final_bind, m4)

final_bind <- final_bind %>%
  dplyr::distinct()

##### final bind full L2 

enrolled_l2max <- right_join(final_bind, l2_max, by = 'FamilyID')

enrolled_l2max_match <- enrolled_l2max %>% filter(!is.na(Type))

# write
write.csv(enrolled_l2max_match, file = '/Users/birdieligos/Documents/Reports/lieklyenrolledL2.csv', row.names = FALSE)

enrolled <- select(enrolled_l2max_match, FamilyID)

enrolled_famid <- enrolled %>% 
  distinct()


### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PRETTY SURE WE DONT USE BELOW
### ### ### ### ### ### ### ### ### ### ### ### ### ### 


### geo target clean up 

geo_match_best <- geo_match %>%
  arrange(desc(MATCH_LEVEL), desc(MATCH_CONF)) %>%
  distinct(Individual_ID, .keep_all = TRUE)

print(colnames(geo_match_best))



############################################################### geo_audience

geo_audience <- select(geo_match_best, Type, Grade, DOB, Student.Age, LALCONSUMERID, FamilyID.y,
                       Individual_ID, MATCH_CONF, MATCH_COMBO, MATCH_LEVEL)


geo_audience <- geo_audience %>%
  rename(FamilyID = FamilyID.y)


geo_audience <- geo_audience %>%
  filter(!FamilyID %in% enrolled_famid$FamilyID)

geo_audience_work <- geo_audience


##### pick demo columns from L2 we carw about 

l2_demo <- select(l2_max,	Individual_ID, FamilyID,	Residence_HHParties_Description,
                  AgeRange, Childrens_General, Grandchildren_Int, Senior_Adult_In_HH, Young_Adult_In_HH, 
                  Marital_Status, Occupation_Group, Ethnic_Group, Hispanic_Country_Code,	Assimilation_Codes,
                  Language_Code,	Religion_Code,	Education_of_Person, Social_Ranking_Index_by_Individual,
                  Social_Ranking_Index_by_Area,	Likely_Income_Ranking_by_Area,	Likely_Educational_Attainment_Ranking_by_Area,
                  Homeowner_Probability_Model, CRA_Income_Classification_Code, Generations_In_HH)


geo_audiencetarget_demo <- left_join(geo_audience_work, l2_demo)


######
enrolled_demos <- select(enrolled_l2max_match, Type, Grade, DOB, Student.Age, FamilyID,
                         Individual_ID)


enrolled_demos <- left_join(enrolled_demos, l2_demo)

print(colnames(enrolled_demos))


############# demo distribution of enrolled 

library(dplyr)
library(tidyr)

# 1. define column groups
student_cols <- c("Type", "Grade", "DOB", "Student.Age")
id_cols      <- c("FamilyID", "Individual_ID")
demo_cols    <- setdiff(names(enrolled_demos), c(student_cols, id_cols))

# 2. which demo cols are character?
char_demo <- demo_cols[sapply(enrolled_demos[demo_cols], is.character)]

# 3. treat blanks as NA in character demo cols
df <- enrolled_demos %>%
  mutate(across(all_of(char_demo), ~ na_if(., "")))

# 4. Table A: missingness summary
missingness <- df %>%
  summarise(
    n_total = n(),
    across(all_of(demo_cols),
           ~ sum(!is.na(.)),
           .names = "n_filled_{col}")
  ) %>%
  pivot_longer(
    starts_with("n_filled_"),
    names_to  = "variable",
    values_to = "n_filled"
  ) %>%
  mutate(
    variable  = sub("^n_filled_", "", variable),
    n_missing = n_total - n_filled,
    pct_filled = 100 * n_filled / n_total
  ) %>%
  select(variable, n_total, n_filled, n_missing, pct_filled)

# 5. Table B: value distributions
distributions <- df %>%
  pivot_longer(
    all_of(demo_cols),
    names_to         = "variable",
    values_to        = "value",
    values_transform = list(value = as.character)
  ) %>%
  filter(!is.na(value)) %>%
  count(variable, value, name = "n") %>%
  mutate(pct = 100 * n / nrow(df))

# inspect
missingness
distributions


################# family profiles

library(dplyr)

# 1. define your fields
const_demo <- c(
  "Childrens_General",
  "Grandchildren_Int",
  "Senior_Adult_In_HH",
  "Young_Adult_In_HH",
  "Ethnic_Group",
  "Language_Code",
  "Religion_Code",
  "Homeowner_Probability_Model",
  "CRA_Income_Classification_Code",
  "Generations_In_HH"
)

all_demo <- setdiff(
  names(enrolled_demos),
  c("Type","Grade","DOB","Student.Age","FamilyID","Individual_ID")
)

var_demo <- setdiff(all_demo, const_demo)

# 2. build family profiles
family_profiles <- enrolled_demos %>%
  group_by(FamilyID) %>%
  summarise(
    # pull the one constant value (they should all match)
    across(all_of(const_demo),
           ~ unique(na.omit(.))[1],
           .names = "{col}"),
    # collapse any varying values into a list-string
    across(all_of(var_demo),
           ~ paste0(unique(na.omit(.)), collapse = ", "),
           .names = "{col}"),
    .groups = "drop"
  )

# Inspect:
family_profiles


############## family profile traits 

family_profiles_agg <- family_profiles %>%
  select(-FamilyID) %>%                   # 1. drop the identifier
  pivot_longer(                           # 2. long form: variable, value
    everything(),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  count(variable, value, name = "n_families") %>%  # 3. count per pair
  mutate(
    pct_families = 100 * n_families / nrow(family_profiles)  # 4. percent
  )

#################### family profiles

family_profiles_grouped <- family_profiles %>%
  select(-FamilyID, -Childrens_General, -Grandchildren_Int, -Young_Adult_In_HH, -CRA_Income_Classification_Code) %>%
  group_by(across(everything())) %>%
  summarise(
    n_families = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(n_families)) %>%
  mutate(
    pct_families = 100 * n_families / sum(n_families)
  )

family_profiles_grouped


