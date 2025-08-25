# Load required libraries
library(googledrive)
library(dplyr)
library(lubridate)
library(bigrquery)
library(stringr)
library(tidyr)
library(readxl)


# text data
report <- read.csv('/Users/birdieligos/Downloads/exportresult_export_36665687162308544161f7294ba_0_results (1).csv', stringsAsFactors = FALSE)
report <- report %>%
  rename(phone_number = recipient_number)
report <- select(report, agent, agent, phone_number, send_date, campaign_name)
report <- report %>%
  mutate(contact = sub("^\\+","", phone_number)) %>%
  select(-phone_number)

# contact list
contact_la <- read.csv('/Users/birdieligos/Downloads/contact_list_7214e6e63689060804147021059.csv', stringsAsFactors = FALSE)
print(colnames(contact_la))

# temp fix
contact_la <- contact_la %>%
  mutate(contact = if_else(!is.na(mobile) & mobile != "", mobile, contact))
contact_la <- contact_la %>%
  filter(contact != "2014529051")
#

contact_la <- select(contact_la, first_name, last_name, contact, address, city, zipcode, tags, 
                     pdi_id, Gender)
contact_la <- contact_la %>%
  mutate(contact = as.character(contact))

# Trim whitespace on the join column in both tables
report    <- report    %>% mutate(contact = trimws(contact))
contact_la <- contact_la %>% mutate(contact = trimws(contact))

# Perform the join
join <- left_join(report, contact_la, by = "contact")

# 1. Store original tags
join <- join %>% 
  mutate(original_tags = tags)

work <- join

# 2. Blank out tags except on the row with the most recent starting_date
# join <- join %>%
#   group_by(pdi_id) %>%
#   mutate(
#     tags = if_else(
#       send_date == max(send_date, na.rm = TRUE),
#       tags,
#       ""
#     )
#   ) %>%
#   ungroup()

# # 3. Identify rows where tags were changed from non-blank to blank
# changed_rows <- join %>%
#   filter(original_tags != "" & tags == "")
# 
# # 4. Print those rows (or just their row numbers)
# print(changed_rows)
# # Or, to print row indices:
# print(which(join$original_tags != "" & join$tags == ""))
# 
# # work
# work <- join
# 
# work <- work %>%
#   mutate(tags = replace_na(tags, "")) %>%
#   rowwise() %>%
#   mutate(
#     parts  = list(str_trim(str_split(tags, ",")[[1]])),
#     Pledge_ID = paste(parts[str_detect(parts, "^(Yes|No)\\b")], collapse = ","),
#     No_Reason = paste(parts[str_detect(parts, "^DNSU")], collapse = ","),
#     AI_tag = paste(parts[!str_detect(parts, "^(Yes|No)\\b") & !str_detect(parts, "^DNSU")], collapse = ",")
#   ) %>%
#   ungroup() %>%
#   mutate(across(c(Pledge_ID, No_Reason, AI_tag), ~ na_if(.x, ""))) %>%
#   select(-parts)
# 
# work <- work %>%
#   mutate(
#     Pledge_ID = if_else(
#       replace_na(str_detect(No_Reason, "^DNSU"), FALSE),
#       "No - Did not sign up",
#       Pledge_ID
#     )
#   )

work <- work %>%
  separate(send_date, into = c("Date", "Time"), sep = " ") %>%
  mutate(Date = as.Date(Date)) 

# work <- work %>%
#   mutate(
#     AGE = case_when(
#       AGE >= 18  & AGE <= 30 ~ "18-30",
#       AGE >  30  & AGE <= 45 ~ "30-45",
#       AGE >  45  & AGE <= 60 ~ "45-60",
#       AGE >  60            ~ "60+",
#       TRUE                  ~ "Unknown"
#     )
#   )

# ethnicity 
# work <- work %>%
#   mutate(
#     ETHNICITY = case_when(
#       ETHNICITY %in% c("S","SS")           ~ "Latino",
#       ETHNICITY == "AS"                   ~ "African American",
#       ETHNICITY %in% c("O","OO")          ~ "Arabic",
#       ETHNICITY %in% c("A","AR")          ~ "Armenian",
#       ETHNICITY %in% c("E","EE")          ~ "East Indian",
#       ETHNICITY %in% c("G","GG")          ~ "Greek",
#       ETHNICITY %in% c("I","II")          ~ "Italian",
#       ETHNICITY %in% c("J","JJ")          ~ "Jewish",
#       ETHNICITY %in% c("H","HH")          ~ "Jewish Probable",
#       ETHNICITY %in% c("D","DD")          ~ "Pacific Islander",
#       ETHNICITY %in% c("B","BB")          ~ "Persian",
#       ETHNICITY %in% c("P","PP")          ~ "Portuguese",
#       ETHNICITY %in% c("R","RR")          ~ "Russian",
#       ETHNICITY %in% c("M","MM")          ~ "AsianAnglo",
#       ETHNICITY %in% c("C","CC")          ~ "Chinese",
#       ETHNICITY %in% c("F","FF")          ~ "Filipino",
#       ETHNICITY %in% c("N","NN")          ~ "Japanese",
#       ETHNICITY %in% c("K","KK")          ~ "Korean",
#       ETHNICITY %in% c("L","LL")          ~ "Southeast Asian",
#       ETHNICITY %in% c("V","VV")          ~ "Vietnamese",
#       ETHNICITY %in% c("W","WW")          ~ "Chinese / Korean",
#       ETHNICITY %in% c("Z","ZZ")          ~ "Chinese / Vietnamese",
#       ETHNICITY %in% c("U","UU")          ~ "Chinese / Korean / Vietnamese",
#       TRUE                                 ~ NA_character_
#     )
#   )

work <- select(work, -original_tags)


work <- work %>%
  # drop the “CA – ” prefix
  mutate(full = str_remove(city, "^CA –\\s*")) %>%
  # extract “City Of Long Beach” as city
  mutate(
    city = str_extract(full, ".*(?= Council District)"),
    city_council_district = full
  ) %>%
  select(-full)

work <- work %>%
  mutate(
    city                      = str_remove(city,               "^CA\\s*[–-]\\s*"),
    city_council_district     = str_remove(city_council_district, "^CA\\s*[–-]\\s*")
  )

work <- work %>%
  mutate(
    CONTRACT = if_else(str_detect(address, "Long Beach"), "Long Beach Contract", NA_character_)
  )

work$tags <- NA_character_
work$agent <- NA_character_
work <- work %>%
  mutate(contact = as.character(contact))

# Upload to BigQuery
bq_table <- bq_table(project = "liberty-hill-462819", dataset = "CallHubV1", table = "InitialDashSource")
bq_table_upload(bq_table, work, write_disposition = "WRITE_TRUNCATE")


print(unique(work$disposition))

