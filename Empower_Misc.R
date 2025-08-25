library(bigrquery)
library(curl)
library(dplyr)
library(lubridate)
library(httpuv)
library(base)
library(googledrive)
library(stringr)
library(tidyr)
library(tibble)
unloadNamespace("plyr")

sf <- read.csv('/Users/birdieligos/Downloads/Qualified Customer Data-2024-12-03-18-34-46.csv', stringsAsFactors = FALSE)

# looking to purchase
buy <- filter(
  sf,
  (is.na(Vehicle.Purchase.Date) | Vehicle.Purchase.Date == "") &
    (is.na(Vehicle.New.or.Used) | Vehicle.New.or.Used == "") &
    !is.na(Status) &
    !Status %in% c("Vehicle Purchased", "Rejected")
)
# outreach pipeline 

print(colnames(sf))

phone <- sf %>% select(SCE.Rebate.Level, Phone) 

phone <- phone %>%
  mutate(across(everything(), ~ str_squish(str_remove_all(.x, "[^[:alnum:]\\s]"))))

phone$Qualified <- 1

# hubdialer

data <- bq_table_download("slstrategy.EmPower.Empower_Calls_2024", bigint = "integer64")

data <- filter(data, data$Are_you_interested_in_regstering_for_an_upcoming_in_person_workshop_ == 'TRUE')

data <- select(data, Phone_Number)
data$Pipline_Count <- 1 

data <- data %>%
  rename(Phone = Phone_Number) %>%
  mutate(Phone = str_trim(Phone))

# merge 

merge <- left_join(phone, data)

merge <- merge %>%
  mutate(Pipline_Count = replace_na(as.integer(Pipline_Count), 0))

print(sum(merge$Pipline_Count))

print(sum(merge$Qualified))

# attendance data 

attended <- read.csv('/Users/birdieligos/Downloads/report1733286526181.csv', stringsAsFactors = FALSE)

attended <- attended %>% filter(Phone != "")

attended <- attended %>%
  filter(!is.na(Phone) & Phone != "") %>%
  mutate(Phone = str_squish(str_remove_all(Phone, "[^[:alnum:]\\s]")))

attended$Attended <- 1

merge2 <- right_join(merge, attended, by = "Phone")

merge2 <- merge2 %>%
  mutate(Pipline_Count = replace_na(as.integer(Pipline_Count), 0))

print(sum(merge2$Pipline_Count))

## did not attend but qualified 

data <- bq_table_download("slstrategy.EmPower.Empower_Calls_2024", bigint = "integer64")

data <- filter(data, data$Are_you_a_SoCal_Edison_Customer_ == 'Yes')
data <- filter(data, data$Do_you_live_in_South_Gate_or_a_surrounding_city_ == 'Yes')
data <- filter(
  data,
  !is.na(Have_you_purchased_a_plug_in_hybrid_vehicle_or_are_you_looking_to_purchase_a_new_vehicle_) &
    Have_you_purchased_a_plug_in_hybrid_vehicle_or_are_you_looking_to_purchase_a_new_vehicle_ != "No/Refuse"
)

data <- select(data, Phone_Number)
data$Pipline_Count <- 1 

data <- data %>%
  rename(Phone = Phone_Number) %>%
  mutate(Phone = str_trim(Phone))





