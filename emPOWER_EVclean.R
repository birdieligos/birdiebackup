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
library(googlesheets4)



# 
Iserpico_EV <- read.csv('/Users/birdieligos/Downloads/Iserpico EV.csv', stringsAsFactors = FALSE)
Experian_EV <- read.csv('/Users/birdieligos/Downloads/Experian EV.csv', stringsAsFactors = FALSE)
EV_Mailer_EV <- read.csv('/Users/birdieligos/Downloads/EV Mailer EV.csv', stringsAsFactors = FALSE)
Data_Axel_EV <- read.csv('/Users/birdieligos/Downloads/Data Axel EV.csv', stringsAsFactors = FALSE)



print(colnames(Iserpico_EV))
print(colnames(Experian_EV))
print(colnames(EV_Mailer_EV))
print(colnames(Data_Axel_EV))


library(dplyr)

library(dplyr)

# 1) Iserpico
Iserpico_clean <- Iserpico_EV %>%
  rename(
    email      = EMAIL,
    first_name = FIRST,
    last_name  = LAST,
    address    = ADDRESS,
    city       = CITY,
    state      = ST,
    zip        = ZIP,
    zip4       = ZIP4,
    cell_phone = CELL,
    make       = MAKE,
    model      = MODEL,
    year       = YEAR,
    fuel_type  = FUEL
  ) %>%
  mutate(
    zip4 = as.character(zip4),
    year = as.character(year)
  ) %>%
  select(email, first_name, last_name,
         address, city, state,
         zip, zip4,
         cell_phone, make, model, year, fuel_type)

# 2) Experian
Experian_clean <- Experian_EV %>%
  rename(
    email      = EMAIL,
    first_name = First_Name,
    last_name  = Last_Name,
    address    = Address_1,
    city       = City,
    state      = State,
    zip        = Zip,
    zip4       = Zip4,
    cell_phone = Phone,
    make       = Vehicle_Make,
    model      = Vehicle_Model,
    year       = Model_Year,
    fuel_type  = Fuel_Type
  ) %>%
  mutate(
    zip4 = as.character(zip4),
    year = as.character(year)
  ) %>%
  select(email, first_name, last_name,
         address, city, state,
         zip, zip4,
         cell_phone, make, model, year, fuel_type)

# 3) EV Mailer
EV_Mailer_clean <- EV_Mailer_EV %>%
  rename(
    email      = Email,
    first_name = First,
    last_name  = Last,
    address    = Address,
    city       = City,
    state      = ST,
    zip        = Zip,
    cell_phone = Cell.Phone,
    make       = Auto.Make,
    model      = Auto.Model,
    year       = Auto.Year,
    fuel_type  = Auto.Fuel.Type
  ) %>%
  mutate(
    zip4 = NA_character_,
    year = as.character(year)
  ) %>%
  select(email, first_name, last_name,
         address, city, state,
         zip, zip4,
         cell_phone, make, model, year, fuel_type)

# 4) Data Axel
Data_Axel_clean <- Data_Axel_EV %>%
  rename(
    email      = email.address,
    first_name = first.name,
    last_name  = last.name,
    address    = address.line1,
    city       = city,
    state      = state,
    zip        = zip,
    zip4       = zip4,
    cell_phone = cell.phone
  ) %>%
  mutate(
    zip4      = as.character(zip4),
    year      = NA_character_,
    make      = NA_character_,
    model     = NA_character_,
    fuel_type = NA_character_
  ) %>%
  select(email, first_name, last_name,
         address, city, state,
         zip, zip4,
         cell_phone, make, model, year, fuel_type)

# 5) Bind all four
EV_all <- bind_rows(
  Iserpico_clean,
  Experian_clean,
  EV_Mailer_clean,
  Data_Axel_clean
)

# Check
glimpse(EV_all)

library(dplyr)

library(dplyr)

# 1. Count by City
city_counts <- Experian_EV %>%
  group_by(City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 2. Count by Zip
zip_counts <- Experian_EV %>%
  group_by(Zip) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 3. Vehicle Make counts
make_counts <- Experian_EV %>%
  group_by(Vehicle_Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 4. Vehicle Model counts
model_counts <- Experian_EV %>%
  group_by(Vehicle_Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 5. Combined Make & Model counts
make_model_counts <- Experian_EV %>%
  group_by(Vehicle_Make, Vehicle_Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 6. EV vs Hybrid (Fuel_Type)
type_counts <- Experian_EV %>%
  group_by(Fuel_Type) %>%
  summarise(count = n())

# 7. Rows with an email
email_count <- Experian_EV %>%
  summarise(email_count = sum(!is.na(EMAIL) & EMAIL != ""))

# 8. Rows with a phone number
phone_count <- Experian_EV %>%
  summarise(phone_count = sum(!is.na(Phone) & Phone != ""))

# View all results
city_counts
zip_counts
make_counts
model_counts
make_model_counts
type_counts
email_count
phone_count


# Print all rows for each summary
print(city_counts, n = Inf)
print(zip_counts, n = Inf)
print(make_counts, n = Inf)
print(model_counts, n = Inf)
print(make_model_counts, n = Inf)
print(type_counts, n = Inf)

