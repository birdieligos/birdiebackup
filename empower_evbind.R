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





# Read CSVs into data frames
Iserpico  <- read.csv("/Users/birdieligos/Downloads/Iserpico.CSV", stringsAsFactors = FALSE)
Experian   <- read.csv("/Users/birdieligos/Downloads/Experian.CSV", stringsAsFactors = FALSE)
EVMailer   <- read.csv("/Users/birdieligos/Downloads/EVMailer.CSV", stringsAsFactors = FALSE)
DataAxel   <- read.csv("/Users/birdieligos/Downloads/DataAxel.CSV", stringsAsFactors = FALSE)



Iserpico$LIST_NAME <- "Iserpico"

one <- Iserpico

one <- one %>%
  rename(Unique_ID = `Iserpico.EV_matchback`)

one <- select(one, PDIID, EMAIL, FIRST, LAST, ADDRESS, CITY, ST, ZIP,
              ZIP4, Unique_ID, MAKE, MODEL, YEAR, FUEL, CELL, LIST_NAME)



Experian$LIST_NAME <- "Experian"

two <- Experian

print(colnames(Experian))

two <- select(two, PDIID, First_Name, Last_Name, Address_1, Address_2, City, State, Zip, Zip4, Phone,
              County, Purchase_Date, Model_Year, Vehicle_Make, Vehicle_Model, Fuel_Type,
              Unique_ID, EMAIL, LIST_NAME)


EVMailer$LIST_NAME <- "EVMailer"

three <- EVMailer

print(colnames(three))


three <- select(three, PDIID, Email, First, Last, Address, Cell.Phone, City, ST, Zip, Auto.Make, Auto.Model, 
                Auto.Fuel.Type, Auto.Year, LIST_NAME)

three <- three %>%
  mutate(Unique_ID = row_number())



DataAxel$LIST_NAME <- "DataAxel"

four <- DataAxel

four <- four %>%
  rename(Unique_ID = `individual.id`)


print(colnames(four))

four <- select(four, PDIID, first.name, last.name, address.line1, city, state, zip, email.address, cell.phone, LIST_NAME)


# bind

library(dplyr)


one2 <- one %>%
  rename(
    STATE = ST
  ) %>%
  select(
    PDIID, EMAIL, FIRST, LAST, ADDRESS, CITY, STATE,
    ZIP, ZIP4, CELL, MAKE, MODEL, YEAR, FUEL,
    Unique_ID, LIST_NAME
  )


two2 <- two %>%
  rename(
    FIRST   = First_Name,
    LAST    = Last_Name,
    ADDRESS = Address_1,
    CITY    = City,
    STATE   = State,
    ZIP     = Zip,
    ZIP4    = Zip4,
    CELL    = Phone,
    MAKE    = Vehicle_Make,
    MODEL   = Vehicle_Model,
    YEAR    = Model_Year,
    FUEL    = Fuel_Type
  ) %>%
  select(
    PDIID, EMAIL, FIRST, LAST, ADDRESS, CITY, STATE,
    ZIP, ZIP4, CELL, MAKE, MODEL, YEAR, FUEL,
    Unique_ID, LIST_NAME
  )


three2 <- three %>%
  rename(
    EMAIL   = Email,
    FIRST   = First,
    LAST    = Last,
    ADDRESS = Address,
    CITY    = City,
    STATE   = ST,
    ZIP     = Zip,
    CELL    = Cell.Phone,
    MAKE    = Auto.Make,
    MODEL   = Auto.Model,
    YEAR    = Auto.Year,
    FUEL    = Auto.Fuel.Type
  ) %>%
  mutate(
    ZIP4 = NA_character_
  ) %>%
  select(
    PDIID, EMAIL, FIRST, LAST, ADDRESS, CITY, STATE,
    ZIP, ZIP4, CELL, MAKE, MODEL, YEAR, FUEL,
    Unique_ID, LIST_NAME
  )


four2 <- DataAxel %>%
  mutate(LIST_NAME="DataAxel") %>%
  rename(
    Unique_ID=individual.id,
    EMAIL=email.address,
    FIRST=first.name,
    LAST=last.name,
    ADDRESS=address.line1,
    CITY=city,
    STATE=state,
    ZIP=zip,
    CELL=cell.phone
  ) %>%
  mutate(
    ZIP4=NA_character_,
    MAKE=NA_character_,
    MODEL=NA_character_,
    YEAR=NA_integer_,
    FUEL=NA_character_
  ) %>%
  select(
    PDIID,EMAIL,FIRST,LAST,ADDRESS,CITY,STATE,
    ZIP,ZIP4,CELL,MAKE,MODEL,YEAR,FUEL,
    Unique_ID,LIST_NAME
  )

# force ZIP4 → character in one2 and two2
one2 <- one2 %>% mutate(ZIP4 = as.character(ZIP4))
two2 <- two2 %>% mutate(ZIP4 = as.character(ZIP4))

# three2 and four2 already have ZIP4 as NA_character_

combined <- bind_rows(one2, two2, three2, four2)

print(unique(combined$FUEL))

combined <- combined %>%
  group_by(PDIID) %>%
  filter(
    (is.na(PDIID) | PDIID == "") |
      row_number() == 1
  ) %>%
  ungroup()

combined <- combined %>%
  group_by(CELL) %>%
  filter((is.na(CELL) | CELL == "") | row_number() == 1) %>%
  ungroup()

combined <- combined %>%
  distinct(FIRST, LAST, ADDRESS, CITY, STATE, ZIP, ZIP4, .keep_all = TRUE)

combined <- combined %>%
  filter(ZIP %in% c(
    90001, 90002, 90011, 90022, 90023, 90040, 90058, 90059, 90061, 90063,
    90201, 90220, 90221, 90222, 90248, 90255, 90262, 90270, 90280,
    90640, 90660, 90746,
    90802, 90805, 90806, 90807, 90810, 90813,
    91754
  ))


pdi <- read.csv('/Users/birdieligos/Downloads/ll_mmall070925.csv', stringsAsFactors = FALSE)
pdi2 <- read.csv('/Users/birdieligos/Downloads/empower_subFG_070925.csv', stringsAsFactors = FALSE)

pdi_1 <- rbind(pdi, pdi2)

# Step 1: Dedupe by WIRELESSPHONENUMBER
pdi <- pdi %>% distinct(WIRELESSPHONENUMBER, .keep_all = TRUE)

# Step 2: Dedupe by PDIID
pdi <- pdi %>% distinct(V1_PDIID, .keep_all = TRUE)

pdi <- pdi_1 %>%
  mutate(
    WIRELESSPHONENUMBER = coalesce(WIRELESSPHONENUMBER, PHONENUMBER)
  ) %>%
  select(-PHONENUMBER)

pdi_clean <- select(pdi, WIRELESSPHONENUMBER, V1_PDIID, V1_PARTY, V1_GENDER, V1_ETHNICITY, V1_AGE)

pdi_clean <- pdi_clean %>%
  rename(PDIID = V1_PDIID)

combined <- combined %>%
  mutate(PDIID = str_trim(PDIID))

pdi_clean <- pdi_clean %>%
  mutate(PDIID = str_trim(PDIID))

join <- left_join(combined, pdi_clean, by = "PDIID")


join <- join %>%
  mutate(CELL = coalesce(CELL, WIRELESSPHONENUMBER))

## demos


ethnicity_map <- c(
  S  = "Latino",
  SS = "Latino",
  AS = "African American",
  O  = "Arabic",
  OO = "Arabic",
  A  = "Armenian",
  AR = "Armenian",
  E  = "East Indian",
  EE = "East Indian",
  G  = "Greek",
  GG = "Greek",
  I  = "Italian",
  II = "Italian",
  J  = "Jewish",
  JJ = "Jewish",
  H  = "Jewish Probable",
  HH = "Jewish Probable",
  D  = "Pacific Islander",
  DD = "Pacific Islander",
  B  = "Persian",
  BB = "Persian",
  P  = "Portuguese",
  PP = "Portuguese",
  R  = "Russian",
  RR = "Russian",
  M  = "AsianAnglo",
  MM = "AsianAnglo",
  C  = "Chinese",
  CC = "Chinese",
  F  = "Filipino",
  FF = "Filipino",
  N  = "Japanese",
  NN = "Japanese",
  K  = "Korean",
  KK = "Korean",
  L  = "Southeast Asian",
  LL = "Southeast Asian",
  V  = "Vietnamese",
  VV = "Vietnamese",
  W  = "Chinese / Korean",
  WW = "Chinese / Korean",
  Z  = "Chinese / Vietnamese",
  ZZ = "Chinese / Vietnamese",
  U  = "Chinese / Korean / Vietnamese",
  UU = "Chinese / Korean / Vietnamese"
)

join <- join %>%
  mutate(
    # first treat "Unknown" as NA on the character level
    V1_AGE = na_if(as.character(V1_AGE), "Unknown"),
    # then convert to integer
    V1_AGE = as.integer(V1_AGE),
    AGE_BUCKET = cut(
      V1_AGE,
      breaks = c(18, 30, 45, 60, 75, Inf),
      labels = c("18-30", "30-45", "45-60", "60-75", "75+"),
      right = FALSE,
      include.lowest = TRUE
    )
  )

print(colnames(join))

evlist_clean <- join


write.csv(evlist_clean, file = '/Users/birdieligos/Documents/Reports/emPOWER_EVlistclean_070925.csv', row.names = FALSE)


