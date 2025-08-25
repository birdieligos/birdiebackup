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


############################################################# 
# gsheet connection for session + POC data
############################################################# 
ats <- read_sheet("https://docs.google.com/spreadsheets/d/1BDtaS_-TtZC-NBDpgr4SzmaLCYFYgZgMbP4Y_CWNK5g/edit?gid=0#gid=0")
ats_work <- select(ats, Date)

ats_work <- ats_work %>%
  rename(DATE = Date)

ats_work <- ats_work %>%
  mutate(
    DATE = as.Date(DATE)
  )

ats_work$COUNT <- 1

ats_work$KPI <- "# of Sessions"

# uber poc 
uber <- ats

uber <- uber %>%
  select(Date, matches("Uber"))


uber_long <- uber %>%
  pivot_longer(
    cols = contains("Uber"),
    names_to = "POC_Level",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))

uber_long <- select(uber_long, -POC_Level)

uber_long$KPI <- "POCs Involved"

uber_long$COUNT <- 1

############################################################# 
# feedback tracker connection
############################################################# 
feedback <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1tG4X4eTVX0yD_TLTuGUvhBKxAzvFJpuAmFBV6BvJVcI",
  sheet = "Feedback"
)

feedback <- select(feedback, Date)

feedback <- feedback %>%
  rename(DATE = Date)

feedback <- feedback %>%
  mutate(
    DATE = as.Date(DATE)
  )

feedback$COUNT <- 1

feedback$KPI <- "Insights Collected"

feedback <- feedback %>%
  mutate(
    DATE = if_else(
      DATE >= as.Date("2024-04-01") & DATE <= as.Date("2024-06-30"),
      DATE %m-% months(3),
      DATE
    )
  )

feedback <- feedback %>%
  mutate(
    DATE = if_else(
      DATE >= as.Date("2025-04-01") & DATE <= as.Date("2025-06-30"),
      DATE %m-% months(3),
      DATE
    )
  )

############################################################# 
# earner datasheet
############################################################# 
earner <- bq_table_download("slstrategy.Uber.Earner_Data")
earn_work <- earner

earn_work <- select(earn_work, Date, EVENT_GROUP_R, uuid, Attended, RSVP)

earn_work <- filter(earn_work, EVENT_GROUP_R %in% c("Applicants", "Town Hall", "Office Hours", "Crew Members"))

# Trim UUIDs
earn_work <- earn_work %>%
  mutate(uuid = str_trim(uuid))

# test 

# Join and compute Re_election_Interest
earn_work <- earn_work %>%
  arrange(uuid, Date) %>%
  group_by(uuid) %>%
  mutate(
    last_crew_date = if_else(EVENT_GROUP_R == "Crew Members", Date, as.Date(NA))
  ) %>%
  fill(last_crew_date, .direction = "down") %>%
  mutate(
    Re_election_Interest = case_when(
      EVENT_GROUP_R == "Applicants" & !is.na(last_crew_date) & Date > last_crew_date ~ 1,
      EVENT_GROUP_R == "Applicants"                                             ~ 0,
      TRUE                                                                       ~ 0
    )
  ) %>%
  ungroup() %>%
  select(-last_crew_date)

library(dplyr)
library(tidyr)

############################################################# 
# MERGE RE ELECTION AND RSVP, ATTENDED
#################################################################

kpi_extra <- earn_work

# Step 1: Base metrics
metrics_by_date <- kpi_extra %>%
  group_by(Date, EVENT_GROUP_R) %>%
  summarise(
    RSVP      = sum(RSVP, na.rm = TRUE),
    Attended = sum(Attended, na.rm = TRUE),
    Re_Election_Interest = sum(Re_election_Interest, na.rm = TRUE),
    .groups = "drop"
  )

thoh <- metrics_by_date %>%
  filter(str_detect(EVENT_GROUP_R, "Office Hours|Town Hall|Applicants"))


#33 appplicants/voter side side hurry split 
app <- metrics_by_date

app <- app %>%
  filter(EVENT_GROUP_R %in% c("Applicants", "Crew Members"))

metrics_by_date <- select(app, Date, Attended, EVENT_GROUP_R, Re_Election_Interest)

metrics_by_date <- metrics_by_date %>%
  filter(EVENT_GROUP_R != "Crew Members")

app <- metrics_by_date
### appplicants/voter side side hurry split 

# Step 1: Base metrics
thoh <- thoh %>%
  group_by(Date) %>%
  mutate(
    `Office Hours RSVP`       = if_else(EVENT_GROUP_R == "Office Hours", RSVP, 0),
    `Office Hour Attendance`  = if_else(EVENT_GROUP_R == "Office Hours", Attended, 0),
    `Town Hall RSVP`          = if_else(EVENT_GROUP_R == "Town Hall", RSVP, 0),
    `Town Hall Attendance`    = if_else(EVENT_GROUP_R == "Town Hall", Attended, 0),
    `Crew Applications`       = if_else(EVENT_GROUP_R == "Applicants", Attended, 0)
  ) %>%
  ungroup()




# re election calc
calc <- app

calc <- select(app, Date, Re_Election_Interest)

calc <- calc %>%
  filter(Re_Election_Interest != 0)

re_calc_long <- calc %>%
  pivot_longer(
    cols = c(`Re_Election_Interest`),
    names_to = "KPI",
    values_to = "COUNT"
  )

re_calc_long <- re_calc_long %>%
  rename(DATE = Date)

re_calc_long$Value <- NA_character_

thoh_work <- select(thoh, -Attended, -RSVP, -EVENT_GROUP_R, -Re_Election_Interest)



metrics_by_date <- thoh_work %>%
  rename(DATE = Date)

metrics_by_date <- metrics_by_date %>%
  mutate(
    `Total External Program Engagement` =
      `Office Hours RSVP` + `Town Hall RSVP` + `Crew Applications`
  )

metrics_long <- metrics_by_date %>%
  pivot_longer(
    cols = c(`Office Hours RSVP`, `Office Hour Attendance`,
             `Town Hall RSVP`, `Town Hall Attendance`, `Crew Applications`,
             `Total External Program Engagement`),
    names_to = "KPI",
    values_to = "COUNT"
  )

done_1 <- metrics_long

uber_long <- uber_long %>%
  rename(DATE = Date)

done <- done_1

done$Value <- NA_character_

done <- rbind(uber_long, done, re_calc_long)

done <- done %>%
  mutate(DATE = as.Date(DATE))

done <- done %>%
  filter(COUNT != 0)

# feedback
feedback_grab <- feedback
feedback_grab$Value <- NA_character_

done <- rbind(done, feedback_grab)

done <- done %>%
  mutate(Quarter = paste0("Q", quarter(DATE), " ", year(DATE)))

# re-election interest
calc <- read_sheet("https://docs.google.com/spreadsheets/d/1C9Fu7gOo3qrJKR2COPZdFjXkWFSfXfMULo18Sp5HADs/edit?gid=0#gid=0")

calc <- calc %>%
  mutate(
    `End Date` = as.character(`End Date`),
    `End Date` = if_else(
      `End Date` == "Active",
      today(),
      as_datetime(as.numeric(`End Date`), origin = "1970-01-01") %>% as_date()
    )
  )


calc <- calc %>%
  mutate(
    `Start Date` = as_date(`Start Date`)
  )

cm <- select(calc, UUID, `Start Date`, `End Date`)

reapps <- cm %>%
  mutate(
    yrs = floor(time_length(interval(`Start Date`, `End Date`), "years"))
  ) %>%
  filter(yrs >= 1) %>%
  rowwise() %>%
  mutate(
    Date = list(`Start Date` + years(seq_len(yrs)))
  ) %>%
  unnest(Date) %>%
  ungroup() %>%
  transmute(
    Date,
    COUNT = 1L,
    UUID
  )

reapps$KPI <- 'Re-Election Interest'

reapps <- reapps %>%
  mutate(Quarter = paste0("Q", quarter(Date), " ", year(Date)))

reapps <- reapps %>%
  rename(Value = UUID)

reapps <- reapps %>%
  rename(DATE = Date)


done <- done %>% filter(KPI != "Re_Election_Interest")

rbind <- rbind(reapps, done)

done <- rbind
#
done_keep <- done

done <- select(done, -DATE, -Value)

done <- done %>%
  filter(!is.na(Quarter) & Quarter != "QNA NA")

done_summary <- done %>%
  group_by(KPI, Quarter) %>%
  summarise(COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop")

done <- done_summary

done <- done %>%
  group_by(KPI) %>%
  mutate(Baseline = mean(COUNT, na.rm = TRUE)) %>%
  ungroup()

done <- done %>%
  mutate(YEAR = str_extract(Quarter, "\\d{4}"))


############################################################# 
# QoQ 
#############################################################
# done <- done %>%
#   group_by(KPI) %>%
#   arrange(KPI, YEAR, Quarter) %>%
#   mutate(
#     QoQ = round((COUNT / lag(COUNT, 1) - 1), 2)
#   ) %>%
#   ungroup()
# 
# 
# 
# done <- done %>%
#   mutate(
#     Year_Num = as.integer(str_extract(Quarter, "\\d{4}$")),
#     Qtr_Num  = as.integer(str_extract(Quarter, "(?<=Q)[1-4]"))
#   ) %>%
#   group_by(KPI) %>%
#   arrange(KPI, Year_Num, Qtr_Num, .by_group = TRUE) %>%
#   mutate(
#     YTD_Current   = map2_dbl(Year_Num, Qtr_Num, ~ sum(COUNT[Year_Num == .x    & Qtr_Num <= .y], na.rm = TRUE)),
#     YTD_Prev      = map2_dbl(Year_Num, Qtr_Num, ~ sum(COUNT[Year_Num == .x - 1 & Qtr_Num <= .y], na.rm = TRUE)),
#     YoY_QTD       = round((YTD_Current / YTD_Prev - 1) * 100, 2),
#     
#     YTD_Year      = map_dbl(Year_Num, ~ sum(COUNT[Year_Num == .x], na.rm = TRUE)),
#     YTD_Year_Prev = map_dbl(Year_Num, ~ sum(COUNT[Year_Num == .x - 1], na.rm = TRUE)),
#     YoY_Year      = round((YTD_Year / YTD_Year_Prev - 1) * 100, 2)
#   ) %>%
#   ungroup() %>%
#   select(-Year_Num, -Qtr_Num, -YTD_Current, -YTD_Prev, -YTD_Year, -YTD_Year_Prev)
# 
# done <- done %>%
#   mutate(across(
#     c(YoY_QTD, YoY_Year),
#     ~ replace(., is.infinite(.), NA_real_)
#   ))

##### above is dynamic YoY and QoQ, saving for later
##### below is hard coded most recenet YoY and QoQ

#QoQ
done_work <- done

recent_two_wide <- done_work %>%
  mutate(
    Year_Num   = as.integer(str_extract(Quarter, "\\d{4}$")),
    Qtr_Num    = as.integer(str_extract(Quarter, "(?<=Q)[1-4]")),
    Quarter_ID = Year_Num + (Qtr_Num - 1) / 4
  ) %>%
  group_by(KPI) %>%
  slice_max(Quarter_ID, n = 2, with_ties = FALSE) %>%
  arrange(KPI, desc(Quarter_ID)) %>%
  mutate(Period = c("Current", "Previous")) %>%
  ungroup() %>%
  pivot_wider(
    id_cols     = KPI,
    names_from  = Period,
    values_from = c(Quarter, COUNT),
    names_glue  = "{Period}_{.value}"
  ) %>%
  select(KPI, Current_Quarter, Previous_Quarter, Current_COUNT, Previous_COUNT)


recent_two_wide <- recent_two_wide %>%
  mutate(
    QoQ = (Current_COUNT - Previous_COUNT) / Previous_COUNT
  )

qoq_clean <- select(recent_two_wide, KPI, QoQ)
############################################################# 
# YoY
#############################################################
library(dplyr)
library(lubridate)
library(tidyr)

year_work <- done_keep

recent_yoy <- year_work %>%
  mutate(Year = year(DATE)) %>%
  group_by(KPI, Year) %>%
  summarise(Annual_COUNT = sum(COUNT, na.rm = TRUE), .groups = "drop") %>%
  group_by(KPI) %>%
  slice_max(order_by = Year, n = 2, with_ties = FALSE) %>%
  arrange(KPI, desc(Year)) %>%
  mutate(Period = c("Current", "Previous")) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = KPI,
    names_from = Period,
    values_from = Annual_COUNT,
    names_glue = "{Period}_COUNT"
  ) %>%
  mutate(
    YoY_Year = (Current_COUNT - Previous_COUNT) / Previous_COUNT
  ) %>%
  select(KPI, Current_COUNT, Previous_COUNT, YoY_Year)

yoy_clean <- select(recent_yoy, KPI, YoY_Year)
#############################################################
# add yoy and qoq clean 
done_add <- done

done_add <- left_join(done_add, yoy_clean)

done_add <- left_join(done_add, qoq_clean)
###### final filters

done <- done_add %>%
  mutate(
    Quarter = if_else(
      Quarter == "Q2 2024" & str_detect(KPI, "Office Hours"),
      "Q1 2024",
      Quarter
    )
  )

done <- done %>%
  mutate(
    Quarter = if_else(
      Quarter == "Q3 2021" & str_detect(KPI, "Crew"),
      "Q2 2021",
      Quarter
    )
  )

done <- done %>%
  mutate(
    Quarter = if_else(
      Quarter == "Q2 2024" & str_detect(KPI, "Office"),
      "Q1 2024",
      Quarter
    )
  )

done <- done %>%
  mutate(
    KPI = if_else(
      KPI == "Re_Election_Interest",
      "Re-Election Interest",
      KPI
    )
  )

############################################################# 
# Create BigQuery table
#############################################################
project_id <- "slstrategy"
dataset_id <- "UBER_2025"
table_id <- "KPI_TRACKER_V1"

# Create a table reference
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists
if (bq_table_exists(table_ref)) {
  bq_table_delete(table_ref)
}

# Define the schema based on `joined`
schema <- list(
  bq_field("COUNT", "INT64"), 
  bq_field("KPI", "STRING"),
  bq_field("Value", "STRING"),
  bq_field("Quarter", "STRING"),
  bq_field("YEAR", "STRING"),
  bq_field("QoQ", "FLOAT64"),
  bq_field("YoY_QTD", "FLOAT64"),
  bq_field("YoY_Year", "FLOAT64"),
  bq_field("Baseline", "FLOAT64")
)


# Upload the data
tryCatch({
  bq_table_create(table_ref, fields = schema)
  bq_table_upload(table_ref, values = done)
  cat("Table created and data uploaded successfully!\n")
}, error = function(e) {
  cat("An error occurred:", conditionMessage(e), "\n")
})


