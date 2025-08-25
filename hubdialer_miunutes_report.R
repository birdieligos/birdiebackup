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


dial <- read.csv('/Users/birdieligos/Downloads/hubdialer_agent_report_mult_from_01_01_2024_through_01_01_2025.csv', stringsAsFactors = FALSE)

dial <- select(dial, Campaign.ID, Logged.In, Minutes.Logged.In)

dial$Date <- as.Date(dial$Logged.In, format="%m/%d/%Y %H:%M:%S")
dial <- dial[, !names(dial) %in% "Logged.In"]

dial$Campaign.ID <- as.character(dial$Campaign.ID)

dial <- dial %>%
  group_by(Campaign.ID, Date) %>%
  summarise(Minutes = sum(Minutes.Logged.In, na.rm = TRUE)) %>%
  ungroup()


# campaign id mapping

name <- read.csv('/Users/birdieligos/Downloads/hubdialer_dial_progress_report_mult.csv', stringsAsFactors = FALSE)


name <- select(name, Campaign.ID, Name)

name <- name[!is.na(name$Campaign.ID), ]

name$District <- case_when(
  grepl("CA41", name$Name) ~ "CA41",
  grepl("CA22", name$Name) ~ "CA22",
  grepl("CA27", name$Name) ~ "CA27",
  grepl("CA13|BallotCuring", name$Name) ~ "CA13",
  TRUE ~ NA_character_
)

name <- name[!is.na(name$District), ]

name$Campaign.ID <- as.character(name$Campaign.ID)

name <- select(name, -Name)

# merge

dial$Campaign.ID <- trimws(dial$Campaign.ID)
name$Campaign.ID <- trimws(name$Campaign.ID)

final_df <- left_join(dial, name, by = "Campaign.ID", multiple = "all")

final_df <- select(final_df, -Campaign.ID)

final_df <- final_df[!is.na(final_df$District), ]

final_df <- select(final_df, -Campaign.ID)

final_df <- final_df %>%
  group_by(Date, District) %>%
  summarise(Minutes = sum(Minutes, na.rm = TRUE)) %>%
  ungroup()


### cost

cost <- final_df
cost$Cost <- cost$Minutes * 0.081490304

# cost total

cost_total <- cost
cost_total <- select(cost_total, -Date, -Minutes)

cost_total <- cost_total %>%
  group_by(District) %>%
  summarise(Total_Cost = sum(Cost, na.rm = TRUE)) %>%
  ungroup()

cost_total <- cost_total %>%
  bind_rows(summarise(cost_total, across(where(is.numeric), sum, na.rm = TRUE), District = "Total"))

cost_total$Total_Cost <- round(cost_total$Total_Cost, 2)

write.csv(cost_total, '/Users/birdieligos/Documents/Reports/CNCelection_minutes.csv', row.names = FALSE)

write.csv(cost, '/Users/birdieligos/Documents/Reports/CNCelection_minutesdetail.csv', row.names = FALSE)


# ACTUAL DIALS

data1 <- bq_table_download("slscampaigns-364520.CNC.CA13_caller_processed2")











