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


# when reading a file, this makes sure that strings remain characters instead of becoming factors
options(stringsAsFactors = FALSE)

# Load the 'oes' data from BigQuery
oes <- bq_table_download('slstrategy.Oakland_Military.Oakland_Military_Calls_022025', bigint = "integer64")

oes <- oes[oes$Time >= hms::as_hms("13:00:00") & oes$Time <= hms::as_hms("16:59:59"), ]

oes$Contacts <- ifelse(oes$Status %in% c("Do Not Call", "Human", "Language Barrier", 
                                         "Not Home", "Refused/Hung Up", "Wrong Number", 
                                         "Deceased"), 1, 0)

report <- select(oes, Agent, Date, Contacts)

report$Dials <- 1

report <- report %>%
  group_by(Agent, Date) %>%
  summarize(Dials = sum(Dials), Contacts = sum(Contacts), .groups = "drop")

report$DialperDay <- report$Dials/4
report$ContactsperDay <- report$Contacts/4

print(mean(report$Dials))
print(mean(report$Contacts))


print(unique(oes$Status))

report <- filter(oes, Application_Status %in% c('Undecided/Would Like A Tour or More Information', 'Already Applied', 'Yes/Applied Over the Phone'))

write.csv(report, file = '/Users/birdieligos/Documents/Reports/OMI_Report_031825.csv', row.names = FALSE)
# strike list 
strike <- filter(oes, Status %in% c('Do Not Call', 'Language Barrier', 'Wrong Number', 'Disconnected', 'Moved'))

strike2 <- filter(oes, Application_Status %in% c('Yes/Applied Over the Phone', 'No/Not Interested', 'Already Applied'))


strikelist <- rbind(strike, strike2)

write.csv(strikelist, file = '/Users/birdieligos/Documents/Reports/OMI_STRIKE_022225.csv', row.names = FALSE)

#oes <- filter(oes, Status == 'Language Barrier')

print(unique(oes$Application_Status))

report <- filter(oes, Application_Status == 'Undecided/Would Like A Tour or More Information')

write.csv(report, file = '/Users/birdieligos/Documents/Reports/oaklandMAYBE.csv', row.names = FALSE)


# new digital

digital <- read.csv('/Users/birdieligos/Downloads/newleadsyah.csv', stringsAsFactors = FALSE)


digital$Phone.. <- gsub("[^0-9]", "", digital$Phone..) 
digital$Phone.. <- sub("1$", "", digital$Phone..)
digital$Phone.. <- sub("^1+", "", digital$Phone..)


write.csv(digital, file = '/Users/birdieligos/Documents/Reports/final_digital2_022825.csv', row.names = FALSE)


