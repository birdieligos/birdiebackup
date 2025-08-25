

test <- read.csv('/Users/birdieligos/Downloads/log_107910_y8nxi4qenqbqyu8d89ej3znu0veldgn2_2025-04-13_03_10_39.csv', stringsAsFactors = FALSE)
library(dplyr)

dup_summary <- test %>%
  group_by(Number.Dialed) %>%
  summarize(Count = n(), .groups = "drop") %>%
  filter(Count > 1)

print(dup_summary)

library(dplyr)

test <- test %>% distinct(Number.Dialed, .keep_all = TRUE)



print(length(unique(test$Number.Dialed)))


ws <- read.csv('/Users/birdieligos/Downloads/event-1091148538-invitees-export.csv', stringsAsFactors = FALSE)

colnames(ws)[7] <- "phone"

head(ws$phone)

ws$phone <- gsub("[^0-9]", "", ws$phone)

ws$phone <- gsub("^1", "", ws$phone)

ws <- select(ws, First.Name, Last.Name, phone)

call <- read.csv('/Users/birdieligos/Downloads/2025_04_14_campaign_9982_empwoerqc.csv', stringsAsFactors = FALSE)

call$phone <- gsub("^1", "", call$phone)

call <- select(call, phone, user_first_name, user_last_name)

call <- call[call$user_first_name != "" & !is.na(call$user_first_name), ]

call$ORIGINAL_CALLER <- tools::toTitleCase(
  paste(call$user_first_name, call$user_last_name)
)

call <- select(call, -user_first_name, -user_last_name)

result <- left_join(ws, call)

write.csv(result, file = '/Users/birdieligos/Documents/Reports/EMPOWERQC041425.csv', row.names = FALSE)

# 

call <- read.csv('/Users/birdieligos/Downloads/EMPOWER_CANVASS_ENGLISH_THIRDPASS041425.csv', stringsAsFactors = FALSE)

call$phone <- gsub("^1", "", call$phone)

write.csv(call, file = '/Users/birdieligos/Documents/Reports/fuckkkk.csv', row.names = FALSE)




