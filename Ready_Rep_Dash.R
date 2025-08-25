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
library(googlesheets4)
                

# CNC PDI productivity report
cnc <- read.csv('/Users/birdieligos/Downloads/readyrep050725.csv', stringsAsFactors = FALSE)

# replace '...' with '.'
colnames(cnc) <- gsub("\\.+", ".", colnames(cnc))

# replace '.' with '_'
colnames(cnc) <- gsub("\\.", "_", colnames(cnc))

# format DATE field
cnc$DATE <- as.Date(cnc$DATE, format = "%m/%d/%Y")


#These questions are now in data set
#instead of hard code -- run an ifelse
cnc$Attend_Event_Y <- cnc$WOULD_YOU_BE_INTERESTED_IN_ATTENDING_AN_EVENT_YES
cnc$Attend_Event_N <- cnc$WOULD_YOU_BE_INTERESTED_IN_ATTENDING_AN_EVENT_NO
cnc$Attend_Event_Maybe <- cnc$WOULD_YOU_BE_INTERESTED_IN_ATTENDING_AN_EVENT_MAYBE
cnc$Ready_Rep_Y <- cnc$CAN_WE_INTEREST_YOU_IN_JOINING_US_AS_A_READY_REP_YES
cnc$Ready_Rep_N <- cnc$CAN_WE_INTEREST_YOU_IN_JOINING_US_AS_A_READY_REP_NO
cnc$Ready_Rep_Maybe <- cnc$CAN_WE_INTEREST_YOU_IN_JOINING_US_AS_A_READY_REP_MAYBE

# Define the columns to keep
columns_to_keep <- c("PROJECTNAME", 
                     "CANVASSER", 
                     "DATE", 
                     "TOTALVOTERS", 
                     "DOORSKNOCKED", 
                     "CONTACTS", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU", 
                     "Attend_Event_Y",
                     "Attend_Event_N",
                     "Attend_Event_Maybe",
                     "Ready_Rep_Y",
                     "Ready_Rep_N",
                     "Ready_Rep_Maybe",
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", 
                     "NON_CONTACT_MOBILE_NH", 
                     "NON_CONTACT_MOBILE_MV", 
                     "NON_CONTACT_MOBILE_REF", 
                     "NON_CONTACT_MOBILE_GTD", 
                     "NON_CONTACT_MOBILE_D")

# Select and keep only these columns in the dataframe
cnc <- cnc[, columns_to_keep]

print(unique(cnc$PROJECTNAME))

cnc$PROJECTNAME <- dplyr::recode(cnc$PROJECTNAME,
                                 '24_09_AACC_Ready_Rep_Walk' = 'African American Chamber of Commerce of the San Joaquin Valley',
                                 '24_10_LUCC_Ready_Rep_94509_Canvass' = 'Lift Up Contra Costa',
                                 '24_11_OCCET_92707_92705_Walk_ReadyRep' = 'OCCET',
                                 'OCCETT Deep Canvass' = 'OCCET Deep Canvass',
                                 '24_10_Project_Joy_Ready_Rep_Walk' = 'Project Joy',
                                 '24_11_BGC_Ready_Rep_Walk_95340_95341_94348' = 'Boys and Girls Club of Merced County',
                                 '25_01_805UndocuFund_Ready_Rep_Walk' = '805 Undocufund',
                                 '24_11_FMBCC_Ready_Rep_93721_93706_93705' = 'Move the Valley',
                                 '25_03_ready_rep_jakara_deep_canvass' = 'Move the Valley Deep Canvass',
                                 '24_12_ready_rep_jakara_walk' = 'Move the Valley',
                                 '25_02_Ready_Rep_InnerCityStruggle_Walk' = 'Inner City Struggle',
                                 '25_01_Ready_Rep_COCO_Walk' = 'COCO LA',
                                 'LUCC_DeepCanvass_English_20241210' = 'Lift Up Contra Costa Deep Canvass', 
                                 '03_25_LUCC_Ready_Rep_94520_94565_Walk' = 'Lift Up Contra Costa'
)


#################################### alliance SD
# ALLIANCE SD PDI productivity report
summercanva <- bq_table_download('slstrategy.Ready_Rep.AllianceSD_Canvass24')

asd <- read.csv('/Users/birdieligos/Downloads/asdrr022425.csv', stringsAsFactors = FALSE)

summercanva$PROJECTNAME <- ifelse(
  is.na(summercanva$PROJECTNAME), 
  "Alliance SD",  
  ifelse(grepl("Deep Canvass", summercanva$PROJECTNAME, ignore.case = TRUE), 
         "Alliance SD Deep Canvass", 
         "Alliance SD")
)

asd$PROJECTNAME <- ifelse(
  is.na(asd$PROJECTNAME), 
  "Alliance SD", 
  ifelse(grepl("Deep Canvass", asd$PROJECTNAME, ignore.case = TRUE), 
         "Alliance SD Deep Canvass", 
         "Alliance SD")
)

# replace '...' with '.'
colnames(asd) <- gsub("\\.+", ".", colnames(asd))

# replace '.' with '_'
colnames(asd) <- gsub("\\.", "_", colnames(asd))

# format DATE field
asd$DATE <- as.Date(asd$DATE, format = "%m/%d/%Y")

colnames(summercanva) <- gsub("_{2,}", "_", colnames(summercanva))


print(colnames(asd))
print(colnames(summercanva))
# split up questions
asd$Attend_Event_Y <- asd$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_Y_KIT
asd$Attend_Event_N <- asd$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_N_KIT

asd$Ready_Rep_Y <- asd$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_Y_KIT
asd$Ready_Rep_N <- asd$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_N_KIT

asd$Attend_Event_Maybe <- 0
asd$Ready_Rep_Maybe <- 0

## rename columns 
names(asd)[names(asd) == "NON_CONTACT_DISPOSITION_CODES_NOT_HOME"] <- "NON_CONTACT_MOBILE_NH"
names(asd)[names(asd) == "NON_CONTACT_DISPOSITION_CODES_MOVED"] <- "NON_CONTACT_MOBILE_MV"
names(asd)[names(asd) == "NON_CONTACT_DISPOSITION_CODES_REFUSED"] <- "NON_CONTACT_MOBILE_REF"
names(asd)[names(asd) == "NON_CONTACT_DISPOSITION_CODES_LANGUAGE"] <- "NON_CONTACT_MOBILE_D"
names(asd)[names(asd) == "NON_CONTACT_DISPOSITION_CODES_GATED"] <- "NON_CONTACT_MOBILE_GTD"
names(asd)[names(asd) == "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_Y"] <- "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES"
names(asd)[names(asd) == "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_N"] <- "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO"


# Define the columns to keep
columns_to_keep <- c("PROJECTNAME", 
                     "CANVASSER", 
                     "DATE", 
                     "TOTALVOTERS", 
                     "DOORSKNOCKED", 
                     "CONTACTS", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU", 
                     "Attend_Event_Y",
                     "Attend_Event_N",
                     "Attend_Event_Maybe",
                     "Ready_Rep_Y",
                     "Ready_Rep_N",
                     "Ready_Rep_Maybe",
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", 
                     "NON_CONTACT_MOBILE_NH", 
                     "NON_CONTACT_MOBILE_MV", 
                     "NON_CONTACT_MOBILE_REF", 
                     "NON_CONTACT_MOBILE_GTD", 
                     "NON_CONTACT_MOBILE_D")

# Select and keep only these columns in the dataframe
asd <- asd[, columns_to_keep]

## rename columns 
names(summercanva)[names(summercanva) == "NON_CONTACT_DISPOSITION_CODES_NOT_HOME"] <- "NON_CONTACT_MOBILE_NH"
names(summercanva)[names(summercanva) == "NON_CONTACT_DISPOSITION_CODES_MOVED"] <- "NON_CONTACT_MOBILE_MV"
names(summercanva)[names(summercanva) == "NON_CONTACT_DISPOSITION_CODES_REFUSED"] <- "NON_CONTACT_MOBILE_REF"
names(summercanva)[names(summercanva) == "NON_CONTACT_DISPOSITION_CODES_LANGUAGE"] <- "NON_CONTACT_MOBILE_D"
names(summercanva)[names(summercanva) == "NON_CONTACT_DISPOSITION_CODES_GATED"] <- "NON_CONTACT_MOBILE_GTD"
names(summercanva)[names(summercanva) == "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_Y"] <- "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES"
names(summercanva)[names(summercanva) == "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_N"] <- "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO"

# split up questions
summercanva$Attend_Event_Y <- summercanva$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_Y_KIT
summercanva$Attend_Event_N <- summercanva$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_N_KIT

summercanva$Ready_Rep_Y <- summercanva$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_Y_KIT
summercanva$Ready_Rep_N <- summercanva$ARE_YOU_INTERESTED_IN_KEEPING_IN_TOUCH_MULTIPLE_N_KIT

summercanva$Attend_Event_Maybe <- 0
summercanva$Ready_Rep_Maybe <- 0

# Select and keep only these columns in the dataframe
summercanva <- summercanva[, columns_to_keep]

test <- rbind(asd, summercanva)

########################################### CROWD CANVASS

cc <- read_sheet("https://docs.google.com/spreadsheets/d/1brrMGLcVTcX7VJ2KNWY3iUzAczkENAZtv2EATcRvHyk/edit#gid=1466879854")

# Rename the column
colnames(cc)[colnames(cc) == "Timestamp"] <- "DATE"

# Convert the renamed column to Date format (assuming the format is "mm/dd/yyyy")
cc$DATE <- as.Date(cc$DATE, format = "%m/%d/%Y")

# replace '...' with '.'
colnames(cc) <- gsub("\\.+", ".", colnames(cc))

# replace '.' with '_'
colnames(cc) <- gsub("\\.", "_", colnames(cc))

library(janitor)

cc <- cc %>%
  rename_with(~ gsub("[^A-Za-z0-9]+", "_", .x) %>%  # Replace special characters with _
                gsub("_+", "_", .) %>%  # Replace multiple _ with a single _
                gsub(" ", "", .))  # Remove spaces
# split up questions
# alerts
cc$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES <- ifelse(cc$Can_we_sign_you_up_for_the_Local_Emergency_Alert_Notifications_https_www_listoscalifornia_org_alerts_ == "Yes", 1, 0)
cc$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U <- ifelse(cc$Can_we_sign_you_up_for_the_Local_Emergency_Alert_Notifications_https_www_listoscalifornia_org_alerts_ == "Undecided", 1, 0)
cc$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO <- ifelse(cc$Can_we_sign_you_up_for_the_Local_Emergency_Alert_Notifications_https_www_listoscalifornia_org_alerts_ == "NO", 1, 0)
cc$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU <- ifelse(cc$Can_we_sign_you_up_for_the_Local_Emergency_Alert_Notifications_https_www_listoscalifornia_org_alerts_ == "I'm already signed up", 1, 0)

# event attend
cc$Attend_Event_Y <- ifelse(cc$We_are_also_looking_for_residents_neighbors_who_are_interested_in_attending_upcoming_events_on_emergency_preparedness_to_learn_how_to_better_prepare_you_and_your_family_in_case_of_an_emergency_Would_you_be_interested_in_attending_an_event_ == "Yes", 1, 0)
cc$Attend_Event_N <- ifelse(cc$We_are_also_looking_for_residents_neighbors_who_are_interested_in_attending_upcoming_events_on_emergency_preparedness_to_learn_how_to_better_prepare_you_and_your_family_in_case_of_an_emergency_Would_you_be_interested_in_attending_an_event_ == "No", 1, 0)
cc$Attend_Event_Maybe <- ifelse(cc$We_are_also_looking_for_residents_neighbors_who_are_interested_in_attending_upcoming_events_on_emergency_preparedness_to_learn_how_to_better_prepare_you_and_your_family_in_case_of_an_emergency_Would_you_be_interested_in_attending_an_event_ == "Maybe", 1, 0)
cc$Attend_Event_Y[is.na(cc$Attend_Event_Y)] <- 0
cc$Attend_Event_N[is.na(cc$Attend_Event_N)] <- 0
cc$Attend_Event_Maybe[is.na(cc$Attend_Event_Maybe)] <- 0


# ready rep interest
cc$Ready_Rep_Y <- ifelse(cc$Can_we_interest_you_in_joining_us_as_a_Ready_REP_it_s_like_a_neighborhood_captain_who_would_ensure_your_neighbors_your_family_and_friends_are_also_prepared_for_an_emergency_ == "Yes", 1, 0)
cc$Ready_Rep_N <- ifelse(cc$Can_we_interest_you_in_joining_us_as_a_Ready_REP_it_s_like_a_neighborhood_captain_who_would_ensure_your_neighbors_your_family_and_friends_are_also_prepared_for_an_emergency_ == "No", 1, 0)
cc$Ready_Rep_Maybe <- ifelse(cc$Can_we_interest_you_in_joining_us_as_a_Ready_REP_it_s_like_a_neighborhood_captain_who_would_ensure_your_neighbors_your_family_and_friends_are_also_prepared_for_an_emergency_ == "Maybe", 1, 0)

# concerns
# remove whitespace and special chr
cc$`_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_` <- gsub(
  "[^[:alnum:] ]", "", 
  trimws(cc$`_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_`)
)

# ifelse
cc$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN <- ifelse(
  cc$'_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_' == "Dont want additional notifications", 
  1, 
  0
)

cc$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG <- ifelse(
  cc$'_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_' == "Doesnt want to be contacted by the Government", 
  1, 
  0
)

cc$WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE <- ifelse(
  cc$'_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_' == "Thinks they are prepared enough", 
  1, 
  0
)

cc$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU <- ifelse(
  cc$'_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_' == "Dont find them useful", 
  1, 
  0
)

cc$WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD <- ifelse(
  cc$'_IF_NO_I_completely_understand_Can_you_tell_me_what_some_of_your_concerns_are_' == "Wont Disclose", 
  1, 
  0
)


# add non contact dummy columns 
cc$NON_CONTACT_MOBILE_NH <- 0
cc$NON_CONTACT_MOBILE_MV <- 0
cc$NON_CONTACT_MOBILE_REF <- 0
cc$NON_CONTACT_MOBILE_D <- 0
cc$NON_CONTACT_MOBILE_GTD <- 0

# add columns 
cc$PROJECTNAME <- paste(cc$Your_Organization)
cc$TOTALVOTERS <- 1
cc$DOORSKNOCKED <- 1
cc$CONTACTS <- 1


## rename columns 
cc$CANVASSER <- cc$Organization_Contact_Representative_Name

# Define the columns to keep
columns_to_keep <- c("PROJECTNAME", 
                     "CANVASSER", 
                     "DATE", 
                     "TOTALVOTERS", 
                     "DOORSKNOCKED", 
                     "CONTACTS", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO", 
                     "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU", 
                     "Attend_Event_Y",
                     "Attend_Event_N",
                     "Attend_Event_Maybe",
                     "Ready_Rep_Y",
                     "Ready_Rep_N",
                     "Ready_Rep_Maybe",
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", 
                     "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", 
                     "NON_CONTACT_MOBILE_NH", 
                     "NON_CONTACT_MOBILE_MV", 
                     "NON_CONTACT_MOBILE_REF", 
                     "NON_CONTACT_MOBILE_GTD", 
                     "NON_CONTACT_MOBILE_D")

# Select and keep only these columns in the dataframe
cc <- cc[, columns_to_keep]

cc$TYPE <- 'CROWD CANVASS'


#Change name of CultivaLA
cc$PROJECTNAME[cc$PROJECTNAME == "Cultiva"] <- "CultivaLA"
###################################################READY REP POST EVENT SURVEY
### POST JAN 6TH 2025
# NEW
new <- read.csv('/Users/birdieligos/Downloads/CSV 36/OES Ready Rep Post Event Survey.csv', stringsAsFactors = FALSE)


#
#
#
#
#
#
###### do not TOUCH
donottouch <- read.csv('/Users/birdieligos/Downloads/CSV 23/OES Ready Rep Post Event Survey.csv', stringsAsFactors = FALSE)



rrpe <- rbind(new, donottouch)

#Project name
mapping <- data.frame(
  Collector.ID = c(457536228, 457536567, 457536562, 457536569, 457536571, 
                   457536609, 457536601, 457536610, 457536614, 457536617, 
                   457536631, 457536628, 457536619, 457536642, 457536635, 
                   457536639, 457537681),
  PROJECTNAME = c("Inland Empire United", "Move the Valley", 
                  "Alliance San Diego", "Lift Up Contra Costa (Civic Engagement Table)", 
                  "Central Coast Alliance United for a Sustainable Economy (CAUSE)", 
                  "805 Undocufund", "OCCET", 
                  "Boys and Girls Club of Merced County", 
                  "African American Chamber of Commerce of the San Joaquin Valley", 
                  "LA Voice", "CultivaLA", "Project Joy", "ACCE", 
                  "Ventures", "Parent Engagement Academy", 
                  "InnerCity Struggle", "Community Coalition")
)

# join mapping to rrpe by collector id
rrpe <- rrpe %>%
  left_join(mapping, by = "Collector.ID")

# report 
report <- filter(rrpe, PROJECTNAME %in% c('OCCET', 'Lift Up Contra Costa (Civic Engagement Table)', 'Move the Valley', 'Ventures'))

write.csv(report, file = '/Users/birdieligos/Documents/Reports/Yelitza_Org_PostSurvey.csv', row.names = FALSE)

#"CANVASSER"
rrpe$CANVASSER <- "Event Survey"

#DATE
rrpe$DATE <- as.Date(rrpe$End.Date, format = "%m/%d/%Y")

test <- select(rrpe, PROJECTNAME)
#TOTALVOTERS
rrpe$TOTALVOTERS <- 1


#"DOORSKNOCKED"
rrpe$DOORSKNOCKED <- 1

#CONTACTS
rrpe$CONTACTS <- 1

#CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES


rrpe <- rrpe %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES = ifelse(
    `Did.we.sign.you.up.for.the.Local.Emergency.Alert.Notifications.today..https...www.listoscalifornia.org.alerts.` == "Yes" |
      `X.Lo.inscribimos.para.recibir.notificaciones.de.alerta.de.emergencia.local..hoy..https...www.listoscalifornia.org.alerts.` == "Sí", 
    1, 
    0
  ))


##   "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U" 

rrpe$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U <- 0


#CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO

rrpe <- rrpe %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO = ifelse(
    `Did.we.sign.you.up.for.the.Local.Emergency.Alert.Notifications.today..https...www.listoscalifornia.org.alerts.` == "No" |
      `X.Lo.inscribimos.para.recibir.notificaciones.de.alerta.de.emergencia.local..hoy..https...www.listoscalifornia.org.alerts.` == "No", 
    1, 
    0
  ))


#CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU

rrpe <- rrpe %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU = ifelse(
    `Did.we.sign.you.up.for.the.Local.Emergency.Alert.Notifications.today..https...www.listoscalifornia.org.alerts.` == "I’m Already Signed Up" |
      `X.Lo.inscribimos.para.recibir.notificaciones.de.alerta.de.emergencia.local..hoy..https...www.listoscalifornia.org.alerts.` == "Ya estoy registrado", 
    1, 
    0
  ))


#Attend_Event_Y
rrpe <- rrpe %>%
  mutate(Attend_Event_Y = ifelse(
    `We.are.also.looking.for.residents..neighbors..who.are.interested.in.attending.upcoming.events.on.emergency.preparedness.to.learn.how.to.better.prepare.you.and.your.family.in.case.of.an.emergency..would.you.be.interested.in.attending.an.event.` == "Yes" |
      `También.estamos.buscando.residentes.de.la.comunida..vecinos.que.estan.interesados...en.asistir.proximos.eventos.sobre.como.preparce.para.emergencias.y.aprender.cómo.usted.y.su.familia.pueden.preparce.en.caso.de.una.emergencia...Estaría.interesado.en.asistir.a.un.evento.` == "Sí", 
    1, 
    0
  ))


#Attend_Event_N
rrpe <- rrpe %>%
  mutate(Attend_Event_N = ifelse(
    `We.are.also.looking.for.residents..neighbors..who.are.interested.in.attending.upcoming.events.on.emergency.preparedness.to.learn.how.to.better.prepare.you.and.your.family.in.case.of.an.emergency..would.you.be.interested.in.attending.an.event.` == "No" |
      `También.estamos.buscando.residentes.de.la.comunida..vecinos.que.estan.interesados...en.asistir.proximos.eventos.sobre.como.preparce.para.emergencias.y.aprender.cómo.usted.y.su.familia.pueden.preparce.en.caso.de.una.emergencia...Estaría.interesado.en.asistir.a.un.evento.` == "No", 
    1, 
    0
  ))


#Attend_Event_Maybe
rrpe <- rrpe %>%
  mutate(Attend_Event_Maybe = ifelse(
    `We.are.also.looking.for.residents..neighbors..who.are.interested.in.attending.upcoming.events.on.emergency.preparedness.to.learn.how.to.better.prepare.you.and.your.family.in.case.of.an.emergency..would.you.be.interested.in.attending.an.event.` == "Maybe" |
      `También.estamos.buscando.residentes.de.la.comunida..vecinos.que.estan.interesados...en.asistir.proximos.eventos.sobre.como.preparce.para.emergencias.y.aprender.cómo.usted.y.su.familia.pueden.preparce.en.caso.de.una.emergencia...Estaría.interesado.en.asistir.a.un.evento.` == "Tal vez", 
    1, 
    0
  ))


#Ready_Rep_Y

rrpe <- rrpe %>%
  mutate(Ready_Rep_Y = ifelse(
    `Can.we.interest.you.in.joining.us.as.a..Ready.REP....it.s.like.a.neighborhood.captain..who.would.ensure.your.neighbors..your.family..and.friends.are.also.prepared.for.an.emergency.` == "Yes" |
      `X.Podemos.interesarle.en.unirse.con.nosotros.como.un..Ready.REP...es.como.un.capitán.de.vecindario..que.se.aseguraría.de.que.sus.vecinos..su.familia.y.sus.amigos.también.estén.preparados.para.una.emergencia.` == "Sí", 
    1, 
    0
  ))

#   "Ready_Rep_N",

rrpe <- rrpe %>%
  mutate(Ready_Rep_N = ifelse(
    `Can.we.interest.you.in.joining.us.as.a..Ready.REP....it.s.like.a.neighborhood.captain..who.would.ensure.your.neighbors..your.family..and.friends.are.also.prepared.for.an.emergency.` == "No" |
      `X.Podemos.interesarle.en.unirse.con.nosotros.como.un..Ready.REP...es.como.un.capitán.de.vecindario..que.se.aseguraría.de.que.sus.vecinos..su.familia.y.sus.amigos.también.estén.preparados.para.una.emergencia.` == "No", 
    1, 
    0
  ))


#   "Ready_Rep_Maybe",
rrpe <- rrpe %>%
  mutate(Ready_Rep_Maybe = ifelse(
    `Can.we.interest.you.in.joining.us.as.a..Ready.REP....it.s.like.a.neighborhood.captain..who.would.ensure.your.neighbors..your.family..and.friends.are.also.prepared.for.an.emergency.` == "Maybe" |
      `X.Podemos.interesarle.en.unirse.con.nosotros.como.un..Ready.REP...es.como.un.capitán.de.vecindario..que.se.aseguraría.de.que.sus.vecinos..su.familia.y.sus.amigos.también.estén.preparados.para.una.emergencia.` == "Tal vez", 
    1, 
    0
  ))


#   "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 

rrpe <- rrpe %>%
  mutate(WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN = ifelse(
    `I.completely.understand..Can.you.tell.me.what.some.of.your.concerns.are.` == "Don’t want additional notifications" |
      `Entiendo.completamente...Puede.decirme.cuáles.son.algunas.de.sus.preocupaciones.` == "No quiero notificaciones adicionales", 
    1, 
    0
  ))


#   "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG", 
rrpe <- rrpe %>%
  mutate(WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG = ifelse(
    `I.completely.understand..Can.you.tell.me.what.some.of.your.concerns.are.` == "Don’t want to be contacted by the Government" |
      `Entiendo.completamente...Puede.decirme.cuáles.son.algunas.de.sus.preocupaciones.` == "No quiero que el gobierno me contacte", 
    1, 
    0
  ))



#   "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE",
rrpe <- rrpe %>%
  mutate(WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE = ifelse(
    `I.completely.understand..Can.you.tell.me.what.some.of.your.concerns.are.` == "I think that I am prepared enough" |
      `Entiendo.completamente...Puede.decirme.cuáles.son.algunas.de.sus.preocupaciones.` == "Creo que estoy lo suficientemente preparado", 
    1, 
    0
  ))

#   "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", 
rrpe <- rrpe %>%
  mutate(WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU = ifelse(
    `I.completely.understand..Can.you.tell.me.what.some.of.your.concerns.are.` == "I don’t find them useful" |
      `Entiendo.completamente...Puede.decirme.cuáles.son.algunas.de.sus.preocupaciones.` == "No los encuentro útiles", 
    1, 
    0
  ))

#   "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", 

rrpe <- rrpe %>%
  mutate(WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD = ifelse(
    `I.completely.understand..Can.you.tell.me.what.some.of.your.concerns.are.` == "Other" |
      `Entiendo.completamente...Puede.decirme.cuáles.son.algunas.de.sus.preocupaciones.` == "Otro", 
    1, 
    0
  ))

#   "NON_CONTACT_MOBILE_NH", 
rrpe$NON_CONTACT_MOBILE_NH <- 0


#   "NON_CONTACT_MOBILE_MV", 
rrpe$NON_CONTACT_MOBILE_MV <- 0

#   "NON_CONTACT_MOBILE_REF", 
rrpe$NON_CONTACT_MOBILE_REF <- 0

#   "NON_CONTACT_MOBILE_GTD", 
rrpe$NON_CONTACT_MOBILE_GTD <- 0

#   "NON_CONTACT_MOBILE_D"
rrpe$NON_CONTACT_MOBILE_D <- 0


# "TYPE"
rrpe <- rrpe %>%
  mutate(TYPE = case_when(
    `What.type.of.event.did.you.attend.today.` == "Summit" |
      `X.A.qué.tipo.de.evento.asististio.hoy.` == "Conferencia?" ~ "Summit",
    
    `What.type.of.event.did.you.attend.today.` == "Workshop" |
      `X.A.qué.tipo.de.evento.asististio.hoy.` == "Taller" ~ "Workshop",
    
    `What.type.of.event.did.you.attend.today.` == "House Meeting" |
      `X.A.qué.tipo.de.evento.asististio.hoy.` == "Reunión de casa" ~ "House Meeting",
    
    TRUE ~ ""  # Default case: leave blank
  ))


##fix missing workshops
rrpe <- rrpe %>%
  mutate(TYPE = ifelse(Respondent.ID %in% c(118708355620, 118713685150, 118713745975, 118713814221, 118717959897, 118717984493, 118718000833,
                                            118737604601, 118737602460, 118737602449, 118737602442, 118737602328, 118737602223, 118737395993,
                                            118736896595, 118736891179, 118736870084), "Workshop Post Event Survey", TYPE))



rrpe <- rrpe %>%
  mutate(TYPE = ifelse(Respondent.ID %in% c(118751378588,  
                                            118751378140), "House Meeting Post Event Survey", TYPE))


rrpe <- rrpe %>%
  mutate(TYPE = ifelse(Respondent.ID %in% c(118753722695, 118751223600, 118764138982, 118764701263,
                                            118751220896, 118764027198, 118764125342, 118764136294), "Summit Post Event Survey", TYPE))


#remove bad data
rrpe <- rrpe %>%
  filter(!(Respondent.ID %in% c(118757591824, 118760222367, 118761284157, 118764658987, 118768628814, 118769089420, 118761283923, 118762267299, 118762051275, 118762789256, 118762852349, 118719520910, 118719519860, 118711697122, 118706907495, 118700673600, 118681413793, 118725756081,
                                118730214091, 118734213385, 118744781519, 118742957699, 118750289777, 118751266894,
                                118751379135, 118751380519, 118751389418, 118751396349, 118778433932, 118778409121, 118778401715,
                                118778118273, 118778085011)))

# remove NA collector ID
rrpe <- rrpe[!is.na(rrpe$Collector.ID), ]
rrpe <- rrpe[!is.na(rrpe$TYPE) & rrpe$TYPE != "", ]
#####RRPE TYPE CHECK

test <- select(rrpe, Respondent.ID, Collector.ID,  What.type.of.event.did.you.attend.today., X.A.qué.tipo.de.evento.asististio.hoy., TYPE, Date.of.Event., Start.Date)
test <- filter(test, TYPE == "")


################
# Check if any TYPE is empty
if (any(rrpe$TYPE == "")) {
  # Get the Respondent.IDs where TYPE is empty
  empty_ids <- rrpe$Respondent.ID[rrpe$TYPE == ""]
  
  # Stop the script and report the IDs
  stop("The script stopped because the following Respondent.IDs have an empty TYPE: ", 
       paste(empty_ids, collapse = ", "))
}



###RRPE TYPE CHECK

#keep only the columns needed
rrpe <- rrpe %>%
  select(59:85)

#rrpe <- rrpe1

# Create a temporary dataframe with 12 rows and the same columns as rrpe, filled with NA
new_rows <- as.data.frame(matrix(NA, nrow = 12, ncol = ncol(rrpe)))
colnames(new_rows) <- colnames(rrpe)

# Set the specific values for the "CANVASSER" and "TYPE" columns
new_rows$CANVASSER <- "Event Survey"
new_rows$TYPE <- "Summit Post Event Survey"
new_rows$PROJECTNAME <- "Project Joy"
new_rows$DATE[1:6] <- as.Date('2024-10-15')
new_rows$DATE[7:12] <- as.Date('2024-10-16')
new_rows$TOTALVOTERS <- 0
new_rows$DOORSKNOCKED <- 1
new_rows$CONTACTS <- 0
new_rows[, 7:26] <- 0


# Bind the new rows to the bottom of rrpe
rrpe <- rbind(rrpe, new_rows)


######################################################ready rep pre event survey
rrpree <- read.csv('/Users/birdieligos/Downloads/CSV 37/OES Ready Rep Pre Event Survey.csv', stringsAsFactors = FALSE)

#"PROJECTNAME", 
# create a mapping table for collector id and projectname
mapping <- data.frame(
  Collector.ID = c(457671816, 457671838, 457671843, 457671847, 457671858, 
                   457671867, 457671872, 457671879, 457671886, 457671891, 
                   457671900, 457671905, 457671925, 457671928, 457671937, 
                   457671945, 457671951),
  PROJECTNAME = c("Inland Empire United", "Alliance San Diego", "Move the Valley", 
                  "Lift Up Contra Costa", 
                  "Central Coast Alliance United for a Sustainable Economy", 
                  "OCCET", "805 Undocufund", 
                  "Boys and Girls Club of Merced County", 
                  "African American Chamber of Commerce of the San Joaquin Valley", 
                  "LA Voice", "ACCE", "Community Coalition", "Project Joy", 
                  "CultivaLA", "Parent Engagement Academy", "InnerCity Struggle", 
                  "Ventures")
)

# join the mapping table to rrpree
rrpree <- rrpree %>%
  left_join(mapping, by = "Collector.ID")

# report 
report <- rrpree

report <- filter(rrpe, PROJECTNAME %in% c('OCCET', 'Lift Up Contra Costa (Civic Engagement Table)', 'Move the Valley', 'Ventures'))

write.csv(report, file = '/Users/birdieligos/Documents/Reports/Yelitza_Org_PreSurvey.csv', row.names = FALSE)

# "CANVASSER", 
rrpree$CANVASSER <- "Event Survey"

# "DATE", 
rrpree$DATE <- as.Date(rrpree$End.Date, format = "%m/%d/%Y")


# "TOTALVOTERS", 
rrpree$TOTALVOTERS <- 1


# "DOORSKNOCKED", 
rrpree$DOORSKNOCKED <- 1

# "CONTACTS", 
rrpree$CONTACTS <- 1


# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES", 
rrpree$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES <- 0

# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U", 
rrpree$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U <- 0

# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO",
rrpree$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO <- 0

# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU",
rrpree$CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU <- 0

# "Attend_Event_Y",
rrpree$Attend_Event_Y <- 0

# "Attend_Event_N",
rrpree$Attend_Event_N <- 0

# "Attend_Event_Maybe",
rrpree$Attend_Event_Maybe <- 0

# "Ready_Rep_Y",
rrpree$Ready_Rep_Y <- 0

# "Ready_Rep_N",
rrpree$Ready_Rep_N <- 0

# "Ready_Rep_Maybe",
rrpree$Ready_Rep_Maybe <- 0

# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 
rrpree$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN <- 0

# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG",
rrpree$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG <- 0

# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE", 
rrpree$WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE <- 0

# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", 
rrpree$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU <- 0

# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", 
rrpree$WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD <- 0

# "NON_CONTACT_MOBILE_NH", 
rrpree$NON_CONTACT_MOBILE_NH <- 0

# "NON_CONTACT_MOBILE_MV", 
rrpree$NON_CONTACT_MOBILE_MV <- 0

# "NON_CONTACT_MOBILE_REF", 
rrpree$NON_CONTACT_MOBILE_REF <- 0

# "NON_CONTACT_MOBILE_GTD", 
rrpree$NON_CONTACT_MOBILE_GTD <- 0

# "NON_CONTACT_MOBILE_D"
rrpree$NON_CONTACT_MOBILE_D <- 0


#TYPE
rrpree$TYPE <- "Summit"


###Filter out bad respondents

rrpree <- rrpree %>%
  filter(!(Respondent.ID %in% c(118742956061, 118744723345, 118748765916 )))

# filter out rows where collector id is na
rrpree <- rrpree %>%
  filter(!is.na(Collector.ID))
#filter
rrpree <- rrpree %>%
  select(19:45)

################ IE UNITED

ieu <- read.csv('/Users/birdieligos/Documents/Reports/IEUnitedorgRawData.csv', stringsAsFactors = FALSE)

#project name

ieu$PROJECTNAME <- "Inland Empire United"

# "CANVASSER"

ieu$CANVASSER <- ieu$Organization

# "DATE"
ieu$DATE <- ymd_hms(ieu$creationDate.GMT., tz = "UTC")
ieu$DATE <- as.Date(ieu$DATE, format = "%m/%d/%Y")


#TOTALVOTERS
ieu$TOTALVOTERS <- 1


#"DOORSKNOCKED"
ieu$DOORSKNOCKED <- 1

#CONTACTS
ieu$CONTACTS <- 1

# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES"
ieu <- ieu %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES = ifelse(
    Sign.up.Local.Emergency.Notifications.Regístrate.para.Notificaciones.Locales.de.Emergencias. == "Yes/Si", 1, 0
  ))



# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U"
ieu <- ieu %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U = ifelse(
    Sign.up.Local.Emergency.Notifications.Regístrate.para.Notificaciones.Locales.de.Emergencias. == "Undecided/Indeciso", 1, 0
  ))


# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO"
ieu <- ieu %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO = ifelse(
    Sign.up.Local.Emergency.Notifications.Regístrate.para.Notificaciones.Locales.de.Emergencias. == "No", 1, 0
  ))


# "CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU"
ieu <- ieu %>%
  mutate(CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU = ifelse(
    Sign.up.Local.Emergency.Notifications.Regístrate.para.Notificaciones.Locales.de.Emergencias. == "I'm Already Signed Up/Ya estoy inscrito", 1, 0
  ))


# "Attend_Event_Y"
ieu <- ieu %>%
  mutate(Attend_Event_Y= ifelse(
    Attend.Event.Asistir.al.evento. == "Yes/Si", 1, 0
  ))

# "Attend_Event_N"
ieu <- ieu %>%
  mutate(Attend_Event_N= ifelse(
    Attend.Event.Asistir.al.evento. == "No", 1, 0
  ))


# "Attend_Event_Maybe"
ieu <- ieu %>%
  mutate(Attend_Event_Maybe= ifelse(
    Attend.Event.Asistir.al.evento. == "Maybe/Indeciso", 1, 0
  ))


# "Ready_Rep_Y"
ieu <- ieu %>%
  mutate(Ready_Rep_Y= ifelse(
    Join..Ready.REP..Unirte.a..Ready.REP.. == "Yes/Si", 1, 0
  ))



# "Ready_Rep_N"
ieu <- ieu %>%
  mutate(Ready_Rep_N= ifelse(
    Join..Ready.REP..Unirte.a..Ready.REP.. == "No", 1, 0
  ))


# "Ready_Rep_Maybe"
ieu <- ieu %>%
  mutate(Ready_Rep_Maybe= ifelse(
    Join..Ready.REP..Unirte.a..Ready.REP.. == "Maybe/Indeciso", 1, 0
  ))


# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", 
ieu$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN <- 0
# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG"
ieu$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG <- 0
# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE"
ieu$WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE <- 0
# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU"
ieu$WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU <- 0
# "WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD"
ieu$WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD <- 0
# "NON_CONTACT_MOBILE_NH"
ieu$NON_CONTACT_MOBILE_NH <- 0
# "NON_CONTACT_MOBILE_MV" 
ieu$NON_CONTACT_MOBILE_MV <- 0
# "NON_CONTACT_MOBILE_REF" 
ieu$NON_CONTACT_MOBILE_REF <- 0
# "NON_CONTACT_MOBILE_GTD" 
ieu$NON_CONTACT_MOBILE_GTD <- 0
# "NON_CONTACT_MOBILE_D"
ieu$NON_CONTACT_MOBILE_D <- 0
#TYPE
ieu$TYPE <- "CROWD CANVASS"

ieu <- ieu %>%
  select(14:40)



######################################################## RBIND 
pdibind <- rbind(cnc, asd)


pdibind$TYPE <- ifelse(
  grepl("Deep Canvass", pdibind$PROJECTNAME, ignore.case = TRUE), 
  "Deep Canvass", 
  "PDI Canvass"
)

pdibind$PROJECTNAME <- gsub("Deep Canvass", "", pdibind$PROJECTNAME, ignore.case = TRUE)

allcanvass <- rbind(pdibind, cc, rrpe, rrpree, ieu)



#############################################GROUP SUMS

# Group by projectname, canvasser, date, type and sum the rest of the columns
allcanvass_summarized <- allcanvass %>%
  group_by(PROJECTNAME, CANVASSER, DATE, TYPE) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

allcanvass_summarized$TYPE <- gsub("Pre Event Survey|Post Event Survey", "", allcanvass_summarized$TYPE)
allcanvass_summarized$TYPE <- trimws(allcanvass_summarized$TYPE)  # Remove any leading/trailing spaces

allcanvass_summarized$PROJECTNAME <- ifelse(
  allcanvass_summarized$PROJECTNAME %in% c("Alliance San Diego", "Alliance SD Deep Canvass"),
  "Alliance SD",
  allcanvass_summarized$PROJECTNAME
)

allcanvass_summarized$TYPE <- gsub("^CROWD CANVASS$", "Crowd Canvass", allcanvass_summarized$TYPE)


# Standardize PROJECTNAME in allcanvass_summarized
allcanvass_summarized <- allcanvass_summarized %>%
  mutate(PROJECTNAME = recode(PROJECTNAME,
                              "Alliance SD " = "Alliance SD",
                              "Inner City Struggle" = "InnerCity Struggle",
                              "Lift Up Contra Costa " = "Lift Up Contra Costa",
                              "Lift Up Contra Costa (Civic Engagement Table)" = "Lift Up Contra Costa",
                              "Move the Valley" = "Move The Valley",
                              "Move the Valley Deep Canvass" = "Move The Valley",
                              "OCCET " = "OCCET"
  ) %>% str_trim())

# Final cleanup for hidden mismatches
allcanvass_summarized <- allcanvass_summarized %>%
  mutate(PROJECTNAME = ifelse(str_detect(PROJECTNAME, "Move The Valley"), "Move The Valley", PROJECTNAME))

print(unique(allcanvass_summarized$PROJECTNAME))

################################################################# DATA UPDATES 03/18/25
##### DELETE ROWS 
rows_to_remove <- tibble::tibble(
  PROJECTNAME = c("ACCE", "ACCE", "Ventures", "Ventures", "Ventures", "Ventures", "Ventures", "Ventures", "OCCET"),
  DATE = as.Date(c("2024-10-03", "2024-10-18", "2024-09-25", "2024-11-04", "2024-12-12", "2025-01-21", "2025-01-23", "2025-01-27", "2025-01-16")),
  TYPE = c("Summit", "Summit", "Summit", "Summit", "Summit", "Summit", "Summit", "Workshop", "Summit")
)

allcanvass_summarized <- anti_join(allcanvass_summarized, rows_to_remove, by = c("PROJECTNAME", "DATE", "TYPE"))

### UPDATE MANUAL
library(dplyr)
library(lubridate)

allcanvass_summarized <- allcanvass_summarized %>%
  # 1) apply **all** DATE UPDATE & DATE+TYPE UPDATE rules
  mutate(
    DATE = case_when(
      PROJECTNAME == "Move The Valley"                      & DATE == as.Date("2025-01-19") & TYPE == "Crowd Canvass"  ~ as.Date("2025-01-12"),
      PROJECTNAME == "Boys And Girls Club Of Merced County" & DATE == as.Date("2025-02-05") & TYPE == "Crowd Canvass"  ~ as.Date("2025-02-03"),
      PROJECTNAME == "805 Undocufund"                       & DATE == as.Date("2025-02-10") & TYPE == "Summit"         ~ as.Date("2025-02-08"),
      PROJECTNAME == "OCCET"                                & DATE == as.Date("2025-02-18") & TYPE == "Summit"         ~ as.Date("2025-01-18"),
      
      ## African American Chamber…
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-03-17") & TYPE == "Workshop"     ~ as.Date("2025-01-11"),
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-03-27") & TYPE == "Summit"       ~ as.Date("2025-01-31"),  # 3/31 House Meeting blank skipped
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-04-01") & TYPE == "House Meeting" ~ as.Date("2025-02-28"),
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-04-01") & TYPE == "Summit"       ~ as.Date("2024-12-21"),
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-04-01") & TYPE == "Workshop"     ~ as.Date("2025-01-20"),
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-04-03") & TYPE == "Workshop"     ~ as.Date("2025-01-31"),
      
      ## Alliance SD (only those with a provided new date)
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-02-21") & TYPE == "Workshop"  ~ as.Date("2025-02-20"),
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-02-22") & TYPE == "Workshop"  ~ as.Date("2025-02-20"),
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-03-02") & TYPE == "Workshop"  ~ as.Date("2025-02-25"),
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-03-04") & TYPE == "Summit"    ~ as.Date("2025-02-26"),
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-03-05") & TYPE == "Summit"    ~ as.Date("2025-02-26"),
      
      ## Boys & Girls Club…
      PROJECTNAME == "Boys And Girls Club Of Merced County" & DATE == as.Date("2025-03-01") & TYPE == "House Meeting" ~ as.Date("2025-02-25"),
      PROJECTNAME == "Boys And Girls Club Of Merced County" & DATE == as.Date("2025-03-01") & TYPE == "Workshop"      ~ as.Date("2025-02-26"),
      
      ## Boys & Girls Club DATE+TYPE UPDATE on 3/3:
      PROJECTNAME == "Boys And Girls Club Of Merced County" & DATE == as.Date("2025-03-03") & TYPE == "Workshop"      ~ as.Date("2025-02-27"),
      
      ## Community Coalition
      PROJECTNAME == "Community Coalition" & DATE == as.Date("2025-03-25") & TYPE == "Summit"      ~ as.Date("2025-03-26"),
      PROJECTNAME == "Community Coalition" & DATE == as.Date("2025-03-27") & TYPE == "Summit"      ~ as.Date("2025-03-26"),
      PROJECTNAME == "Community Coalition" & DATE == as.Date("2025-03-28") & TYPE == "Summit"      ~ as.Date("2025-03-26"),
      PROJECTNAME == "Community Coalition" & DATE == as.Date("2025-04-03") & TYPE == "Summit"      ~ as.Date("2025-03-26"),
      
      ## CultivaLA: none   
      ## Inland Empire United: none   
      ## InnerCity Struggle: none   
      
      ## Lift Up Contra Costa
      PROJECTNAME == "Lift Up Contra Costa" & DATE == as.Date("2025-03-28") & TYPE == "Summit"         ~ as.Date("2025-03-29"),
      
      ## Move The Valley DATE UPDATE only
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-15") & TYPE == "Workshop"           ~ as.Date("2025-02-15"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-21") & TYPE == "House Meeting"      ~ as.Date("2025-02-21"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-26") & TYPE == "House Meeting"      ~ as.Date("2025-02-26"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-25") & TYPE == "House Meeting"      ~ as.Date("2025-03-25"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-01") & TYPE == "Crowd Canvass"      ~ as.Date("2025-03-15"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-01") & TYPE == "Workshop"           ~ as.Date("2025-03-28"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-07") & TYPE == "Crowd Canvass"      ~ as.Date("2025-03-22"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-09") & TYPE == "Crowd Canvass"      ~ as.Date("2025-03-23"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-10") & TYPE == "Summit"             ~ as.Date("2025-03-29"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-11") & TYPE == "Workshop"           ~ as.Date("2025-03-31"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-13") & TYPE == "Crowd Canvass"      ~ as.Date("2025-01-25"),
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-14") & TYPE == "Crowd Canvass"      ~ as.Date("2025-02-01"),
      
      ## OCCET
      PROJECTNAME == "OCCET" & DATE == as.Date("2024-12-15") & TYPE == "Summit"      ~ as.Date("2025-12-14"),
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-21") & TYPE == "House Meeting"~ as.Date("2025-02-22"),
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-21") & TYPE == "Summit"      ~ as.Date("2025-02-22"),
      
      ## Parent Engagement Academy: none   
      ## Project Joy: none   
      
      ## Ventures
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-01-23") & TYPE == "House Meeting" ~ as.Date("2025-01-22"),
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-24") & TYPE == "House Meeting" ~ as.Date("2025-02-23"),
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-27") & TYPE == "House Meeting" ~ as.Date("2025-02-23"),
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-28") & TYPE == "House Meeting" ~ as.Date("2025-02-23"),
      
      TRUE ~ DATE
    )
  ) %>%
  # 2) your original TYPE mappings (unchanged)…
  mutate(
    TYPE = case_when(
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2024-11-09") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2024-12-21") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-08") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-11") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-12") & TYPE == "Summit"  ~ "Workshop",
      # …all your original lines here…
      TRUE ~ TYPE
    )
  ) %>%
  # 3) layer on **all** the new TYPE‐UPDATE & DATE+TYPE‐UPDATE rules
  mutate(
    TYPE = case_when(
      ## 805 Undocufund
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-16") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-18") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-21") & TYPE == "Summit"   ~ "Crowd Canvass",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-22") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-23") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-26") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-27") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-28") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "805 Undocufund" & DATE == as.Date("2025-02-28") & TYPE == "Workshop" ~ "House Meeting",
      
      ## ACCE
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-01-30") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-01-31") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-08") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-15") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-15") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-26") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-27") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-02-28") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-01") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-02") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-02") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-03") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-03") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-04") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-04") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-13") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-14") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-20") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-26") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-26") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-27") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-29") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-03-30") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "ACCE" & DATE == as.Date("2025-04-09") & TYPE == "Summit"   ~ "House Meeting",
      
      ## African American Chamber…
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-11-08") & TYPE == "Summit"      ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-11-08") & TYPE == "Workshop"    ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-11-30") & TYPE == "Summit"      ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-11-30") & TYPE == "Workshop"    ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-11") & TYPE == "Summit"      ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-18") & TYPE == "Summit"      ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-21") & TYPE == "House Meeting"~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-21") & TYPE == "Summit"      ~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-21") & TYPE == "Workshop"    ~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-29") & TYPE == "House Meeting"~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2024-12-29") & TYPE == "Summit"      ~ "House Meeting",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-01-09") & TYPE == "House Meeting"~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-01-09") & TYPE == "Summit"      ~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-01-11") & TYPE == "Summit"      ~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-01-20") & TYPE == "Summit"      ~ "Workshop",
      PROJECTNAME == "African American Chamber Of Commerce Of The San Joaquin Valley" & DATE == as.Date("2025-01-31") & TYPE == "Summit"      ~ "Workshop",
      
      ## Alliance SD
      PROJECTNAME == "Alliance SD" & DATE == as.Date("2025-02-01") & TYPE == "Workshop" ~ "DELETE",  # will filter out
      # …all other TYPE UPDATE / DELETE lines…
      
      ## Boys & Girls Club…
      PROJECTNAME == "Boys And Girls Club Of Merced County" & DATE == as.Date("2025-03-03") & TYPE == "Workshop" ~ "House Meeting",
      
      ## Community Coalition
      PROJECTNAME == "Community Coalition" & DATE == as.Date("2025-03-26") & TYPE == "House Meeting" ~ "Summit",
      
      ## CultivaLA
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2024-10-03") & TYPE == "Crowd Canvass" ~ "Canvassing",
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2024-12-14") & TYPE == "Crowd Canvass" ~ "Summit",
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2024-12-21") & TYPE == "Crowd Canvass" ~ "Crowd Canvass",
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2025-01-11") & TYPE == "Summit"        ~ "Crowd Canvass",
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2025-03-29") & TYPE == "Workshop"      ~ "Crowd Canvass",
      PROJECTNAME == "CultivaLA" & DATE == as.Date("2025-03-30") & TYPE == "Workshop"      ~ "Crowd Canvass",
      
      ## Inland Empire United
      PROJECTNAME == "Inland Empire United" & TYPE == "Crowd Canvass" & DATE %in% mdy(c("9/13/24","9/14/24","9/15/24","9/16/24","9/20/24","9/21/24","9/22/24","9/23/24","9/27/24","9/28/24","10/6/24")) ~ "Canvassing",
      
      ## InnerCity Struggle
      PROJECTNAME == "InnerCity Struggle" & TYPE == "Workshop" & DATE %in% mdy(c("9/17/24","10/4/24","10/9/24","10/10/24")) ~ "Summit",
      PROJECTNAME == "InnerCity Struggle" & DATE == as.Date("2025-02-12") & TYPE == "Summit"   ~ "Crowd Canvass",
      PROJECTNAME == "InnerCity Struggle" & DATE == as.Date("2025-02-14") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "InnerCity Struggle" & DATE == as.Date("2025-02-22") & TYPE == "House Meeting" ~ "Summit",
      PROJECTNAME == "InnerCity Struggle" & DATE == as.Date("2025-02-22") & TYPE == "Workshop" ~ "Summit",
      
      ## Lift Up Contra Costa
      PROJECTNAME == "Lift Up Contra Costa" & DATE == as.Date("2025-01-22") & TYPE %in% c("House Meeting","Summit") ~ "Crowd Canvass",
      PROJECTNAME == "Lift Up Contra Costa" & DATE == as.Date("2025-01-29") & TYPE == "Summit"  ~ "Workshop",
      PROJECTNAME == "Lift Up Contra Costa" & DATE == as.Date("2025-02-22") & TYPE == "Workshop"~ "Summit",
      PROJECTNAME == "Lift Up Contra Costa" & DATE == as.Date("2025-03-28") & TYPE == "House Meeting" ~ "Canvassing",
      
      ## Move The Valley
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-10-26") & TYPE %in% c("Summit","Workshop") ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-10-29") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-11-05") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-03") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-08") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-11") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-11") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-15") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-19") & TYPE == "House Meeting" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-19") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2024-12-26") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-05") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-07") & TYPE == "House Meeting" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-07") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-10") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-22") & TYPE == "House Meeting" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-22") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-24") & TYPE == "House Meeting" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-24") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-27") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-27") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-28") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-28") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-29") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-29") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-30") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-31") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-01-31") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-05") & TYPE == "House Meeting" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-05") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-05") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-07") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-12") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-14") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-19") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-21") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-02-21") & TYPE == "Workshop" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-07") & TYPE == "House Meeting" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-07") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-09") & TYPE == "Summit" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-24") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-03-24") & TYPE == "Workshop" ~ "House Meeting",  # DATE+TYPE already moved
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-04") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-09") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-11") & TYPE == "Summit" ~ "Workshop",
      PROJECTNAME == "Move The Valley" & DATE == as.Date("2025-04-11") & TYPE == "Workshop" ~ "Workshop",
      
      ## OCCET
      PROJECTNAME == "OCCET" & DATE == as.Date("2024-12-14") & TYPE == "Summit"  ~ "Workshop",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-01-18") & TYPE == "Workshop"~ "Summit",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-01-28") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-01-31") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-01-31") & TYPE == "Workshop"~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-02") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-02") & TYPE == "Workshop"~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-09") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-10") & TYPE == "Summit"  ~ "Workshop",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-13") & TYPE == "Summit"  ~ "Workshop",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-19") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-19") & TYPE == "Workshop"~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-20") & TYPE == "House Meeting"~ "Workshop",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-23") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-23") & TYPE == "Workshop"~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-24") & TYPE == "Summit"  ~ "Workshop",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-27") & TYPE == "Summit"  ~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-27") & TYPE == "Workshop"~ "House Meeting",
      PROJECTNAME == "OCCET" & DATE == as.Date("2025-02-28") & TYPE == "Summit"  ~ "Workshop",
      
      ## Parent Engagement Academy
      PROJECTNAME == "Parent Engagement Academy" & DATE == as.Date("2024-09-28") & TYPE == "Summit"     ~ "Workshop",
      PROJECTNAME == "Parent Engagement Academy" & DATE == as.Date("2025-01-22") & TYPE == "Summit"     ~ "House Meeting",
      PROJECTNAME == "Parent Engagement Academy" & DATE == as.Date("2025-03-06") & TYPE == "Summit"     ~ "House Meeting",
      PROJECTNAME == "Parent Engagement Academy" & DATE == as.Date("2025-03-21") & TYPE == "Workshop"   ~ "Summit",
      
      ## Project Joy
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-10-15") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-10-16") & TYPE == "House Meeting" ~ "Summit",
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-10-16") & TYPE == "Workshop" ~ "Summit",
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-11-25") & TYPE == "Summit" ~ "Summit",
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-11-26") & TYPE == "House Meeting" ~ "Summit",
      PROJECTNAME == "Project Joy" & DATE == as.Date("2024-12-01") & TYPE == "Summit" ~ "Workshop",
      
      ## Ventures
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-01-19") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-01-19") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-01-22") & TYPE == "Summit"   ~ "House Meeting",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-01-22") & TYPE == "Workshop" ~ "House Meeting",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-18") & TYPE == "Summit"   ~ "Workshop",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-19") & TYPE == "House Meeting" ~ "Workshop",
      PROJECTNAME == "Ventures" & DATE == as.Date("2025-02-20") & TYPE == "Summit"   ~ "Workshop",
      
      TRUE ~ TYPE
    )
  ) %>%
  # 4) finally, drop all the DELETE‐marked rows:
  filter(
    !(PROJECTNAME=="805 Undocufund"                     & DATE==as.Date("2025-01-20") & TYPE=="Crowd Canvass"),
    !(PROJECTNAME=="Alliance SD"                       & TYPE %in% c("Summit","Workshop") & DATE %in% as.Date(c("2024-08-20","2024-11-09","2025-01-28","2025-02-19","2025-02-21","2025-02-22","2025-03-02","2025-03-03","2025-03-13","2025-03-15"))),
    !(PROJECTNAME=="Lift Up Contra Costa"              & DATE==as.Date("2024-09-18") & TYPE=="Summit"),
    TRUE
  )

##################################################### report pull
report <- allcanvass_summarized
report <- ungroup(report)
report <- select(report, PROJECTNAME, DATE, TYPE, DOORSKNOCKED)
report <- report %>%
  group_by(PROJECTNAME, DATE, TYPE) %>%
  summarise(DOORSKNOCKED = sum(DOORSKNOCKED, na.rm = TRUE), .groups = "drop")

write.csv(report, file = '/Users/birdieligos/Documents/Reports/ReadyRep_OrgProgress_022625_031825.csv', row.names = FALSE)

##### WRITE TO BIG QUERY 
library(bigrquery)

# Define BigQuery location and table reference
project_id <- "slstrategy"
dataset_id <- "Ready_Rep"
table_id <- "Canvass_Productivity_2024"
# Create a table reference
table_ref <- bq_table(project = project_id, dataset = dataset_id, table = table_id)

# Check if the table exists
table_exists <- bq_table_exists(table_ref)

# If the table exists, delete it
if (table_exists) {
  bq_table_delete(table_ref)
}

# Define the schema based on selected columns
schema <- list(
  bq_field("PROJECTNAME", "STRING"),
  bq_field("CANVASSER", "STRING"),
  bq_field("DATE", "DATE"),
  bq_field("TOTALVOTERS", "INT64"),
  bq_field("DOORSKNOCKED", "INT64"),
  bq_field("CONTACTS", "INT64"),
  bq_field("CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_YES", "INT64"),
  bq_field("CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_U", "INT64"),
  bq_field("CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_NO", "INT64"),
  bq_field("CAN_WE_SIGN_YOU_UP_FOR_EMERGENCY_SERVICE_NOTIFICATIONS_IASU", "INT64"),
  bq_field("Attend_Event_Y", "INT64"),
  bq_field("Attend_Event_N", "INT64"),
  bq_field("Attend_Event_Maybe", "INT64"),
  bq_field("Ready_Rep_Y", "INT64"),
  bq_field("Ready_Rep_N", "INT64"),
  bq_field("Ready_Rep_Maybe", "INT64"),
  bq_field("WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWAN", "INT64"),
  bq_field("WHAT_ARE_SOME_OF_YOUR_CONCERNS_DWTBCBTG", "INT64"),
  bq_field("WHAT_ARE_SOME_OF_YOUR_CONCERNS_TTAPE", "INT64"),
  bq_field("WHAT_ARE_SOME_OF_YOUR_CONCERNS_DFTU", "INT64"),
  bq_field("WHAT_ARE_SOME_OF_YOUR_CONCERNS_WD", "INT64"),
  bq_field("NON_CONTACT_MOBILE_NH", "INT64"),
  bq_field("NON_CONTACT_MOBILE_MV", "INT64"),
  bq_field("NON_CONTACT_MOBILE_REF", "INT64"),
  bq_field("NON_CONTACT_MOBILE_GTD", "INT64"),
  bq_field("NON_CONTACT_MOBILE_D", "INT64"),
  bq_field("TYPE", "STRING")
)

# Attempt to create the BigQuery table and upload the data
tryCatch({
  # Create the table with the defined schema
  bq_table_create(table_ref, fields = schema)
  
  # Upload the data to the newly created table
  bq_table_upload(table_ref, values = allcanvass_summarized)
  
  # Print a success message if the table creation and data upload are successful
  cat("Table created and data uploaded successfully!\n")
}, 
error = function(e) {
  # Print error message if an error occurs
  cat("An error occurred:", conditionMessage(e), "\n")
})





###################################################### ORG PROGRESS TO GOALS 
goal <- allcanvass_summarized

# completed events to goal df
# add in tier data
tiers_org <- read_sheet("https://docs.google.com/spreadsheets/d/1_-zCWu4To9Nw0vLrE2n1IjeELJ44KxmpAEDi6asCbTM/edit?resourcekey=&gid=485056992")

tiers_org <- tiers_org %>%
  rename(Tier = ...2, RM = ...3, PROJECTNAME = Organization) %>%
  select(PROJECTNAME, Tier, RM)

tiers_org <- tiers_org %>%
  filter(!is.na(PROJECTNAME))

print(unique(tiers_org$PROJECTNAME))
print(unique(goal$PROJECTNAME))

tiers_org <- tiers_org %>%
  mutate(PROJECTNAME = case_when(
    PROJECTNAME == "Inland Empire United (Coalition)" ~ "Inland Empire United",
    PROJECTNAME == "Alliance San Diego" ~ "Alliance SD",
    PROJECTNAME == "Lift Up Contra Costa (Civic Engagment Table)" ~ "Lift Up Contra Costa",
    PROJECTNAME == "Central Coast Alliance United for a Sustainable Economy  (CAUSE)" ~ "CAUSE",
    PROJECTNAME == "Orange County Civic Engagement Table" ~ "OCCET",
    PROJECTNAME == "805 Undocufund" ~ "805 Undocufund",
    PROJECTNAME == "Boys and Girls Club of Merced County" ~ "Boys and Girls Club of Merced County",
    PROJECTNAME == "African American Chamber of Commerce of the San Joaquin Valley" ~ "AACC",
    PROJECTNAME == "LA Voice" ~ "LA Voice",
    PROJECTNAME == "ACCE" ~ "ACCE",
    PROJECTNAME == "Community Coalition" ~ "Community Coalition",
    PROJECTNAME == "Project Joy" ~ "Project Joy",
    PROJECTNAME == "CultivaLA" ~ "CultivaLA",
    PROJECTNAME == "Parent Engagement Academy" ~ "Parent Engagement Academy",
    PROJECTNAME == "InnerCity Struggle" ~ "InnerCity Struggle",
    PROJECTNAME == "LUCC" ~ "Lift Up Contra Costa",
    PROJECTNAME == "Orange County Civic Engagement Table" ~ "OCCET",
    PROJECTNAME == "Move The Valley" ~ "Move The Valley",
    TRUE ~ PROJECTNAME
  ))

goal <- goal %>%
  mutate(PROJECTNAME = case_when(
    PROJECTNAME == "Inland Empire United (Coalition)" ~ "Inland Empire United",
    PROJECTNAME == "Inner City Struggle" ~ "InnerCity Struggle",
    PROJECTNAME == "Lift Up Contra Costa (Civic Engagement Table)" ~ "Lift Up Contra Costa",
    PROJECTNAME == "OCCET Deep Canvass" ~ "OCCET",
    PROJECTNAME == "African American Chamber of Commerce of the San Joaquin Valley" ~ "AACC",
    PROJECTNAME == "BGC" ~ "Boys and Girls Club of Merced County",
    PROJECTNAME == "LUCC" ~ "Lift Up Contra Costa",
    PROJECTNAME == "Orange County Civic Engagement Table" ~ "OCCET",
    PROJECTNAME == "Move The Valley" ~ "Move The Valley",
    TRUE ~ PROJECTNAME
  ))

# Update PROJECTNAME in tiers_org dataframe
tiers_org <- tiers_org %>%
  mutate(PROJECTNAME = case_when(
    PROJECTNAME == "Alliance SD Deep Canvass" ~ "Alliance SD",
    PROJECTNAME == "Alliance San Diego" ~ "Alliance SD",
    TRUE ~ PROJECTNAME
  ))

# Update PROJECTNAME in goal dataframe
goal <- goal %>%
  mutate(PROJECTNAME = case_when(
    PROJECTNAME == "Alliance SD Deep Canvass" ~ "Alliance SD",
    PROJECTNAME == "Alliance San Diego" ~ "Alliance SD",
    TRUE ~ PROJECTNAME
  ))

# Clean and standardize PROJECTNAME in tiers_org
tiers_org <- tiers_org %>%
  mutate(PROJECTNAME = str_trim(PROJECTNAME),
         PROJECTNAME = str_to_title(PROJECTNAME))

# Clean and standardize PROJECTNAME in goal
goal <- goal %>%
  mutate(PROJECTNAME = str_trim(PROJECTNAME),
         PROJECTNAME = str_to_title(PROJECTNAME))

# Specific replacement for Move The Valley in case of hidden mismatches
tiers_org <- tiers_org %>%
  mutate(PROJECTNAME = ifelse(str_detect(PROJECTNAME, "Move The Valley"), "Move The Valley", PROJECTNAME))

goal <- goal %>%
  mutate(PROJECTNAME = ifelse(str_detect(PROJECTNAME, "Move The Valley"), "Move The Valley", PROJECTNAME))

# completed events
goal <- goal %>%
  ungroup() %>%
  select(PROJECTNAME, TYPE, DOORSKNOCKED, Ready_Rep_Y, DATE)

goal <- goal %>%
  group_by(PROJECTNAME, TYPE, DATE) %>%
  summarise(
    DOORSKNOCKED = sum(DOORSKNOCKED, na.rm = TRUE),
    Ready_Rep_Y = sum(Ready_Rep_Y, na.rm = TRUE),
    .groups = "drop"
  )


tiers_org <- tiers_org %>%
  mutate(PROJECTNAME = ifelse(PROJECTNAME == "Alliance Sd", "Alliance SD", PROJECTNAME))

goal <- goal %>%
  mutate(PROJECTNAME = ifelse(PROJECTNAME == "Alliance Sd", "Alliance SD", PROJECTNAME))

# 

goal_tier <- left_join(goal, tiers_org)

#### format 

goal_tier$Days <- 1

goal_tier <- goal_tier %>%
  rename(Engaged = DOORSKNOCKED)

goal_tier <- select(goal_tier, -DATE)

goal_tier <- goal_tier %>%
  group_by(PROJECTNAME, TYPE, Tier, RM) %>%
  summarise(
    Engaged = sum(Engaged, na.rm = TRUE),
    Ready_Rep_Y = sum(Ready_Rep_Y, na.rm = TRUE),
    Days = sum(Days, na.rm = TRUE),
    .groups = "drop"
  )

str(goal_tier)


############ write tier goal table
# Combine all tiers into a single structured dataframe
tiers <- rbind(
  # Tier 1
  data.frame(
    Tier = 1,
    Event = c("Summit", "Summit", "House Meetings", "Workshops", "PDI Canvassing"),
    Requirement_Type = c("Engaged", "Ready Rep Y", "Days", "Days", "Days"),
    Requirement_Value = c(300, 150, 8, 2, 30),
    Alternative_Group = c(NA, NA, NA, NA, NA),
    Requirement_Group = c(1, 1, 1, 1, 1)
  ),
  # Tier 2
  data.frame(
    Tier = 2,
    Event = c("Summit", "Summit", "House Meetings", "Workshops", "PDI Canvassing"),
    Requirement_Type = c("Engaged", "Ready Rep Y", "Days", "Days", "Days"),
    Requirement_Value = c(200, 80, 8, 2, 20),
    Alternative_Group = c(NA, NA, NA, NA, NA),
    Requirement_Group = c(1, 1, 1, 1, 1)
  ),
  # Tier 3
  data.frame(
    Tier = 3,
    Event = c("Summit", "Summit", "House Meetings", "Workshops", "PDI Canvassing"),
    Requirement_Type = c("Engaged", "Ready Rep Y", "Days", "Days", "Days"),
    Requirement_Value = c(100, 40, 8, 2, 10),
    Alternative_Group = c(NA, NA, NA, NA, NA),
    Requirement_Group = c(1, 1, 1, 1, 1)
  ),
  # Tier 3.5
  data.frame(
    Tier = 3.5,
    Event = c("Summit", "House Meetings", "Workshops", "PDI Canvassing", "Crowd Canvassing"),
    Requirement_Type = c("Engaged", "Days", "Days", "Days", "Days"),
    Requirement_Value = c(200, 8, 2, 5, 5),
    Alternative_Group = c(NA, NA, 1, 1, 1),
    Requirement_Group = c(1, 1, 1, 1, 1)
  ),
  # Tier 4
  tier_4 <- data.frame(
    Tier = 4,
    Event = c(
      "PDI Canvass", "Crowd Canvass", "House Meetings", "Crowd Canvass",
      "Workshops", "Summit"
    ),
    Requirement_Type = c("Days", "Days", "Days", "Days", "Engaged", "Engaged"),
    Requirement_Value = c(5, 5, 4, 4, 8, 159),
    Alternative_Group = c(1, 1, 2, 2, NA, NA),
    Requirement_Group = c(1, 1, 1, 1, 1, 2)
  ),
  # Tier 5
  data.frame(
    Tier = 5,
    Event = c("House Meetings", "Workshops"),
    Requirement_Type = c("Days", "Days"),
    Requirement_Value = c(8, 2),
    Alternative_Group = c(NA, NA),
    Requirement_Group = c(1, 1)
  )
)

################################### join tiers goals and progress
goal_tier <- goal_tier %>%
  mutate(
    Tier = str_trim(Tier),
    TYPE = str_trim(TYPE)
  )

tiers <- tiers %>%
  mutate(
    Tier = str_trim(Tier),
    Event = str_trim(Event)
  )

goal_tier <- goal_tier %>%
  mutate(TYPE = str_to_title(str_remove(TYPE, "Post Event Survey")))

goal_tier <- goal_tier %>%
  mutate(TYPE = str_to_title(str_remove(TYPE, "Pre Event Survey")))

goal_tier <- goal_tier %>%
  mutate(TYPE = case_when(
    str_trim(TYPE) == "Crowd Canvass" ~ "Crowd Canvassing",
    str_trim(TYPE) == "Pdi Canvas" ~ "PDI Canvass",
    str_trim(TYPE) == "Pdi Canvass" ~ "PDI Canvass",
    str_trim(TYPE) == "Summit" ~ "Summit",
    str_trim(TYPE) == "Workshop" ~ "Workshops",
    str_trim(TYPE) == "House Meeting" ~ "House Meetings",
    str_trim(TYPE) == "Deep Canvass" ~ "Deep Canvass",
    TRUE ~ str_trim(TYPE)
  ))

tiers <- tiers %>%
  mutate(Event = case_when(
    str_trim(Event) == "Crowd Canvass" ~ "Crowd Canvassing",
    str_trim(Event) == "Pdi Canvas" ~ "PDI Canvass",
    str_trim(Event) == "Summit" ~ "Summit",
    str_trim(Event) == "Workshop" ~ "Workshops",
    str_trim(Event) == "House Meeting" ~ "House Meetings",
    str_trim(Event) == "Deep Canvass" ~ "Deep Canvass",
    TRUE ~ str_trim(Event)
  ))

goal_tier <- goal_tier %>%
  pivot_longer(
    cols = c(Engaged, Ready_Rep_Y, Days),
    names_to = "Progress_Type",
    values_to = "Progress_Value"
  )


tiers <- tiers %>%
  mutate(Requirement_Type = ifelse(Requirement_Type == "Ready Rep Y", "Ready_Rep_Y", Requirement_Type))

tiers <- tiers %>%
  mutate(Requirement_Type = ifelse(Requirement_Type == "PDI Canvass", "PDI Canvassing", Requirement_Type))

goal_tier <- goal_tier %>%
  group_by(PROJECTNAME, TYPE, Progress_Type, RM, Tier) %>%
  summarise(
    Progress_Value = sum(Progress_Value, na.rm = TRUE),
    .groups = "drop"
  )

goal_tier <- goal_tier %>%
  mutate(TYPE = str_trim(TYPE), Progress_Type = str_trim(Progress_Type))

tiers <- tiers %>%
  mutate(Event = str_trim(Event), Requirement_Type = str_trim(Requirement_Type))

goal_tier <- goal_tier %>%
  mutate(Tier = str_extract(Tier, "\\d+(\\.\\d+)?"))

tiers <- tiers %>%
  mutate(Event = ifelse(Event == "PDI Canvassing", "PDI Canvass", Event))


tiers_org <- tiers_org %>%
  mutate(Tier = str_extract(Tier, "\\d+(\\.\\d+)?"))

tiers_org <- select(tiers_org, -RM)

tiers <- left_join(tiers_org, tiers)

merged_df <- goal_tier %>%
  right_join(tiers, by = c("TYPE" = "Event", "Progress_Type" = "Requirement_Type", "Tier" = "Tier", "PROJECTNAME" = "PROJECTNAME"))

merged_df <- merged_df %>%
  mutate(Progress_Value = replace_na(Progress_Value, 0),
         Progress_Value = as.integer(Progress_Value))

merged_df <- merged_df %>%
  mutate(
    Alternative_Group = as.character(Alternative_Group), # Convert to character first
    Requirement_Group = as.character(Requirement_Group), # Convert to character first
    Requirement_Value = replace_na(Requirement_Value, 0),
    Alternative_Group = replace_na(Alternative_Group, "Not Applicable"),
    Requirement_Group = replace_na(Requirement_Group, "Not Applicable")
  ) %>%
  mutate(across(where(is.character), str_trim))

merged_df <- merged_df %>%
  mutate(Requirement_Status = ifelse(Alternative_Group == "Not Applicable" & Requirement_Group == "Not Applicable", 
                                     "Not Required", "Required"))

################################# write goals big query table 

library(bigrquery)

# Define the schema
schema <- list(
  bq_field("PROJECTNAME", "STRING"),
  bq_field("TYPE", "STRING"),
  bq_field("Progress_Type", "STRING"),
  bq_field("RM", "STRING"),
  bq_field("Tier", "STRING"),
  bq_field("Progress_Value", "INT64"),
  bq_field("Requirement_Value", "INT64"),
  bq_field("Alternative_Group", "STRING"),
  bq_field("Requirement_Group", "STRING"),
  bq_field("Requirement_Status", "STRING")
)

# Specify BigQuery project, dataset, and table IDs
project_id <- "slstrategy"
dataset_id <- "Ready_Rep"
table_id <- "Ready_Rep_Goals"
bq_table <- paste0(project_id, ".", dataset_id, ".", table_id)

# Write the dataframe to BigQuery and overwrite the table
bq_table_upload(
  x = bq_table,
  values = merged_df,
  fields = schema,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_TRUNCATE"
)



