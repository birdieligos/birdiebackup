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

################## District Databases
######################################################################################### FRESNO COUNTY RESULTS
######################################################## FRESNO AD27 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO AD 27.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'AD 27'
ad31$COUNTY <- 'FRESNO'
AD27FRESNO <- ad31

######################################################################## FRESNO PRESIDENTIAL RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PRESIDENTIAL.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'PRESIDENTIAL'
ad31$COUNTY <- 'FRESNO'
PRESIDENTIALFRESNO <- ad31

# RBIND

rbind <- rbind(AD27FRESNO, PRESIDENTIALFRESNO)

###################################################################### FRESNO SENATE RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO SENATE.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'SENATE'
ad31$COUNTY <- 'FRESNO'
SENATEFRESNO <- ad31

# RBIND

rbind <- rbind(rbind, SENATEFRESNO)

###################################################################### FRESNO CD 13 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO CD13.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'CD 13'
ad31$COUNTY <- 'FRESNO'
CD13FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, CD13FRESNO)


###################################################################### FRESNO CD 21 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO CD21.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'CD 21'
ad31$COUNTY <- 'FRESNO'
CD21FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, CD21FRESNO)

###################################################################### FRESNO PROP3 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP 3.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub("YES", "DEM", colnames(ad31))
colnames(ad31) <- gsub("NO", "REP", colnames(ad31))
colnames(ad31) <- gsub("\\.+$", "", colnames(ad31)) 

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'PROP 3'
ad31$COUNTY <- 'FRESNO'
PROP3FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, PROP3FRESNO)

###################################################################### FRESNO PROP 32 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP 32.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub("YES", "DEM", colnames(ad31))
colnames(ad31) <- gsub("NO", "REP", colnames(ad31))
colnames(ad31) <- gsub("\\.+$", "", colnames(ad31)) 

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'PROP 32'
ad31$COUNTY <- 'FRESNO'
PROP32FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, PROP32FRESNO)

###################################################################### FRESNO PROP 33 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP33.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub("YES", "DEM", colnames(ad31))
colnames(ad31) <- gsub("NO", "REP", colnames(ad31))
colnames(ad31) <- gsub("\\.+$", "", colnames(ad31)) 

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'PROP 33'
ad31$COUNTY <- 'FRESNO'
PROP33FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, PROP33FRESNO)

###################################################################### FRESNO PROP 36 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/FRESNO PROP 36.csv', stringsAsFactors = FALSE)

ad31 <- result
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[3, ])
ad31 <- ad31[-c(1, 2, 3), ]

# remove irrelavant rows 
ad31 <- ad31[!(ad31$Precinct %in% c("Vote by Mail", "County", "Electionwide", "Vote Center", "", "Cumulative", "Electionwide - Total", "Cumulative - Total", "County - Total")), ]
rownames(ad31) <- NULL

print(colnames(ad31))
# fill down precinct 
colnames(ad31) <- make.names(colnames(ad31), unique = TRUE)
ad31 <- ad31 %>%
  mutate(Precinct = ifelse(Precinct == "Total", lag(Precinct), Precinct)) %>%
  filter(Precinct != "Total")

# remove more bad rows
ad31 <- ad31[ad31$Times.Cast != "", ]

# update candiate column names
colnames(ad31) <- gsub("YES", "DEM", colnames(ad31))
colnames(ad31) <- gsub("NO", "REP", colnames(ad31))
colnames(ad31) <- gsub("\\.+$", "", colnames(ad31)) 

# select columns 
ad31 <- select(ad31, Precinct, Times.Cast, Registered..Voters, Undervotes, Overvotes,
               DEM, REP)

colnames(ad31) <- gsub("\\.+", ".", colnames(ad31))
colnames(ad31) <- gsub("\\.", "_", colnames(ad31))
ad31[] <- lapply(ad31, function(x) ifelse(grepl("\\*", x), 0, x))

### format
ad31[] <- lapply(ad31, function(x) gsub(",", "", as.character(x)))
ad31[] <- lapply(ad31, function(x) as.integer(as.character(x)))

ad31$RACE <- 'PROP 36'
ad31$COUNTY <- 'FRESNO'
PROP36FRESNO <- ad31

# RBIND

rbind <- rbind(rbind, PROP36FRESNO)

######################################################################################### MERCED COUNTY RESULTS
######################################################## MERCED AD27 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED AD27.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'AD 27'
ad31$COUNTY <- 'MERCED'
AD27MERCED <- ad31

# RBIND

rbind <- rbind(rbind, AD27MERCED)

######################################################## MERCED PRESIDENTIAL RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED PRESIDENTIAL.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PRESIDENTIAL'
ad31$COUNTY <- 'MERCED'
PRESIDENTIALMERCED <- ad31

# RBIND

rbind <- rbind(rbind, PRESIDENTIALMERCED)

######################################################## MERCED SENATE RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED SENATE.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'SENATE'
ad31$COUNTY <- 'MERCED'
SENATEMERCED <- ad31

# RBIND

rbind <- rbind(rbind, SENATEMERCED)


######################################################## MERCED CD13 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED CD13.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'CD 13'
ad31$COUNTY <- 'MERCED'
CD13MERCED <- ad31

# RBIND

rbind <- rbind(rbind, CD13MERCED)


######################################################## MERCED PROP 3 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED PROP3.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*YES*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*NO*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 3'
ad31$COUNTY <- 'MERCED'
PROP3MERCED <- ad31

# RBIND

rbind <- rbind(rbind, PROP3MERCED)

######################################################## MERCED PROP 32 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED PROP32.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*YES*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*NO*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 32'
ad31$COUNTY <- 'MERCED'
PROP32MERCED <- ad31

# RBIND

rbind <- rbind(rbind, PROP32MERCED)

######################################################## MERCED PROP 33 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED PROP33.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*YES*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*NO*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 33'
ad31$COUNTY <- 'MERCED'
PROP33MERCED <- ad31

# RBIND

rbind <- rbind(rbind, PROP33MERCED)

######################################################## MERCED PROP 36 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MERCED PROP36.csv', stringsAsFactors = FALSE)


ad31 <- result
ad31[1, ] <- {
  row <- as.character(ad31[1, ])
  for (i in 2:length(row)) {
    if (row[i] == "" || is.na(row[i])) {
      row[i] <- row[i - 1]
    }
  }
  row[length(row)] <- "TOTAL"
  row
}
ad31[2, ] <- {
  row <- as.character(ad31[2, ])
  for (i in seq_along(row)) {
    if (trimws(row[i]) == "Total Votes") {
      row[i] <- paste("Total Votes", as.character(ad31[1, i]))
    }
  }
  row
}
# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[2, ])
ad31 <- ad31[-c(1, 2), ]

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*YES*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*NO*", "REP", colnames(ad31))

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Total", "Times_Cast", colnames(ad31))
ad31$Overvotes <- 0
ad31$Undervotes <- 0

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 36'
ad31$COUNTY <- 'MERCED'
PROP36MERCED <- ad31

# RBIND

rbind <- rbind(rbind, PROP36MERCED)

######################################################## MADERA AD27 RESULTS
# ad27
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA AD27.csv', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'AD 27'
ad31$COUNTY <- 'MADERA'
AD27MADERA <- ad31

# RBIND

rbind <- rbind(rbind, AD27MADERA)

######################################################## MADERA PRESIDENTIAL RESULTS
# PRESIDENTIAL
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA PRESIDENTIAL.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PRESIDENTIAL'
ad31$COUNTY <- 'MADERA'
PRESIDENTIALMADERA <- ad31

# RBIND

rbind <- rbind(rbind, PRESIDENTIALMADERA)

######################################################## MADERA SENATE RESULTS
# SENATE
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA SENATE.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'SENATE'
ad31$COUNTY <- 'MADERA'
SENATEMADERA <- ad31

# RBIND

rbind <- rbind(rbind, SENATEMADERA)


######################################################## MADERA CD13 RESULTS
# CD13
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA CD13.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*DEM.*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*REP.*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'CD 13'
ad31$COUNTY <- 'MADERA'
CD13MADERA <- ad31

# RBIND

rbind <- rbind(rbind, CD13MADERA)

######################################################## MADERA PROP 3 RESULTS
# PROP 3
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA PROP3.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*Yes*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*No*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 3'
ad31$COUNTY <- 'MADERA'
PROP3MADERA <- ad31

# RBIND

rbind <- rbind(rbind, PROP3MADERA)

######################################################## MADERA PROP 32 RESULTS
# PROP 32
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA PROP32.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*Yes*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*No*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 32'
ad31$COUNTY <- 'MADERA'
PROP32MADERA <- ad31

# RBIND

rbind <- rbind(rbind, PROP32MADERA)

######################################################## MADERA PROP 33 RESULTS
# PROP 33
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA PROP33.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*Yes*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*No*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 33'
ad31$COUNTY <- 'MADERA'
PROP33MADERA <- ad31

# RBIND

rbind <- rbind(rbind, PROP33MADERA)

######################################################## MADERA PROP 36 RESULTS
# PROP 36
result <- read.csv('/Users/birdieligos/Documents/2024 POST ELECTION/MADERA PROP36.CSV', stringsAsFactors = FALSE)

ad31 <- result

# Update row 3 to be the column names
colnames(ad31) <- as.character(ad31[5, ])
ad31 <- ad31[-c(1, 2, 3, 4, 5), ]

colnames(ad31)[1] <- "Precinct"
colnames(ad31)[2] <- "Column_1"
ad31 <- ad31[ad31$Column_1 == "Total", ]

# Update columns
colnames(ad31) <- gsub("Registered Voters", "Registered_Voters", colnames(ad31))
colnames(ad31) <- gsub("Voters Cast", "Times_Cast", colnames(ad31))
colnames(ad31) <- gsub("Under Votes", "Undervotes", colnames(ad31))
colnames(ad31) <- gsub("Over Votes", "Overvotes", colnames(ad31))

# UPDATE CANDIATE COLUMNS
colnames(ad31) <- gsub(".*Yes*", "DEM", colnames(ad31))
colnames(ad31) <- gsub(".*No*", "REP", colnames(ad31))

# select columns 
ad31 <- select(ad31, Precinct, Times_Cast, Registered_Voters, Undervotes, Overvotes,
               DEM, REP)

ad31$RACE <- 'PROP 36'
ad31$COUNTY <- 'MADERA'
PROP36MADERA <- ad31

# RBIND

rbind <- rbind(rbind, PROP36MADERA)

#################################################################################### NOW ADD IN DISTRICT + PRECINCT DATA 
# # precincts 
# sd14precincts <- read.csv('/Users/birdieligos/Documents/PRECINCT COUNT REPORTS/SD14 ELECTION PRECINCT .csv', stringsAsFactors = FALSE)
# sd14precincts$SD <- '14'
# sd14precincts$Description <- trimws(sd14precincts$Description)
# sd14precincts$County <- ifelse(grepl("^10", sd14precincts$Description), "Fresno",
#                                ifelse(grepl("^20", sd14precincts$Description), "Madera",
#                                       ifelse(grepl("^24", sd14precincts$Description), "Merced", 'NA')))
# sd14precincts <- select(sd14precincts, -Total)
# 
# ad27precincts <- read.csv('/Users/birdieligos/Documents/PRECINCT COUNT REPORTS/AD27 ELECTION PRECINCT .csv', stringsAsFactors = FALSE)
# ad27precincts$Description <- trimws(ad27precincts$Description)
# ad27precincts$County <- ifelse(grepl("^10", ad27precincts$Description), "Fresno",
#                       ifelse(grepl("^20", ad27precincts$Description), "Madera",
#                              ifelse(grepl("^24", ad27precincts$Description), "Merced", 'NA')))
# ad27precincts <- select(ad27precincts, -Total)
# 
# # merge precinct database
# merge <- merge(ad27precincts, sd14precincts, all = TRUE)
# 
# 
# # make extra district columns
# merge$AD27 <- ifelse(merge$AD == 27, "YES", "NOT AD27")
# merge$SD14 <- ifelse(merge$SD == 14, "YES", "NOT SD14")
# 
# # Create DISTRICT_OVERLAP column
# merge$DISTRICT_OVERLAP <- ifelse(merge$AD27 == "YES" & merge$SD14 == "YES", "AD27 AND SD14",
#                                  ifelse(merge$AD27 == "NO" & merge$SD14 == "YES", "SD14 EXCLUDING AD27",
#                                         ifelse(merge$AD27 == "YES" & merge$SD14 == "NO", "AD27 EXCLUDING SD14",
#                                                "NOT AD27 OR SD14")))
# 
# colnames(merge)[colnames(merge) == "Description"] <- "Precinct"
# 
# # clean up precincts 
# merge$Precinct <- gsub("^10|^20|^24", "", merge$Precinct)
# merge$Precinct <- gsub("^0+", "", merge$Precinct)
# merge <- merge %>%
#   mutate(Precinct = if_else(County == "Merced",
#                             gsub("^24VC", "", Precinct),
#                             Precinct))
# merge$Precinct <- ifelse(merge$County == "Merced",
#                          gsub("^0+", "", merge$Precinct),
#                          merge$Precinct)
# 
# merge$Precinct <- trimws(merge$Precinct)
# merge$County <- trimws(merge$County)
# colnames(merge)[colnames(merge) == "County"] <- "COUNTY"
# merge$COUNTY <- toupper(merge$COUNTY)
# 
# districtprecincts <- merge

########## get rbind ready
merge <- rbind

merge$Precinct <- trimws(merge$Precinct)
merge$COUNTY <- trimws(merge$COUNTY)
merge <- merge[merge$Registered_Voters != 0, ]
merge <- merge[merge$Precinct != "Total:", ]

results <- merge

########################################################## PDI PRECINCT FILES 

# madera
file_path <- "/Users/birdieligos/Downloads/pct_24GElectionPrecinctsCo20Madera 2.tsv"

madera <- read.delim(file_path, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip=4)

madera <- select(madera, Election.Precinct, AD, SD, CD)

madera$COUNTY <- 'MADERA'

colnames(madera)[colnames(madera) == "Election.Precinct"] <- "Precinct"

# fresno

file_path <- "/Users/birdieligos/Downloads/pct_24GElectionPrecinctsCo10Fresno.tsv"

fresno <- read.delim(file_path, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip=4)

fresno <- select(fresno, Election.Precinct, AD, SD, CD)

fresno$COUNTY <- 'FRESNO'

colnames(fresno)[colnames(fresno) == "Election.Precinct"] <- "Precinct"

# merced

file_path <- '/Users/birdieligos/Downloads/pct_24GElectionPrecinctsCo24Merced.tsv'

merced <- read.delim(file_path, sep="\t", header=TRUE, stringsAsFactors=FALSE, skip=4)

merced <- select(merced, Election.Precinct, AD, SD, CD)

merced$COUNTY <- 'MERCED'

colnames(merced)[colnames(merced) == "Election.Precinct"] <- "Precinct"

# rbind counties 

merge <- rbind(madera, fresno, merced)

# make extra district columns
merge$AD27 <- ifelse(merge$AD == 27, "YES", "NOT AD27")
merge$SD14 <- ifelse(merge$SD == 14, "YES", "NOT SD14")

# Create DISTRICT_OVERLAP column
merge$DISTRICT_OVERLAP <- ifelse(merge$AD27 == "YES" & merge$SD14 == "YES", "AD27 AND SD14",
                                 ifelse(merge$AD27 == "NOT AD27" & merge$SD14 == "YES", "SD14 EXCLUDING AD27",
                                        ifelse(merge$AD27 == "YES" & merge$SD14 == "NOT SD14", "AD27 EXCLUDING SD14",
                                               "NOT AD27 OR SD14")))

merge$Precinct <- as.character(merge$Precinct)

districtprecincts <- merge

########################## combine results and precincts 

merge <- full_join(results, districtprecincts, by = c("Precinct", "COUNTY"))

merge <- merge[!is.na(merge$DISTRICT_OVERLAP), ]

merge <- merge[!is.na(merge$RACE), ]

merge[, 1:7] <- lapply(merge[, 1:7], function(x) {
  as.integer(gsub("[^0-9]", "", as.character(x)))
})

merge$Precinct <- as.character(merge$Precinct)

merge$STAND_ALONE_DISTRICT <- merge$DISTRICT_OVERLAP

ad27 <- filter(merge, AD == '27')
ad27$STAND_ALONE_DISTRICT <- 'AD27'


sd14 <- filter(merge, SD == '14')
sd14$STAND_ALONE_DISTRICT <- 'SD14'

merge <- rbind(sd14, ad27, merge)

###################### write big q
library(bigrquery)

# Define project, dataset, and table details
project_id <- "slscampaigns-364520"
dataset_id <- "Soria_2024"
table_id <- "POST_ELECTION_RESULTS_ANALYSIS"

# Create BigQuery schema
schema <- schema_fields <- list(
  list(name = "Precinct", type = "STRING"),
  list(name = "Times_Cast", type = "INTEGER"),
  list(name = "Registered_Voters", type = "INTEGER"),
  list(name = "Undervotes", type = "INTEGER"),
  list(name = "Overvotes", type = "INTEGER"),
  list(name = "DEM", type = "INTEGER"),
  list(name = "REP", type = "INTEGER"),
  list(name = "RACE", type = "STRING"),
  list(name = "COUNTY", type = "STRING"),
  list(name = "AD", type = "STRING"),
  list(name = "SD", type = "STRING"),
  list(name = "CD", type = "STRING"),
  list(name = "Total", type = "STRING"),
  list(name = "AD27", type = "STRING"),
  list(name = "SD14", type = "STRING"),
  list(name = "STAND_ALONE_DISTRICT", type = "STRING"),
  list(name = "DISTRICT_OVERLAP", type = "STRING")
)

# Upload the dataframe to BigQuery
bq_table_upload(
  bq_table(project_id, dataset_id, table_id),
  values = merge,
  fields = schema,
  write_disposition = "WRITE_TRUNCATE"
)





