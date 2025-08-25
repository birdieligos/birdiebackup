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
library(readxl)
unloadNamespace("plyr")


a <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_1of9.csv', stringsAsFactors = FALSE)
b <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_2of9.csv', stringsAsFactors = FALSE)
c <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_3of9.csv', stringsAsFactors = FALSE)
d <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_4of9.csv', stringsAsFactors = FALSE)
e <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_5of9.csv', stringsAsFactors = FALSE)
f <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_6of9.csv', stringsAsFactors = FALSE)
g <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_7of9.csv', stringsAsFactors = FALSE)
h <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_8of9.csv', stringsAsFactors = FALSE)
i <- read.csv('/Users/birdieligos/Downloads/AllFlags(10)_8of9.csv', stringsAsFactors = FALSE)

merge <- rbind(a,b,c,d,e,f,g,h,i)

print(unique(merge$RESPONSECODE))

data <- bq_table_download("slstrategy.DCBA.DCBA_Meta")






