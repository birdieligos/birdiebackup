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


sen22 <- read.csv('/Users/birdieligos/Downloads/senate_2022.csv', stringsAsFactors = FALSE)

atg22 <- read.csv('/Users/birdieligos/Downloads/attorneygen_2022.csv', stringsAsFactors = FALSE)

gov22 <- read.csv('/Users/birdieligos/Downloads/gov_2022.csv', stringsAsFactors = FALSE)

pres24 <- read.csv('/Users/birdieligos/Downloads/PRES - 2024.csv', stringsAsFactors = FALSE)

pres20 <- read.csv('/Users/birdieligos/Downloads/PRES - 2022.csv', stringsAsFactors = FALSE)

sen22 <- sen22 %>% select(District, DEM = 4, REP = 3)
atg22 <- atg22 %>% select(District, DEM = 3, REP = 4)
gov22 <- gov22 %>% select(District, DEM = 3, REP = 4)
pres24 <- pres24 %>% select(District, DEM = 3, REP = 4)
pres20 <- pres20 %>% select(District, DEM = 4, REP = 3)

# Merge all datasets
merged <- sen22 %>%
  rename_with(~paste0(., "_Sen22"), -District) %>%
  left_join(atg22 %>% rename_with(~paste0(., "_ATG22"), -District), by = "District") %>%
  left_join(gov22 %>% rename_with(~paste0(., "_GOV22"), -District), by = "District") %>%
  left_join(pres24 %>% rename_with(~paste0(., "_PRES24"), -District), by = "District") %>%
  left_join(pres20 %>% rename_with(~paste0(., "_PRES20"), -District), by = "District")

# Run PCA
pca_result <- prcomp(merged %>% select(-District), center = TRUE, scale. = TRUE)

# Add composite score
merged$Composite_Score <- pca_result$x[,1]

# results
pca_result$rotation
summary(pca_result)
# look at “Proportion of Variance” for PC1

library(ggplot2)
ggplot(merged, aes(Composite_Score)) +
  geom_histogram(bins = 30) +
  labs(x="Composite Partisan Score", y="Number of Districts")

write.csv(merged, file = '/Users/birdieligos/Documents/Reports/CAdistrictcomposities.csv', row.names = FALSE)

###### HYBRID STATE RELATIVE PARTISAN VOTER INDEX 


# — libs
library(dplyr)

###### UPDATED HYBRID PARTISAN SCORE SCRIPT

# — load & keep only District + two-party raw vote counts
sen22  <- read.csv('/Users/birdieligos/Downloads/senate_2022.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_Sen22 = 4, REP_Sen22 = 3)
atg22  <- read.csv('/Users/birdieligos/Downloads/attorneygen_2022.csv',  stringsAsFactors = FALSE) %>%
  select(District, DEM_ATG22 = 3, REP_ATG22 = 4)
gov22  <- read.csv('/Users/birdieligos/Downloads/gov_2022.csv',           stringsAsFactors = FALSE) %>%
  select(District, DEM_GOV22 = 3, REP_GOV22 = 4)
pres24 <- read.csv('/Users/birdieligos/Downloads/PRES - 2024.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES24 = 3, REP_PRES24 = 4)
pres20 <- read.csv('/Users/birdieligos/Downloads/PRES - 2022.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES20 = 4, REP_PRES20 = 3)

# — merge all races on District
merged <- sen22 %>%
  left_join(atg22,  by = "District") %>%
  left_join(gov22,  by = "District") %>%
  left_join(pres24, by = "District") %>%
  left_join(pres20, by = "District")

# — compute two-party Democratic share for each race
merged <- merged %>%
  mutate(
    share_pres24 = DEM_PRES24 / (DEM_PRES24 + REP_PRES24),
    share_sen22  = DEM_Sen22  / (DEM_Sen22  + REP_Sen22),
    share_gov22  = DEM_GOV22  / (DEM_GOV22  + REP_GOV22),
    share_atg22  = DEM_ATG22  / (DEM_ATG22  + REP_ATG22),
    share_pres20 = DEM_PRES20 / (DEM_PRES20 + REP_PRES20)
  )

# — exponential-decay weighting by recency (half-life = 4 years)
years     <- c(2024, 2022, 2022, 2022, 2020)
half_life <- 4
lambda    <- log(2) / half_life
w_raw     <- exp(-lambda * (2025 - years))
w         <- w_raw / sum(w_raw)
names(w)  <- c("share_pres24", "share_sen22", "share_gov22", "share_atg22", "share_pres20")

# — compute composite Democratic share and margin vs 50%
merged <- merged %>%
  mutate(
    composite_share  = as.numeric(
      as.matrix(select(., share_pres24:share_pres20)) %*% w
    ),
    composite_margin = composite_share - 0.5,
    Partisan_Label   = ifelse(
      composite_margin >= 0,
      paste0("D+", round(100 * composite_margin)),
      paste0("R+", round(100 * -composite_margin))
    )
  )

# — summaries
# weight table
w_df <- data.frame(race = names(w), weight = round(w, 3))
print(w_df)

# distribution of composite_share and composite_margin
print(summary(merged$composite_share))
print(summary(merged$composite_margin))

# view final results
results <- merged %>%
  select(District, composite_share, composite_margin, Partisan_Label)
print(head(results, 10))

write.csv(
  merged,
  '/Users/birdieligos/Documents/Reports/CA_AD_PVI_dynamic.csv',
  row.names = FALSE
)

######################################### district shift 
pres24 <- read.csv('/Users/birdieligos/Downloads/PRES - 2024.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES24 = 3, REP_PRES24 = 4)
pres20 <- read.csv('/Users/birdieligos/Downloads/PRES - 2022.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES20 = 4, REP_PRES20 = 3)

##
swing_df <- pres24 %>%
  inner_join(pres20, by = "District") %>%
  mutate(
    dem_shift = (DEM_PRES24 - REP_PRES24) - (DEM_PRES20 - REP_PRES20)
  )

ca_shift <- (sum(pres24$DEM_PRES24) - sum(pres24$REP_PRES24)) -
  (sum(pres20$DEM_PRES20) - sum(pres20$REP_PRES20))

swing_df <- pres24 %>%
  inner_join(pres20, by = "District") %>%
  mutate(
    dem_shift    = (DEM_PRES24 - REP_PRES24) - (DEM_PRES20 - REP_PRES20),
    swing_ratio  = 100 * dem_shift / ca_shift
  )

# summary
summary(swing_df$swing_ratio)


####### — output
write.csv(
  swing_df,
  '/Users/birdieligos/Documents/Reports/CA_AD_shift.csv',
  row.names = FALSE
)



###### pres shift 


