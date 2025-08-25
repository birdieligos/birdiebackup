# — libs
library(dplyr)

###### UPDATED HYBRID PARTISAN SCORE SCRIPT

# — load & keep only District + two-party raw vote counts
sen22  <- read.csv('/Users/birdieligos/Downloads/kcaw cd - Senate.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_Sen22 = 3, REP_Sen22 = 4)
atg22  <- read.csv('/Users/birdieligos/Downloads/kcaw cd - attorney general.csv',  stringsAsFactors = FALSE) %>%
  select(District, DEM_ATG22 = 3, REP_ATG22 = 4)
gov22  <- read.csv('/Users/birdieligos/Downloads/kcaw cd - gov.csv',           stringsAsFactors = FALSE) %>%
  select(District, DEM_GOV22 = 3, REP_GOV22 = 4)
pres24 <- read.csv('/Users/birdieligos/Downloads/kcaw cd - pres24.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES24 = 3, REP_PRES24 = 4)
pres20 <- read.csv('/Users/birdieligos/Downloads/kcaw cd - pres20.csv',        stringsAsFactors = FALSE) %>%
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
  '/Users/birdieligos/Documents/Reports/CA_CD_PVI_dynamic.csv',
  row.names = FALSE
)

######################################### district shift 
pres24 <- read.csv('/Users/birdieligos/Downloads/kcaw cd - pres24.csv',        stringsAsFactors = FALSE) %>%
  select(District, DEM_PRES24 = 3, REP_PRES24 = 4)
pres20 <- read.csv('/Users/birdieligos/Downloads/kcaw cd - pres20.csv',        stringsAsFactors = FALSE) %>%
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
  '/Users/birdieligos/Documents/Reports/CA_CD_shift.csv',
  row.names = FALSE
)
