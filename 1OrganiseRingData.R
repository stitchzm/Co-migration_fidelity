## Dataset 24 trans-Saharian species 
# Import ring data 2002-2020 + 2021 + 2022 + 2023 + 2024 
p20 <- read.csv("data/Ponza2002-2020.csv")
p21 <- read.csv("data/Ponza2021_norecapture.csv")
p22 <- read.csv("data/Ponza2022.csv")
p23 <- read.csv("data/Ponza2023.csv")
p24 <- read.csv("data/Ponza2024_norecapture.csv")
library(dplyr)

# Check categories 
p20$AGE <- as.integer(p20$AGE)
p21$AGE <- as.integer(p21$AGE)
p22$AGE <- as.integer(p22$AGE)
p23$AGE <- as.integer(p23$AGE)
p24$AGE <- as.integer(p24$AGE)

# Merge in one dataset 
alldata <- bind_rows(p20,p21,p22,p23,p24)
alldata <- alldata %>%
  mutate(
    DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-")),
    EURING = as.character(EURING))

# DATA CHECK #####
#********************************************************************************
# Check for duplicates in Ring 
anyDuplicated(alldata$RING) 
sum(duplicated(alldata$RING)) # 79281

dup_alldata <- alldata[duplicated(alldata$RING) | duplicated(alldata$RING, fromLast = TRUE), ]
duplicates <- alldata %>%
  group_by(RING, DATE) %>%
  filter(n() > 1)

# Remove rows where RING = 0 
alldata <- alldata %>%
  filter(RING != 0)

# Remove duplicate captures of the same individual on the same day
alldata_clean <- alldata %>%
  distinct(RING, DATE, .keep_all = TRUE)

# Remove ring/species conflicts
conflicts <- alldata_clean %>%
  group_by(RING) %>%
  filter(n_distinct(EURING) > 1)

alldata_clean <- alldata_clean %>%
  filter(!RING %in% conflicts$RING)

# # Retain only the first capture of each ringed individual to remove recaptures
alldata <- alldata_clean %>%
  arrange(RING, DATE) %>%
  distinct(RING, .keep_all = TRUE)

#* Subset data from 2007 
alldata <- alldata[alldata$YEAR >= 2007, ]
#********************************************************************************
# Select studied species for each dataset by using Euring code
selected_codes <- c(6870,8400,8460,8480,9920,10010,10090,11040,11220,11370,
                    11460,12430,12530,12590,12650,12750,12760,13080,13120,
                    13350,13480,13490,15080,15230)

alldata <- alldata[alldata$EURING %in% selected_codes, ]

species_lookup <- data.frame(
  EURING = c(6870, 8400, 8460, 8480, 9920, 10010, 10090, 11040, 11220, 11370,
             11460, 12430, 12530, 12590, 12650, 12750, 12760, 13080, 13120,
             13350, 13480, 13490, 15080, 15230),
  SPS = c("TD","BE","HP","WRN","BS","HM","TP","CN","RS","WC",
          "NW","SW","GRW","IW","SBW","WTR","GW","WDW","WLW",
          "SF","CF","PF","GOR","WCS"), 
  SCIENTIFIC_NAME = c("Streptopelia turtur","Merops apiaster","Upupa epops","Jynx torquilla",
                   "Hirundo rustica","Delichonurbica","Anthus trivialis",
                   "Luscinia megarhynchos","Phoenicurus phoenicurus","Saxicola rubetra",
                   "Oenanthe oenanthe","Acrocephalus schoenobaenus","Acrocephalus arundinaceus",
                   "Hippolais icterina","Curruca cantillans","Curruca communis","Sylvia borin",
                   "Phylloscopus sibilatrix","Phylloscopus trochilus",
                   "Muscicapa striata","Ficedula albicollis","Ficedula hypoleuca",
                   "Oriolus oriolus","Lanius senator"))

species_lookup <- species_lookup %>%
  mutate(EURING = as.character(EURING))

# ADD DATA ####
#********************************************************************************
# Add species columns 
p24species <- alldata %>%
  left_join(species_lookup, by = "EURING")

#********************************************************************************
###Calculate and add the Julian day
p24species$DAY = as.numeric(p24species$DAY)
p24species$MONTH = as.numeric(p24species$MONTH)
p24species$Julian <- do.call(paste, list(p24species$MONTH, p24species$DAY, p24species$YEAR))
p24species$Julian <- as.Date(p24species$Julian, format=c("%m %d %Y"))
p24species$JDays <- as.numeric(format(p24species$Julian, "%j"))

# order columns
p24species <- p24species[,c(1,2,19,3:6,8,16,17,7,9:15)]

# count individuals per species per year
species_year_counts <- p24species %>%
  group_by(YEAR, SCIENTIFIC_NAME) %>%
  summarise(N_individuals = n(), .groups = "drop")

complete_counts <- species_year_counts %>%
  complete(YEAR = 2007:2024, SCIENTIFIC_NAME, fill = list(N_individuals = 0))

library(ggplot2)
ggplot(complete_counts, aes(x = YEAR, y = N_individuals)) +
  geom_line() +
  facet_wrap(~SCIENTIFIC_NAME, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Species abundance trends (2007–2024)",
    x = "Year",
    y = "Abundance"
  )

# save and export data that will be used to calculate the daily abundance for each species
