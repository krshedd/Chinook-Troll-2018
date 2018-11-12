#### Extraction Lists ####
# AY2018
# Kyle Shedd
# Created Wed Apr 18 15:02:57 2018
date()
rm(list = ls(all.names = TRUE))

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK18")
source("C:/Users/krshedd/R/Functions.GCL.R")
library(tidyverse)
library(lubridate)

# dir.create("Extraction Lists")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Winter ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Planning to run each Quad as it's own mixture and stratify from there
# Business rule is to take fish from within 2 SW on either side to fill in for missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
ASL.df <- read_csv(file = "ASL Data/20180416_Harvest - Detailed ASL Samples.csv")
str(ASL.df, give.attr = FALSE)  # District = Quadrant

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
# Year as factor, rename Quadrant
ASL.df <- ASL.df %>% 
  mutate(Year_f = factor(Year)) %>% 
  rename(Quadrant = District)
  
# Create a variable for Fishery
ASL.df <- ASL.df %>% 
  mutate(Fishery = case_when(Harvest == "Spring Troll Fishery" ~ "Spring",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter")))

#~~~~~~~~~~~~~~~~~~
## Visualize ASL data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_bar` (we know that there is 1 row per DNA sample)
ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_wrap(~ Year_f) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week for Winter AY18")

# # Same plots, different method of coding
# # Using dplyr `count`with ggplot2 `geom_col`
# ASL.df %>% 
#   filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
#   count(!(is.na(`Dna Specimen No`)), Year_f, `Stat Week`, Fishery) %>% 
#   ggplot(aes(x = `Stat Week`, y = n, color = Fishery)) +
#   geom_col() +
#   facet_wrap(~ Year_f) +
#   ylab("# DNA Samples")
# # Using dplyr `group_by` and `summarise` with ggplot2 `geom_col`
# ASL.df %>% 
#   filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
#   select(Year, `Stat Week`, Fishery, `Dna Specimen No`) %>% 
#   group_by(Year, `Stat Week`, Fishery) %>% 
#   summarise(Samples = sum(!is.na(`Dna Specimen No`))) %>% 
#   ggplot(aes(x = `Stat Week`, y = Samples, color = Fishery)) +
#   geom_col() +
#   facet_wrap(~ Year) +
#   xlab("Stat Week")

# Plot samples by Stat Week and Quadrant
# Using ggplot2 `geom_col` to plot harvest (identity)
ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_grid(Quadrant ~ Year_f) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week and Quadrant for Winter AY18")

# Table of Samples by Fishery/Quadrant
ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, Quadrant, Fishery) %>% 
  spread(Quadrant,  n)
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in Harvest data
harvest.df <- read_csv(file = "Harvest Data/CE002350.csv", skip = 22)
str(harvest.df, give.attr = FALSE)  # Area Value = District, Time Value = Stat Week

#~~~~~~~~~~~~~~~~~~
## Manipulate harvest data
# Year as factor, rename Stat Week, rename District
harvest.df <- harvest.df %>% 
  mutate(Year_f = factor(Year)) %>% 
  rename("Stat Week" = `Time Value`, District = `Area Value`)

# Create a variable for Fishery
harvest.df <- harvest.df %>% 
  mutate(Fishery = case_when(Harvest == "SP TROLL" ~ "Spring",
                             Harvest == "TRAD" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "TRAD" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "TRAD" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "TRAD" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter")))

# Create a variable for Quadrant
harvest.df <- harvest.df %>% 
  mutate(Quadrant = case_when(District %in% c(113, 114, 116, 154, 156, 157) | District >= 181 ~ 171,
                              District %in% c(103, 104, 152) ~ 172,
                              District %in% c(109, 110, 111, 112, 115) ~ 173,
                              District %in% c(101, 102, 105, 106, 107, 108) ~ 174))

#~~~~~~~~~~~~~~~~~~
## Visualize Harvest Data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_wrap(~ Year_f) +
  ggtitle("Harvest by Stat Week for Winter AY18")

# Plot samples by Stat Week and Quadrant
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_grid(Quadrant ~ Year_f) +
  ggtitle("Harvest by Stat Week and Quadrant for Winter AY18")

# Table of Harvest by Fishery/Quadrant
harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  spread(Quadrant, Harvest)

# Determine max harvest by Fishery/District/Stat Week for heatmaps
max_sw_harvest <- as.numeric(harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  summarise_at(vars(`N Catch`), max))

# Heatmap of Harvest by Stat Week and District for Early Winter
harvest.df %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Early Winter" & Year == "2017") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 41:53) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Early Winter - Harvest by Stat Week and District")
  
# Heatmap of Harvest by Stat Week and District for Late Winter
harvest.df %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 1:18) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Late Winter - Harvest by Stat Week and District")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join ASL and Harvest data by Quad by SW
# Roll up harvest to Quad level
harvest_yr_sw_fishery_quad.df <- harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`))

# Roll up ASL to SW and Quad level, join with harvest
harvest_ASL_join.df <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0))  # replace NA in samples and harvest with 0


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Early Winter Selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion with managers and PST folks indicated that they want as fine of scale data as possible
# However, this is what we have for samples
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017") %>%
  group_by(Quadrant) %>% 
  summarise(Samples = sum(n)) %>% 
  spread(Quadrant, Samples)

# Plot samples and harvest together as proportions
harvest_ASL_join.df %>% 
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Early Winter") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Samples and Harvest by Stat Week and Quadrant for Early Winter AY18")

# Thus the plan for extraction is to pick ~200 for 171 (NO) and run everything else
# With the important caveat of subsampling in proportion to harvest by SW for each quadrant
# Business rule is to take fish from within 2 SW on either side to fill in for missing


#~~~~~~~~~~~~~~~~~~
## 171
# Subsample 200 fish
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 171) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 200)) %>%  # if we want 200 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 51 and 52 up to earlier SW with more samples
extraction_EW_171 <- data_frame('Stat Week' = 41:51,
                                n = c(29, 25, 19, 33, 17, 7, 14, 10, 14, 30, 2)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
EW_171_torun <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 171) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_EW_171, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
EW_171_torun %>% 
  count(`Stat Week`)
  

#~~~~~~~~~~~~~~~~~~
## 172
# Run all 16 fish if possible
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 172) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * sum(n))) %>%  # if we want all samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 43, 44 to 45 and 48 to 47
extraction_EW_172 <- data_frame('Stat Week' = c(41:50, 52),
                                n = c(0, 0, 1, 0, 2, 1, 2, 2, 5, 1, 1)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
EW_172_torun <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 172) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_EW_172, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
EW_172_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## 173
# Run all 57 fish if possible, downgraded to 40 to get better temporal samples
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 173) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 40)) %>%  # if we want 40 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 49, 50 to 48, ignore missing fish from SW 52
extraction_EW_173 <- data_frame('Stat Week' = c(41:52),
                                n = c(12, 9, 4, 6, 2, 1, 1, 3, 0, 0, 1, 0)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
EW_173_torun <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 173) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_EW_173, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
EW_173_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## 174
# Run all 91 fish if possible, downgraded to 64 to get better temporal samples
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 174) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 64)) %>%  # if we want 64 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 47 to 46 and 49 to 50
extraction_EW_174 <- data_frame('Stat Week' = c(41:52),
                                n = c(15 ,14, 10, 8, 3, 4, 0, 1, 2, 3, 4, 0)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
EW_174_torun <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 174) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_EW_174, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
EW_174_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## Create a single Early Winter Extraction data.frame
EW_torun_ASL.df <- bind_rows(EW_171_torun, EW_172_torun, EW_173_torun, EW_174_torun)
EW_torun_ASL.df %>% 
  count(Quadrant)

dput(x = EW_torun_ASL.df, file = "Objects/EW_torun_ASL.df.txt")

# Plot harvest vs. samples by SW for each Quad
EW_torun_ASL.df %>% 
  select(-n) %>% 
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0)) %>%  # replace NA in samples and harvest with 0
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Early Winter") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Extraction and Harvest by Stat Week and Quadrant for Early Winter AY18")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Late Winter Selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion with managers and PST folks indicated that they want as fine of scale data as possible
# However, this is what we have for samples
harvest_ASL_join.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018") %>%
  group_by(Quadrant) %>% 
  summarise(Samples = sum(n)) %>% 
  spread(Quadrant, Samples)

# Plot samples and harvest together as proportions
harvest_ASL_join.df %>% 
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Late Winter") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Samples and Harvest by Stat Week and Quadrant for Late Winter AY18")

# Thus the plan for extraction is to pick all for 171 (NO) 100 for 172 (SO), and 200 each for 173 and 174 (NI, SI)
# With the important caveat of subsampling in proportion to harvest by SW for each quadrant
# Business rule is to take fish from within 2 SW on either side to fill in for missing


#~~~~~~~~~~~~~~~~~~
## 171
# Run all 191 fish if possible, downgraded to 140 to get better temporal samples
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 171) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 140)) %>%  # if we want 140 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 1 to 2; 3, 5, and 6 to 4; 8 to 7; 8 and 9 to 10.
extraction_LW_171 <- data_frame('Stat Week' = 1:11,
                                n = c(0, 8, 3, 36, 3, 16, 16, 15, 2, 26, 13)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
LW_171_torun <- ASL.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 171) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_LW_171, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
LW_171_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## 172
# Subsample 100 fish, downgraded to 90 to get better temporal samples
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 172) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 90)) %>%  # if we want 90 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 5 and 6 to 4; 7 to 8.
extraction_LW_172 <- data_frame('Stat Week' = c(1:2, 4:11),
                                n = c(5, 8, 14, 11, 6, 6, 12, 1, 15, 10)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
LW_172_torun <- ASL.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 172) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_LW_172, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
LW_172_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## 173
# Subsample 200 fish, downgraded to 115 to get better temporal samples
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 173) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 115)) %>%  # if we want 115 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 1 to 2; 3 to 4; 5 to 4 and 6; 7 to 6 and 8; 9 to 10
extraction_LW_173 <- data_frame('Stat Week' = c(1:11),
                                n = c(1, 1, 0, 17, 2, 31, 4, 10, 0, 19, 29)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
LW_173_torun <- ASL.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 173) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_LW_173, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
LW_173_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## 174
# Subsample 140 fish
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 174) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 140)) %>%  # if we want 140 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Need to move some extra fish missing from SW 1 to 2; 3 to 4; 6 to 5; 7 to 8 and 9.
extraction_LW_174 <- data_frame('Stat Week' = c(1:11),
                                n = c(2, 17, 1, 12, 19, 1, 9, 17, 17, 26, 18)) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
LW_174_torun <- ASL.df %>% 
  filter(Fishery == "Late Winter" & Year_f == "2018" & Quadrant == 174) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_LW_174, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
LW_174_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## Create a single Late Winter Extraction data.frame
LW_torun_ASL.df <- bind_rows(LW_171_torun, LW_172_torun, LW_173_torun, LW_174_torun)
LW_torun_ASL.df %>% 
  count(Quadrant)

dput(x = LW_torun_ASL.df, file = "Objects/LW_torun_ASL.df.txt")

# Plot harvest vs. samples by SW for each Quad
LW_torun_ASL.df %>% 
  select(-n) %>% 
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0)) %>%  # replace NA in samples and harvest with 0
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Late Winter") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Extraction and Harvest by Stat Week and Quadrant for Late Winter AY18")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Write Winter Extraction List ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Format into the Extraction List Template
load_objects("Objects")

# Combine Early and Late Winter into one list
Winter_torun_ASL.df <- bind_rows(EW_torun_ASL.df, LW_torun_ASL.df)

# Confirm that all `Dna Specimen No` are 6 characters before splitting
table(nchar(Winter_torun_ASL.df$`Dna Specimen No`))

# Unfortunately there are a mix of 100000XXXX and 000000XXXX WGCs in this year's samples
# So the `Dna Specimen No` isn't enough for me to figure out the whole 10 digit WGC number
# First check and verify that there are no potential "duplicate" cards (i.e. cards with the same last 4 digits)

# WGC numbers from Iris' summary
EW_WGC_4char <- readClipboard()
length(EW_WGC_4char) == length(unique(EW_WGC_4char))

# WGC numbers from Iris' summary
LW_WGC_4char <- readClipboard()
length(LW_WGC_4char) == length(unique(LW_WGC_4char))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull tissue collection info from OceanAK and join, need full 10 digit WGC number and Sample number
LOKI_tissue.df <- read_csv(file = "Associated Data/Winter Troll/GEN_SAMPLED_FISH_TISSUE.csv")

# Subset for variables of interest
LOKI_tissue.df <- LOKI_tissue.df %>% 
  select(`Silly Code`, FK_FISH_ID, DNA_TRAY_CODE, DNA_TRAY_WELL_CODE, PK_TISSUE_TYPE)

## Are all my extraction fish in the LOKI tissue table?
table(Winter_torun_ASL.df$`Dna Specimen No` %in% LOKI_tissue.df$FK_FISH_ID)  # 1 FALSE

# No, which one
setdiff(Winter_torun_ASL.df$`Dna Specimen No`, LOKI_tissue.df$FK_FISH_ID)  # 549901

# Check with Iris' summary to figure out what correct value is
Winter_torun_ASL.df %>% 
  filter(`Dna Specimen No` == 549901)  # KTN SW 49 Quad 174; should be 1000004599

# Change value (both of these work)
Winter_torun_ASL.df <- Winter_torun_ASL.df %>% 
  mutate(`Dna Specimen No` = replace(`Dna Specimen No`, `Dna Specimen No` == 549901, 459901))

Winter_torun_ASL.df <- Winter_torun_ASL.df %>% 
  mutate(`Dna Specimen No` = recode(`Dna Specimen No`, `549901` = 459901L))

## Join LOKI Tissue Table with ASL and format for Extraction List Template
Winter_torun_extraction.df <- Winter_torun_ASL.df %>% 
  left_join(LOKI_tissue.df, by = c(`Dna Specimen No` = "FK_FISH_ID")) %>% 
  mutate(`WELL CODE` = str_pad(DNA_TRAY_WELL_CODE, width = 2, side = "left", pad = 0)) %>% 
  mutate(`TISSUE TYPE` = str_replace(PK_TISSUE_TYPE, pattern = " Process", replacement = "")) %>% 
  mutate(`TISSUE TYPE` = str_replace(`TISSUE TYPE`, pattern = " Clip", replacement = "")) %>% 
  rename(SILLY = `Silly Code`, `SAMPLE #` = `Dna Specimen No`, `WGC BARCODE` = `DNA_TRAY_CODE`) %>% 
  select(SILLY, `SAMPLE #`, `WGC BARCODE`, `WELL CODE`, `TISSUE TYPE`) %>% 
  arrange(SILLY, `WGC BARCODE`, `WELL CODE`)

write_csv(Winter_torun_extraction.df, path = "Extraction Lists/Winter_Extraction.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### TBR D108/D111 ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Drift Gillnet
### Only select TBR fish, even if this means cherry picking fish off of cards
# Only large fish (>=660mm, SW 17-29, District 108 and 111)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
gill_ASL.tib <- read_csv(file = "ASL Data/20180829_D108_D111_Drift_Harvest - Detailed ASL Samples.csv")
str(gill_ASL.tib, give.attr = FALSE)

table(gill_ASL.tib$Harvest)
table(gill_ASL.tib$`Stat Week`, gill_ASL.tib$District)

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
TBR_gill_ASL.tib <- gill_ASL.tib %>%
  filter(`Stat Week` >= 17 & `Stat Week` <= 29) %>%  # stat week 17-29
  filter(`Average Length mm` >= 660) %>%  # large fish only
  filter(!is.na(`Dna Specimen No`)) # has DNA sample
  
TBR_gill_ASL.tib %>% 
  count(District)

save_objects(objects = "TBR_gill_ASL.tib", path = "Objects")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport
### Only select TBR fish, even if this means cherry picking fish off of cards
# Only large fish (>=660mm, SW 17-29, District 108 and 111)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
sport_ASL.tib <- read_csv(file = "ASL Data/_2018_TBR_SEAK_SF_Genetic_AWL_31JUL18.csv")
str(sport_ASL.tib, give.attr = FALSE)

table(sport_ASL.tib$STATWEEK, sport_ASL.tib$DISTRICT)

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
TBR_sport_ASL.tib <- sport_ASL.tib %>%
  filter(DISTRICT == 111) %>%  # district 111
  filter(STATWEEK >= 17 & STATWEEK <= 29) %>%  # stat week 17-29
  filter(LENGTH >= 660) %>%  # large fish only
  filter(!is.na(Whatman_Card)) # has DNA sample

TBR_sport_ASL.tib %>% 
  count(DISTRICT)

save_objects(objects = "TBR_sport_ASL.tib", path = "Objects")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Write TBR Extraction List ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Format into the Extraction List Template
load_objects("Objects")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Drift Gillnet
# Confirm that all `Dna Specimen No` are 6 characters before splitting
table(nchar(TBR_gill_ASL.tib$`Dna Specimen No`))

# Unfortunately there are a mix of 100000XXXX and 000000XXXX WGCs in this year's samples
# So the `Dna Specimen No` isn't enough for me to figure out the whole 10 digit WGC number
# First check and verify that there are no potential "duplicate" cards (i.e. cards with the same last 4 digits)

# WGC numbers from Iris' summary
TBR_gill_WGC_4char <- readClipboard()
length(TBR_gill_WGC_4char) == length(unique(TBR_gill_WGC_4char))  # TRUE


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull tissue collection info from OceanAK and join, need full 10 digit WGC number and Sample number
gill_LOKI_tissue.df <- read_csv(file = "Associated Data/TBR/GEN_SAMPLED_FISH_TISSUE.csv") %>% 
  select(`Silly Code`, FK_FISH_ID, DNA_TRAY_CODE, DNA_TRAY_WELL_CODE, PK_TISSUE_TYPE) %>%  # subset
  mutate(dna_specimen_no = as.numeric(paste0(str_sub(DNA_TRAY_CODE, 7, 10), str_pad(DNA_TRAY_WELL_CODE, 2, "left", "0"))))  # create 6 digit dna_specimen_no

## Are all my extraction fish in the LOKI tissue table?
table(TBR_gill_ASL.tib$`Dna Specimen No` %in% gill_LOKI_tissue.df$dna_specimen_no)  # 1 FALSE

# No, which one
setdiff(TBR_gill_ASL.tib$`Dna Specimen No`, gill_LOKI_tissue.df$dna_specimen_no)  # 433703

## Join LOKI Tissue Table with ASL and format for Extraction List Template
TBR_gill_extraction.tib <- TBR_gill_ASL.tib %>% 
  inner_join(gill_LOKI_tissue.df, by = c(`Dna Specimen No` = "dna_specimen_no")) %>% 
  mutate(`WELL CODE` = str_pad(DNA_TRAY_WELL_CODE, width = 2, side = "left", pad = 0)) %>% 
  mutate(`TISSUE TYPE` = str_replace(PK_TISSUE_TYPE, pattern = " Process", replacement = "")) %>% 
  mutate(`TISSUE TYPE` = str_replace(`TISSUE TYPE`, pattern = " Clip", replacement = "")) %>% 
  rename(SILLY = `Silly Code`, `SAMPLE #` = `FK_FISH_ID`, `WGC BARCODE` = `DNA_TRAY_CODE`) %>% 
  select(SILLY, `SAMPLE #`, `WGC BARCODE`, `WELL CODE`, `TISSUE TYPE`) %>% 
  arrange(SILLY, `WGC BARCODE`, `WELL CODE`)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport
# Confirm that all Whatman Card are 10 characters before combining
table(nchar(TBR_sport_ASL.tib$Whatman_Card))
TBR_sport_ASL.tib <- TBR_sport_ASL.tib %>% 
  mutate(Whatman_Card = str_pad(Whatman_Card, 10, "left", "0")) %>%  # pad WGC
  mutate(SAMPLE_NO = str_pad(SAMPLE_NO, 2, "left", "0")) %>%  # pad well
  unite(silly_source, c("Whatman_Card", "SAMPLE_NO"), sep = "_", remove = FALSE)  # silly_source
  
# Unfortunately there are a mix of 100000XXXX and 000000XXXX WGCs in this year's samples

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull tissue collection info from OceanAK and join, need full 10 digit WGC number and Sample number
sport_LOKI_tissue.df <- read_csv(file = "Associated Data/Sport/GEN_SAMPLED_FISH_TISSUE.csv") %>% 
  select(`Silly Code`, FK_FISH_ID, DNA_TRAY_CODE, DNA_TRAY_WELL_CODE, PK_TISSUE_TYPE) %>%  # subset
  mutate(DNA_TRAY_WELL_CODE = str_pad(DNA_TRAY_WELL_CODE, 2, "left", "0")) %>%  # pad well
  unite(silly_source, c("DNA_TRAY_CODE", "DNA_TRAY_WELL_CODE"), sep = "_", remove = FALSE)  # silly_source

## Are all my extraction fish in the LOKI tissue table?
table(TBR_sport_ASL.tib$silly_source %in% sport_LOKI_tissue.df$silly_source)  # all TRUE

## Join LOKI Tissue Table with ASL and format for Extraction List Template
TBR_sport_extraction.tib <- TBR_sport_ASL.tib %>% 
  inner_join(sport_LOKI_tissue.df, by = c("silly_source")) %>% 
  mutate(`WELL CODE` = str_pad(DNA_TRAY_WELL_CODE, width = 2, side = "left", pad = 0)) %>% 
  mutate(`TISSUE TYPE` = str_replace(PK_TISSUE_TYPE, pattern = " Process", replacement = "")) %>% 
  mutate(`TISSUE TYPE` = str_replace(`TISSUE TYPE`, pattern = " Clip", replacement = "")) %>% 
  rename(SILLY = `Silly Code`, `SAMPLE #` = `FK_FISH_ID`, `WGC BARCODE` = `DNA_TRAY_CODE`) %>% 
  select(SILLY, `SAMPLE #`, `WGC BARCODE`, `WELL CODE`, `TISSUE TYPE`) %>% 
  arrange(SILLY, `WGC BARCODE`, `WELL CODE`)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Write joint extraction list
TBR_extraction.tib <- bind_rows(TBR_gill_extraction.tib, TBR_sport_extraction.tib)
write_csv(TBR_extraction.tib, path = "Extraction Lists/TBR_Extraction.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summer ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
date()  # Fri Oct 26 07:26:11 2018
# Making lists for Summer 1 / 2 before Spring because summer is still by quad
# thus, it is easier to deal with than Spring (by Stat Area!)

# Planning to run each Quad as it's own mixture and stratify from there
# Business rule is to take fish from within 2 SW on either side to fill in for missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
asl_summer <- read_csv(file = "ASL Data/20181025_Harvest - Detailed ASL Samples.csv")
str(asl_summer, give.attr = FALSE)  # District = Quadrant
problems(asl_summer)

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
# Filter for just Traditional and Terminal, only DNA sampled
asl_summer %>% 
  count(Harvest)

asl_summer <- asl_summer %>% 
  filter(Harvest %in% c("Spring Troll Fishery", "Traditional State Managed Fisheries")) %>% 
  filter(!is.na(`Dna Specimen No`))
  
asl_summer %>% 
  count(Harvest, District) %>% 
  spread(District, n)

# Year as factor, rename Quadrant
asl_summer <- asl_summer %>% 
  mutate(Year_f = factor(Year)) %>% 
  rename(Quadrant = District)

# Create a variable for Fishery
# Investigate sample date for spring to determine May/June cutoff
asl_summer %>% 
  filter(Harvest == "Spring Troll Fishery") %>% 
  count(`Sample Date`) %>% 
  print(n = 44)
# Based on spring openings, I know that the fishery was closed 5/31-6/4 so fish sampled 6/1 where harvested in May

asl_summer %>% 
  filter(Harvest == "Traditional State Managed Fisheries") %>% 
  count(`Stat Week`) %>% 
  print(n = 100)

asl_summer <- asl_summer %>% 
  mutate(Fishery = case_when(Harvest == "Spring Troll Fishery" & `Sample Date` <= "2018-06-01" ~ "Spring 1",
                             Harvest == "Spring Troll Fishery" & `Sample Date` > "2018-06-01" ~ "Spring 2",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring 1", "Spring 2", "Summer Ret 1", "Summer Ret 2", "Early Winter")))

asl_summer %>% 
  filter(!is.na(`Dna Specimen No`)) %>% 
  count(Fishery, Quadrant) %>% 
  spread(Quadrant, n, fill = 0)

#~~~~~~~~~~~~~~~~~~
## Visualize ASL data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_bar` (we know that there is 1 row per DNA sample)
asl_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_wrap(~ Year_f) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week for Summer AY18")

# Plot samples by Stat Week and Quadrant
asl_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_grid(Quadrant ~ Year_f) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week and Quadrant for Summer AY18")

# Table of Samples by Fishery/Quadrant
asl_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, Quadrant, Fishery) %>% 
  spread(Quadrant,  n)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in Harvest data
# By Period and Quad
harvest_summer_per_quad <- read_csv(file = "Harvest Data/CE005859.csv", skip = 22)
str(harvest_summer_per_quad, give.attr = FALSE)  # Area Value = District, Time Value = Stat Week
problems(harvest_summer_per_quad)

harvest_summer_per_quad %>% 
  filter(`Time Value` >= 4 & `Time Value` < 6) %>% 
  group_by(`Time Value`, `Area Value`) %>% 
  summarize(harvest = sum(`N Catch`)) %>% 
  spread(`Area Value`, harvest, fill = 0)

# By Stat Week and District
harvest_summer <- read_csv(file = "Harvest Data/CE005860.csv", skip = 22)
str(harvest_summer, give.attr = FALSE)  # Area Value = District, Time Value = Stat Week
problems(harvest_summer)

#~~~~~~~~~~~~~~~~~~
## Manipulate harvest data
# Year as factor, rename Stat Week, rename District
harvest_summer <- harvest_summer %>% 
  mutate(Year_f = factor(Year)) %>% 
  rename("Stat Week" = `Time Value`, District = `Area Value`) %>% 
  replace_na(list(`N Catch` = 0))

# Create a variable for Fishery
harvest_summer %>% 
  filter(Harvest == "TRAD") %>% 
  count(`Stat Week`) %>% 
  print(n = 100)

harvest_summer <- harvest_summer %>% 
  mutate(Fishery = case_when(Harvest == "SP TROLL" & `Stat Week` <= 22 ~ "Spring 1",
                             Harvest == "SP TROLL" & `Stat Week` > 22 ~ "Spring 2",
                             Harvest == "TRAD" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "TRAD" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "TRAD" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "TRAD" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring 1", "Spring 2", "Summer Ret 1", "Summer Ret 2", "Early Winter")))

# Create a variable for Quadrant
harvest_summer <- harvest_summer %>% 
  mutate(Quadrant = case_when(District %in% c(113, 114, 116, 154, 156, 157) | District >= 181 ~ 171,
                              District %in% c(103, 104, 152) ~ 172,
                              District %in% c(109, 110, 111, 112, 115) ~ 173,
                              District %in% c(101, 102, 105, 106, 107, 108) ~ 174))

# Table of Harvest by Fishery/Quadrant
harvest_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  group_by(Year_f, Quadrant, Fishery) %>% 
  summarize(harvest = sum(`N Catch`)) %>% 
  spread(Quadrant,  harvest, fill = 0)

# Verify that I summarized Stat Week/District correctly
harvest_summer_per_quad %>% 
  filter(`Time Value` >= 4 & `Time Value` < 6) %>% 
  group_by(`Time Value`, `Area Value`) %>% 
  summarize(harvest = sum(`N Catch`)) %>% 
  spread(`Area Value`, harvest, fill = 0)

#~~~~~~~~~~~~~~~~~~
## Visualize Harvest Data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  group_by(Year_f, `Stat Week`, Fishery) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_wrap(~ Year_f) +
  ggtitle("Harvest by Stat Week for Summer AY18")

# Plot samples by Stat Week and Quadrant
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_grid(Quadrant ~ Year_f) +
  ggtitle("Harvest by Stat Week and Quadrant for Summer AY18")

# Table of Harvest by Fishery/Quadrant
harvest_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  group_by(Year_f, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  spread(Quadrant, Harvest)

# Determine max harvest by Fishery/District/Stat Week for heatmaps
max_sw_harvest <- as.numeric(harvest_summer %>% 
                               filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
                               summarise_at(vars(`N Catch`), max))

# Heatmap of Harvest by Stat Week and District for Summer 1
harvest_summer %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Summer Ret 1") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 27:29) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Summer Ret 1 - Harvest by Stat Week and District")

# Heatmap of Harvest by Stat Week and District for Summer 2
harvest_summer %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Summer Ret 2") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 33:34) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Summer Ret 2 - Harvest by Stat Week and District")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join ASL and Harvest data by Quad by SW
# Roll up harvest to Quad level
harvest_yr_sw_fishery_quad <- harvest_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`))

# Roll up ASL to SW and Quad level, join with harvest
join_summer <- asl_summer %>% 
  filter(Fishery %in% c("Summer Ret 1", "Summer Ret 2")) %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0))  # replace NA in samples and harvest with 0


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summer Ret 1 Selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion with managers and PST folks indicated that they are content with quad level info for summer
# The BoF isn't going to mess with summer
# However, this is what we have for samples
join_summer %>% 
  filter(Fishery == "Summer Ret 1") %>%
  group_by(Quadrant) %>% 
  summarise(Samples = sum(n)) %>% 
  spread(Quadrant, Samples)

# Plot samples and harvest together as proportions
join_summer %>% 
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Summer Ret 1") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Samples and Harvest by Stat Week and Quadrant for Summer Ret 1 AY18")

# Thus the plan for extraction is to pick ~380 for 171 (NO)
# With the important caveat of subsampling in proportion to harvest by SW for each quadrant
# Business rule is to take fish from within 2 SW on either side to fill in for missing


#~~~~~~~~~~~~~~~~~~
## 171
# Subsample 380 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 171) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 380)) %>%  # if we want 380 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU1_171 <- tribble(
  ~`Stat Week`, ~n,
  27,           198,
  28,           182
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU1_171_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 171) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU1_171, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU1_171_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 172
# Subsample 220 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 172) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 220)) %>%  # if we want 220 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU1_172 <- tribble(
  ~`Stat Week`, ~n,
  27,           87,
  28,           133
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU1_172_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 172) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU1_172, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU1_172_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 173
# Subsample 50 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 173) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 50)) %>%  # if we want 50 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU1_173 <- tribble(
  ~`Stat Week`, ~n,
  27,           29,
  28,           21
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU1_173_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 173) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU1_173, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU1_173_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 174
# Subsample 50 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 174) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 50)) %>%  # if we want 50 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU1_174 <- tribble(
  ~`Stat Week`, ~n,
  27,           20,
  28,           30
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU1_174_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 1" & Quadrant == 174) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU1_174, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU1_174_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## Create a single Early Winter Extraction data.frame
SU1_torun_asl <- bind_rows(SU1_171_torun, SU1_172_torun, SU1_173_torun, SU1_174_torun)
SU1_torun_asl %>% 
  count(Quadrant)

save_objects("SU1_torun_asl", path = "Objects")

# Plot harvest vs. samples by SW for each Quad
SU1_torun_asl %>% 
  select(-n) %>% 
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0)) %>%  # replace NA in samples and harvest with 0
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Summer Ret 1") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Extraction and Harvest by Stat Week and Quadrant for Summer Ret 1 AY18")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summer Ret 2 Selection ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion with managers and PST folks indicated that they are content with quad level info for summer
# The BoF isn't going to mess with summer
# However, this is what we have for samples
join_summer %>% 
  filter(Fishery == "Summer Ret 2") %>%
  group_by(Quadrant) %>% 
  summarise(Samples = sum(n)) %>% 
  spread(Quadrant, Samples)

# Plot samples and harvest together as proportions
join_summer %>% 
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Summer Ret 2") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Samples and Harvest by Stat Week and Quadrant for Summer Ret 2 AY18")

# Thus the plan for extraction is to pick ~220 for 171 (NO)
# With the important caveat of subsampling in proportion to harvest by SW for each quadrant
# Business rule is to take fish from within 2 SW on either side to fill in for missing


#~~~~~~~~~~~~~~~~~~
## 171
# Subsample 220 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 171) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 220)) %>%  # if we want 220 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU2_171 <- tribble(
  ~`Stat Week`, ~n,
  33,           96,
  34,           124
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU2_171_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 171) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU2_171, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU2_171_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 172
# Subsample 120 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 172) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 120)) %>%  # if we want 120 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU2_172 <- tribble(
  ~`Stat Week`, ~n,
  33,           31,
  34,           89
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU2_172_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 172) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU2_172, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU2_172_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 173
# Subsample 50 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 173) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 50)) %>%  # if we want 50 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU2_173 <- tribble(
  ~`Stat Week`, ~n,
  33,           18,
  34,           32
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU2_173_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 173) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU2_173, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU2_173_torun %>% 
  count(`Stat Week`)

#~~~~~~~~~~~~~~~~~~
## 174
# Subsample 50 fish
# What does proportional sampling look like?
join_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 174) %>% 
  arrange(`Stat Week`) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 50)) %>%  # if we want 50 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest) %>% 
  mutate(n_extract = pmin(n, pHarvest))

# How many fish per week?
# Plenty of samples, so just go with n_extract
extraction_SU2_174 <- tribble(
  ~`Stat Week`, ~n,
  33,           19,
  34,           31
) %>% 
  filter(n > 0)  # can only keep rows > 0, otherwise nest doesn't work for picking fish

# Randomly pick fish
SU2_174_torun <- asl_summer %>% 
  filter(Fishery == "Summer Ret 2" & Quadrant == 174) %>% 
  nest(-`Stat Week`) %>% 
  right_join(extraction_SU2_174, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
SU2_174_torun %>% 
  count(`Stat Week`)


#~~~~~~~~~~~~~~~~~~
## Create a single Early Winter Extraction data.frame
SU2_torun_asl <- bind_rows(SU2_171_torun, SU2_172_torun, SU2_173_torun, SU2_174_torun)
SU2_torun_asl %>% 
  count(Quadrant)

save_objects("SU2_torun_asl", path = "Objects")

# Plot harvest vs. samples by SW for each Quad
SU2_torun_asl %>% 
  select(-n) %>% 
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0)) %>%  # replace NA in samples and harvest with 0
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Summer Ret 2") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "fixed") +
  ggtitle("Extraction and Harvest by Stat Week and Quadrant for Summer Ret 2 AY18")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Write Summer Extraction List ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Format into the Extraction List Template
load_objects("Objects")

# Combine Early and Late Winter into one list
summer_torun_asl <- bind_rows(SU1_torun_asl, SU2_torun_asl)

# Confirm that all `Dna Specimen No` are 6 characters before splitting
table(nchar(summer_torun_asl$`Dna Specimen No`))

summer_torun_asl %>% 
  filter(nchar(`Dna Specimen No`) == 5) %>% 
  select(`Stat Week`, Fishery, Quadrant, `Dna Specimen No`)

# Unfortunately there are a mix of 100000XXXX and 000000XXXX WGCs in this year's samples
# So the `Dna Specimen No` isn't enough for me to figure out the whole 10 digit WGC number
# First check and verify that there are no potential "duplicate" cards (i.e. cards with the same last 4 digits)

summer_torun_asl %>% 
  mutate(WGC_4digit = str_sub(`Dna Specimen No`, 1, 4)) %>%  # get last 4 digits of WGC
  group_by(WGC_4digit) %>%  # group by those 4 digits
  summarise(count = n_distinct(`Sample Date`)) %>%  # count unique sample dates
  summarise(count = max(count)) # what is the maximum number of sample dates per unique 4 digit WGC
# good to go


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull tissue collection info from OceanAK and join, need full 10 digit WGC number and Sample number
loki_tissue_summer <- read_csv(file = "Associated Data/Summer Troll/GEN_SAMPLED_FISH_TISSUE_KTROL18SU.csv")

# Subset for variables of interest
loki_tissue_summer <- loki_tissue_summer %>% 
  filter(is.na(IS_MISSING_PAIRED_DATA_EXISTS)) %>%  # make sure we aren't issing the tissue
  select(`Silly Code`, FK_FISH_ID, DNA_TRAY_CODE, DNA_TRAY_WELL_CODE, PK_TISSUE_TYPE) %>% 
  mutate(WGC_4digit = str_sub(DNA_TRAY_CODE, 7, 10)) %>% 
  mutate(WGC_2digit_pos = str_pad(string = DNA_TRAY_WELL_CODE, width = 2, side = "left", pad = 0)) %>% 
  unite(dna_specimen_no, c(WGC_4digit, WGC_2digit_pos), sep = '', remove = FALSE) %>% 
  mutate(dna_specimen_no = as.integer(dna_specimen_no))

#~~~~~~~~~~~~~~~~~~
## Are all my extraction fish in the LOKI tissue table?
table(summer_torun_asl$`Dna Specimen No` %in% loki_tissue_summer$dna_specimen_no)  # 6 FALSE

# No, which ones are missing
missing_fish <- sort(setdiff(summer_torun_asl$`Dna Specimen No`, loki_tissue_summer$dna_specimen_no))

summer_torun_asl %>% 
  filter(`Dna Specimen No` %in% missing_fish) %>% 
  select(Fishery, Quadrant, `Stat Week`, `Dna Specimen No`)

# What is the max sample n on each of these cards?
loki_tissue_summer %>% 
  filter(WGC_4digit %in% as.character(str_sub(missing_fish, 1, 4))) %>% 
  group_by(WGC_4digit) %>% 
  summarise(max = max(WGC_2digit_pos))

missing_fish

# Attempt to pick the "next fish" or "previous fish"
new_fish <- c(592007, 754622, 814304, 872501, 872505, 967322)
missing_fish - new_fish # great, no typos

# Make sure these "new fish" are not already in the extraction list
intersect(new_fish, summer_torun_asl$`Dna Specimen No`) # if not zero, modify `new_fish`

# Make sure these "new fish" exist in LOKI
setdiff(new_fish, loki_tissue_summer$dna_specimen_no) # if not zero, modify `new_fish`

# New fish asl
asl_summer_new_fish <- asl_summer %>% 
  filter(`Dna Specimen No` %in% new_fish)

#~~~~~~~~~~~~~~~~~~
## Update summer_torun_asl
summer_torun_asl_mod <- summer_torun_asl %>% 
  filter(!`Dna Specimen No` %in% missing_fish) %>% 
  bind_rows(asl_summer_new_fish)

# Make sure the "missing column is fine
table(summer_torun_asl_mod$n, useNA = "always")

# Verify that we have the correct numbers of fish per fishery/quadrant
summer_torun_asl_mod %>% 
  count(Fishery, Quadrant) %>% 
  spread(Quadrant, nn, fill = 0)

# Verify that we have the correct numbers of fish per statweek/quadrant
summer_torun_asl_mod %>% 
  count(`Stat Week`, Quadrant) %>% 
  spread(`Stat Week`, nn, fill = 0)

#~~~~~~~~~~~~~~~~~~
## Join LOKI Tissue Table with ASL and format for Extraction List Template
summer_torun_extraction <- summer_torun_asl_mod %>% 
  left_join(loki_tissue_summer, by = c(`Dna Specimen No` = "dna_specimen_no")) %>% 
  mutate(`WELL CODE` = str_pad(DNA_TRAY_WELL_CODE, width = 2, side = "left", pad = 0)) %>% 
  mutate(`TISSUE TYPE` = str_replace(PK_TISSUE_TYPE, pattern = " Process", replacement = "")) %>% 
  mutate(`TISSUE TYPE` = str_replace(`TISSUE TYPE`, pattern = " Clip", replacement = "")) %>% 
  rename(SILLY = `Silly Code`, `SAMPLE #` = FK_FISH_ID, `WGC BARCODE` = `DNA_TRAY_CODE`) %>% 
  select(SILLY, `SAMPLE #`, `WGC BARCODE`, `WELL CODE`, `TISSUE TYPE`) %>% 
  arrange(SILLY, `WGC BARCODE`, `WELL CODE`)

write_csv(summer_torun_extraction, path = "Extraction Lists/Summer_Extraction.csv")

summer_torun_extraction %>% 
  count(SILLY, `TISSUE TYPE`)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Spring ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UPDATE
# Planning to run each Quad as it's own mixture and stratify from there
# Business rule is to take fish from within 2 SW on either side to fill in for missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
spring_ASL <- read_csv(file = "ASL Data/2018 Spring Troll Chinook ASLDist Subdistrict.csv")
str(spring_ASL, give.attr = FALSE)  # District = Quadrant

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
# Year as factor and create a variable for Fishery
spring_ASL <- spring_ASL %>% 
  mutate(Year_f = factor(Year)) %>% 
  mutate(Fishery = case_when(Harvest == "Spring Troll Fishery" ~ "Spring",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "Traditional State Managed Fisheries" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter"))) %>% 
  # mutate(Month = month(mdy(`Sample Date`), label = TRUE, abbr = FALSE))  # update! anything SW22 and less is May

#~~~~~~~~~~~~~~~~~~
## Visualize ASL data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_bar` (we know that there is 1 row per DNA sample)
spring_ASL %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_grid(Quadrant ~ Year) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week for Spring AY18")


# Plot samples by Stat Week and Quadrant
# Using ggplot2 `geom_col` to plot harvest (identity)
spring_ASL %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>% 
  ggplot(aes(x = `Stat Week`, fill = Fishery)) +
  geom_bar() +
  facet_grid(Quadrant ~ Month) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Stat Week for Spring AY18")

# Table of Samples by Fishery/Quadrant
spring_ASL %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, Quadrant, Fishery, Month) %>% 
  spread(Quadrant,  n)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in Harvest data
harvest <- read_csv(file = "Harvest Data/CE005837.csv", skip = 22)
str(harvest, give.attr = FALSE)  # Area Value = District, Time Value = Stat Week

#~~~~~~~~~~~~~~~~~~
## Manipulate harvest data
# Year as factor, rename Stat Week, rename District
harvest <- harvest %>% 
  mutate(Year_f = factor(Year)) %>% 
  rename("Stat Week" = `Time Value`, "Stat Area" = `Area Value`)

# Create a variable for Fishery
harvest <- harvest %>% 
  mutate(Fishery = case_when(Harvest == "SP TROLL" ~ "Spring",
                             Harvest == "TRAD" & `Stat Week` <= 18 ~ "Late Winter",
                             Harvest == "TRAD" & `Stat Week` >= 41 ~ "Early Winter",
                             Harvest == "TRAD" & `Stat Week` >= 26 & `Stat Week` <= 31 ~ "Summer Ret 1",
                             Harvest == "TRAD" & `Stat Week` >= 32 & `Stat Week` <= 36 ~ "Summer Ret 2")) %>% 
  mutate(Fishery = factor(Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter")))

# Create a variable for District/SubDistrict
harvest <- harvest %>% 
  mutate(District = as.integer(str_sub(`Stat Area`, 1, 3)))

# Create a variable for Quadrant
harvest <- harvest %>% 
  mutate(Quadrant = case_when(District %in% c(113, 114, 116, 154, 156, 157) | District >= 181 ~ 171,
                              District %in% c(103, 104, 152) ~ 172,
                              District %in% c(109, 110, 111, 112, 115) ~ 173,
                              District %in% c(101, 102, 105, 106, 107, 108) ~ 174))

#~~~~~~~~~~~~~~~~~~
## Visualize Harvest Data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_grid(~ Year_f) +
  ggtitle("Harvest by Stat Week for Spring AY18")

# Plot samples by Stat Week and Quadrant
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = Harvest, fill = Fishery)) +
  geom_col() +
  facet_grid(Quadrant ~ Year_f) +
  ggtitle("Harvest by Stat Week and Quadrant for Spring AY18")

# Table of Harvest by Fishery/Quadrant
harvest %>% 
  filter(Fishery == "Spring" & Year == "2018") %>%
  group_by(Year_f, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  spread(Quadrant, Harvest)

# Determine max harvest by Fishery/District/Stat Week for heatmaps
max_sw_harvest <- as.numeric(harvest %>% 
                               filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
                               summarise_at(vars(`N Catch`), max))

# Heatmap of Harvest by Stat Week and District for Early Winter
harvest %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Early Winter" & Year == "2017") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 41:53) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Early Winter - Harvest by Stat Week and District")

# Heatmap of Harvest by Stat Week and District for Late Winter
harvest %>% 
  mutate(District = factor(x = District, levels = sort(unique(District)))) %>% 
  filter(Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, District) %>% 
  summarise(Harvest = sum(`N Catch`)) %>% 
  ggplot(aes(x = `Stat Week`, y = District, fill = Harvest, label = Harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_sw_harvest)) +
  scale_x_continuous(breaks = 1:18) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Late Winter - Harvest by Stat Week and District")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join ASL and Harvest data by Quad by SW
# Roll up harvest to Quad level
harvest_yr_sw_fishery_quad.df <- harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`))

# Roll up ASL to SW and Quad level, join with harvest
harvest_ASL_join.df <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0))  # replace NA in samples and harvest with 0




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sport ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
date()  # Mon Nov 12 10:23:29 2018
# Making lists for Sport (Origins) before Spring because sport is more 
# straightforward than Spring. Also, if we do sport first, we'll know how many 
# EXTRA fish we'll get for Spring, since we aren't going to hit our 2.8K goal 
# for sport

# Planning to run same mixtures as previous years and stratify from there
# Sitka
# Craig
# Ketchikan
# Petersburg/Wrangel (note some fish were already run for TBR)
# Outisde through biweek 13 (subset of Sitka and Craig)
# Outside post biweek 13 (subset of Sitka and Craig)


# Business rule is to take fish from within 1 biweek on either side to fill in 
# for missing

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
asl_sport <- read_csv(file = "ASL Data/_2018_SEAK_SF_Whatman_AWL_10OCT18.csv")
str(asl_sport, give.attr = FALSE)  # District = Quadrant
problems(asl_sport)

#~~~~~~~~~~~~~~~~~~
## Manipulate ASL data
# Filter for just species 410 (legal), only DNA sampled
asl_sport <- asl_sport %>% 
  filter(SPECCODE == 410) %>% 
  filter(!is.na(SAMPLE_NO))

# Make SITE a factor for ordering purposes
asl_sport <- asl_sport %>% 
  mutate(site = factor(SITE, c("JUNEAU", "KETCHIKAN", "WRANGELL", "PETERSBURG", "SITKA", "CRAIG_KLAWOCK", "GUSTAVUS", "ELFIN_COVE", "YAKUTAT"))) %>% 
  rename(biweek = BIWEEK)

# Pivot harvest by biweek and site
asl_sport %>% 
  count(biweek, site) %>% 
  spread(site, n, fill = 0)


# Create a variable for Fishery
asl_sport <- asl_sport %>% 
  mutate(fishery = case_when(site == "KETCHIKAN" ~ "KTN",
                             site %in% c("PETERSBURG", "WRANGELL") ~ "PB-WR",
                             site == "JUNEAU" ~ "Inside",
                             site %in% c("YAKUTAT", "GUSTAVUS", "ELFIN_COVE", "SITKA", "CRAIG_KLAWOCK") & biweek <= 13 ~ "Outside_early",
                             site %in% c("YAKUTAT", "GUSTAVUS", "ELFIN_COVE", "SITKA", "CRAIG_KLAWOCK") & biweek > 13 ~ "Outside_late")) %>% 
  mutate(fishery = factor(fishery, levels = c("KTN", "PB-WR", "Inside", "Outside_early", "Outside_late")))

asl_sport %>% 
  count(biweek, fishery) %>% 
  spread(fishery, n, fill = 0)

#~~~~~~~~~~~~~~~~~~
## Visualize ASL data
# Plot samples by Stat Week and Site
# Using ggplot2 `geom_bar` (we know that there is 1 row per DNA sample)
asl_sport %>% 
  ggplot(aes(x = biweek, fill = fishery)) +
  geom_bar() +
  facet_wrap(~ site) +
  ylab("# DNA Samples") +
  ggtitle("Samples by Biweek for Sport AY19")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in Harvest data
# By Site and Biweek
harvest_sport <- read_csv(file = "Harvest Data/preliminary_2018_sport_harvest_chinook.csv")
str(harvest_sport, give.attr = FALSE)  # names for sites are different, data is wide
problems(harvest_sport)

#~~~~~~~~~~~~~~~~~~
## Manipulate harvest data
# Make data tall (tidy), recode sites to ALL CAPS
level_key <- list("Juneau" = "JUNEAU", 
                  "Ketchikan" = "KETCHIKAN", 
                  "Wrangell" = "WRANGELL", 
                  "Petersburg" = "PETERSBURG", 
                  "Sitka" = "SITKA", 
                  "Craig" = "CRAIG_KLAWOCK", 
                  "Gustavus" = "GUSTAVUS", 
                  "Elfin Cove" = "ELFIN_COVE", 
                  "Yakutat" = "YAKUTAT")

harvest_sport <- harvest_sport %>% 
  gather(site, harvest, -biweek) %>% 
  mutate(site = recode_factor(site, !!!level_key, .ordered = TRUE))

# Create a variable for Fishery
harvest_sport <- harvest_sport %>% 
  mutate(fishery = case_when(site == "KETCHIKAN" ~ "KTN",
                             site %in% c("PETERSBURG", "WRANGELL") ~ "PB-WR",
                             site == "JUNEAU" ~ "Inside",
                             site %in% c("YAKUTAT", "GUSTAVUS", "ELFIN_COVE", "SITKA", "CRAIG_KLAWOCK") & biweek <= 13 ~ "Outside_early",
                             site %in% c("YAKUTAT", "GUSTAVUS", "ELFIN_COVE", "SITKA", "CRAIG_KLAWOCK") & biweek > 13 ~ "Outside_late")) %>% 
  mutate(fishery = factor(fishery, levels = c("KTN", "PB-WR", "Inside", "Outside_early", "Outside_late")))

# Table of Harvest by site
harvest_sport %>% 
  select(-fishery) %>% 
  spread(site,  harvest, fill = 0)

# Table of Harvest by site
harvest_sport %>% 
  group_by(fishery, biweek) %>% 
  summarise(harvest = sum(harvest)) %>% 
  spread(fishery,  harvest, fill = 0)


#~~~~~~~~~~~~~~~~~~
## Visualize Harvest Data
# Plot samples by Stat Week (all Quadrants)
# Using ggplot2 `geom_col` to plot harvest (identity)
harvest_sport %>% 
  ggplot(aes(x = biweek, y = harvest, fill = fishery)) +
  geom_col() +
  facet_wrap(~ site) +
  ggtitle("Harvest by Biweek for Sport AY18")

# Determine max harvest by site for heatmaps
max_bw_harvest <- max(harvest_sport$harvest)

# Heatmap of Harvest by Stat Week and District for Sport 1
harvest_sport %>% 
  ggplot(aes(x = biweek, y = site, fill = harvest, label = harvest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", limits = c(0, max_bw_harvest)) +
  theme_classic() +
  geom_text(color = "red") +
  ggtitle("Sport - Harvest by Biweek and Site")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join ASL and Harvest data by Site and Biweek
# Roll up ASL to biweek and site level, join with harvest
join_sport <- asl_sport %>% 
  count(site, biweek) %>% 
  full_join(harvest_sport, by = c("site", "biweek")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, harvest = 0))  # replace NA in samples and harvest with 0

join_sport %>% 
  filter(n > harvest)  # WTF, should be 0 rows...
