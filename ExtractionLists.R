#### Extraction List ####
# AY2018 EWint and LWint
# Kyle Shedd
# Created Wed Apr 18 15:02:57 2018
date()

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK18")
library(tidyverse)
library(xlsx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Winter ASL and Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# Foll up ASL to SW and Quad level, join with harvest
harvest_ASL_join.df <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0))  # replace NA in samples and harvest with 0

# Plot samples and harvest together as proportions
harvest_ASL_join.df %>% 
  group_by(Year_f, Fishery, Quadrant) %>% 
  mutate(n = n / sum(n), Harvest = Harvest / sum(Harvest)) %>% 
  ungroup() %>% 
  gather(variable, proportion, -Year_f, -`Stat Week`, -Fishery, -Quadrant) %>% 
  filter(Fishery == "Early Winter") %>% 
  ggplot(aes(x = `Stat Week`, y = proportion, fill = variable)) +
  geom_col() +
  facet_grid(Quadrant ~ variable, scales = "free_y") +
  ggtitle("Samples and Harvest by Stat Week and Quadrant for Early Winter AY18")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Early Winter ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion with managers and PST folks indicated that they want as fine of scale data as possible
# However, this is what we have for samples
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017") %>%
  group_by(Quadrant) %>% 
  summarise(Samples = sum(n)) %>% 
  spread(Quadrant, Samples)

# Thus the plan for extraction is to pick ~200 for 171 (NO) and run everything else
# With the important caveat of subsampling in proportion to harvest by SW for each quadrant

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
  mutate(pHarvest = round(Harvest / sum(Harvest) * 40)) %>%  # if we want all samples proportional to harvest by SW
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
  mutate(pHarvest = round(Harvest / sum(Harvest) * 64)) %>%  # if we want all samples proportional to harvest by SW
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

## Create a single Early Winter Extraction data.frame
EW_torun_ASL.df <- bind_rows(EW_171_torun, EW_172_torun, EW_173_torun, EW_174_torun)
EW_torun_ASL.df %>% 
  count(Quadrant)

