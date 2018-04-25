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
harvest_yr_sw_fishery_quad.df <- harvest.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  group_by(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  summarise(Harvest = sum(`N Catch`))

harvest_ASL_join.df <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year == "2017" | Fishery == "Late Winter" & Year == "2018") %>%
  filter(!is.na(`Dna Specimen No`)) %>%  # filter for known DNA samples
  count(Year_f, `Stat Week`, Fishery, Quadrant) %>% 
  full_join(harvest_yr_sw_fishery_quad.df, by = c("Year_f", "Stat Week", "Fishery", "Quadrant")) %>%  # very important to do a full join in case some weeks are missing harvest or samples
  replace_na(list(n = 0, Harvest = 0))  # replace NA in samples and harvest with 0
  

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

## 171
# What does proportional sampling look like?
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 171) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * 200)) %>%  # if we want 200 samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_remainings = n - pHarvest)

# How many fish per week?
extraction_EW_171 <- data_frame('Stat Week' = 41:51,
                                n = c(29, 25, 19, 33, 17, 7, 14, 10, 14, 30, 2))

# Randomly pick fish
EW_171_torun <- ASL.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 171) %>% 
  nest(-`Stat Week`) %>% 
  left_join(extraction_EW_171, by = "Stat Week") %>% 
  mutate(Sample = map2(data, n, sample_n)) %>% 
  unnest(Sample)

# Verify picked fish
EW_171_torun %>% 
  count(`Stat Week`)
  
  


# 172
harvest_ASL_join.df %>% 
  filter(Fishery == "Early Winter" & Year_f == "2017" & Quadrant == 172) %>% 
  mutate(pHarvest = round(Harvest / sum(Harvest) * sum(n))) %>%  # if we want all samples proportional to harvest by SW
  mutate(n_sufficeint = n >= pHarvest) %>% 
  mutate(n_underage = ifelse(n_sufficeint, 0, pHarvest - n))


#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 293
EW_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "EW AY2017", header = TRUE)
str(EW_WGC.dat)

EW_WGC_171.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 171 | Fishery == "Troll matched axillary" & Dist.Quad == 171)
str(EW_WGC_171.dat)

EW_WGC2Sample_171 <- sample(EW_WGC_171.dat$Whatman.Card..)
EW_WGC2Sample_171_order <- match(EW_WGC2Sample_171, EW_WGC_171.dat$Whatman.Card..)

any(cumsum(EW_WGC_171.dat[EW_WGC2Sample_171_order, "X..Tissues"]) == 293)
max2run_171 <- which(cumsum(EW_WGC_171.dat[EW_WGC2Sample_171_order, "X..Tissues"]) == 293)  # 293 samples from 171
EW_WGC2Run_171 <- EW_WGC2Sample_171[seq(max2run_171)]

#~~~~~~~~~~~~~~~~~~
# 172
# Run all
EW_WGC_172.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 172 | Fishery == "Troll matched axillary" & Dist.Quad == 172)
str(EW_WGC_172.dat)
sum(EW_WGC_172.dat$X..Tissues)

EW_WGC2Run_172 <- EW_WGC_172.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 173
# Run all
EW_WGC_173.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 173 | Fishery == "Troll matched axillary" & Dist.Quad == 173)
str(EW_WGC_173.dat)
sum(EW_WGC_173.dat$X..Tissues)

EW_WGC2Run_173 <- EW_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Run all
EW_WGC_174.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 174 | Fishery == "Troll matched axillary" & Dist.Quad == 174)
str(EW_WGC_174.dat)
sum(EW_WGC_174.dat$X..Tissues)

EW_WGC2Run_174 <- EW_WGC_174.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for EW
EW_WGC2Run <- sort(c(EW_WGC2Run_171, EW_WGC2Run_172, EW_WGC2Run_173, EW_WGC2Run_174))

EW_WGC_Run.dat <- EW_WGC.dat[match(EW_WGC2Run, EW_WGC.dat$Whatman.Card..), ]
str(EW_WGC_Run.dat)
sum(EW_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = EW_WGC_Run.dat, sum)

# dir.create("Extraction Lists")
write.xlsx(x = EW_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "EW Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(EW_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq(EW_WGC2Run), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL16EW", append = TRUE, row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Late Winter ####
#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 302
LW_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "LW AY2017", header = TRUE)
str(LW_WGC.dat)

LW_WGC_171.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 171)
str(LW_WGC_171.dat)

LW_WGC2Sample_171 <- sample(LW_WGC_171.dat$Whatman.Card..)
LW_WGC2Sample_171_order <- match(LW_WGC2Sample_171, LW_WGC_171.dat$Whatman.Card..)

any(cumsum(LW_WGC_171.dat[LW_WGC2Sample_171_order, "X..Tissues"]) == 302)
max2run_171 <- which(cumsum(LW_WGC_171.dat[LW_WGC2Sample_171_order, "X..Tissues"]) == 302)  # 302 samples from 171
LW_WGC2Run_171 <- LW_WGC2Sample_171[seq(max2run_171)]


#~~~~~~~~~~~~~~~~~~
# 172
# Subsample 42
LW_WGC_172.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 172)
str(LW_WGC_172.dat)

LW_WGC2Sample_172 <- sample(LW_WGC_172.dat$Whatman.Card..)
LW_WGC2Sample_172_order <- match(LW_WGC2Sample_172, LW_WGC_172.dat$Whatman.Card..)

any(cumsum(LW_WGC_172.dat[LW_WGC2Sample_172_order, "X..Tissues"]) == 42)
max2run_172 <- which(cumsum(LW_WGC_172.dat[LW_WGC2Sample_172_order, "X..Tissues"]) == 42)  # 42 samples from 172
LW_WGC2Run_172 <- LW_WGC2Sample_172[seq(max2run_172)]

#~~~~~~~~~~~~~~~~~~
# 173
# Run all
LW_WGC_173.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 173)
str(LW_WGC_173.dat)
sum(LW_WGC_173.dat$X..Tissues)

LW_WGC2Run_173 <- LW_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Subsample 95
LW_WGC_174.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 174)
str(LW_WGC_174.dat)

LW_WGC2Sample_174 <- sample(LW_WGC_174.dat$Whatman.Card..)
LW_WGC2Sample_174_order <- match(LW_WGC2Sample_174, LW_WGC_174.dat$Whatman.Card..)

any(cumsum(LW_WGC_174.dat[LW_WGC2Sample_174_order, "X..Tissues"]) == 95)
max2run_174 <- which(cumsum(LW_WGC_174.dat[LW_WGC2Sample_174_order, "X..Tissues"]) == 95)  # 95 samples from 174
LW_WGC2Run_174 <- LW_WGC2Sample_174[seq(max2run_174)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for LW
LW_WGC2Run <- sort(c(LW_WGC2Run_171, LW_WGC2Run_172, LW_WGC2Run_173, LW_WGC2Run_174))

LW_WGC_Run.dat <- LW_WGC.dat[match(LW_WGC2Run, LW_WGC.dat$Whatman.Card..), ]
str(LW_WGC_Run.dat)
sum(LW_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = LW_WGC_Run.dat, sum)

write.xlsx(x = LW_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "LW Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(LW_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq(LW_WGC2Run), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL17LW", append = TRUE, row.names = FALSE)

