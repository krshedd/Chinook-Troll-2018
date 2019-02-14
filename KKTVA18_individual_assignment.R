# KTVA came and did an interview about our Chinook GSI research
# Karen (media outreach) purchased a whole Chinook from 10th and M Seafoods for a demonstration
# We genotyped that fish for the GAPS 13 uSATs and ran IA via ONCOR to figure out where it was from

source("~/../R/Functions.GCL.R")
library(tidyverse)
library(lubridate)

.username <- "krshedd"
.password <- ""

load_objects("../Objects/")

LOKI2R_GAPS.GCL(sillyvec = "KKTVA18", username = .username, password = .password)

KKTVA18.gcl

# Create ONCOR file
gcl2Genepop.GCL(sillyvec = "KKTVA18", path = "../ONCOR/Mixture/KKTVA18.gen", loci = GAPSLoci, VialNums = TRUE, usat = TRUE)

# Our KTVA fish assigned at 100% to CEDARR which is a Skeena population.
GroupNames33[GroupVec33RG_357[SEAKPops357 == "CEDARR"]]