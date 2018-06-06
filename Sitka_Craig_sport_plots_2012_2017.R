# Quick exploration of Craig vs. Sitka sport GSI results

library(tidyverse)
# Load 2012-2017 Criag and Sitka sport results
chin_stock <- dget("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Estimates objects/Sport_CRG_SIT_2012_2017_26RG_Estimates.txt")
str(chin_stock)
bind_rows(chin_stock)

# Randy's function to go from dput list of matrices to one dataframe
readGSIfile <- function(filename) {
  tmp = dget(filename)
  out = do.call(rbind, tmp)
  ReportingGroup = rownames(out)
  rownames(out) = 1:nrow(out)
  out = as.data.frame(out)
  out$ReportingGroup = ReportingGroup
  fnames = list()
  for(i in 1:length(tmp)) fnames[[i]] = rep(names(tmp)[i],nrow(tmp[[i]]))
  out$Fishery = substr(unlist(fnames), 1, nchar(unlist(fnames))-5)
  out$AY <- substr(unlist(fnames), nchar(unlist(fnames))-3, nchar(unlist(fnames)))
  return(out)
}

# Read with Randy's function
chin_stock <- readGSIfile(filename = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Estimates objects/Sport_CRG_SIT_2012_2017_26RG_Estimates.txt")
str(chin_stock)

# Groups to get order
groups26 <- dget("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Objects/GroupNames26.txt")

# AK stocks
chin_stock %>% 
  mutate(ReportingGroup = factor(ReportingGroup, levels = groups26)) %>% 
  filter(ReportingGroup %in% groups26[1:7]) %>% 
  ggplot(aes(x = ReportingGroup, y = mean, fill = Fishery)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~AY)

# Stock comp over time for each RG
chin_stock %>% 
  mutate(ReportingGroup = factor(ReportingGroup, levels = groups26)) %>% 
  mutate(AY = as.numeric(AY)) %>% 
  # filter(ReportingGroup %in% groups26[1:7]) %>% 
  ggplot(aes(x = AY, y = mean, colour = Fishery)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~ReportingGroup)


## Troll all quad by fishery 2009-2017
troll_stock <- readGSIfile(filename = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Estimates objects/Troll2009_2017_33RG_StratifiedEstimates.txt")

groups33 <- dget("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Objects/GroupNames33.txt")


# Stock comp over time for each RG
troll_stock %>% 
  mutate(ReportingGroup = factor(ReportingGroup, levels = groups33)) %>% 
  mutate(AY = as.numeric(AY)) %>% 
  # filter(ReportingGroup %in% groups33[9:22]) %>%
  # filter(Fishery %in% c("SumRet1AllQuad", "SumRet2AllQuad")) %>%
  ggplot(aes(x = AY, y = mean, fill = Fishery)) +
  geom_line() +
  geom_ribbon(aes(ymin = `5%`, ymax = `95%`), alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_x_continuous(breaks = 2009:2017) +
  facet_wrap(~ReportingGroup)
