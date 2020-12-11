# double check working directory
getwd()
# sick

########################################
library(tidyverse)

########################################

# load data
# CA
evt_ca <- read.csv("./EVT_Percentages/US200EVT_CA.csv")
tnc_ca <- read.csv("./EVT_Percentages/US200EVT_CA_TNC.csv")

# add ACRES column based on COUNT
# COUNT is number of 30x30 m pixels in raster, or 900 m^2
# add PERCENT column
evt_ca <- evt_ca %>%
  rename(COUNT_CA = COUNT) %>%
  arrange(desc(COUNT_CA)) %>%
  mutate(ACRES_CA = round(COUNT_CA * 900 / 4046.86),
         PERCENT_CA = round(ACRES_CA / sum(ACRES_CA), 4),
         RANK_CA = 1:pull(tally(evt_ca)))
tnc_ca <- tnc_ca %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC), 4),
         RANK_TNC = 1:pull(tally(tnc_ca)))

# join to single data frame
ca <- left_join(evt_ca, tnc_ca)

# summarize each evt by evt_phys
phys_ca <- ca %>% group_by(EVT_PHYS) %>%
    summarise(phys_acres_ca = sum(ACRES_CA),
            phys_acres_tnc = sum(ACRES_TNC)) %>%
  arrange(desc(phys_acres_ca))

# break down by evt_name of areas that are protected
exotic_herb_ca <- ca %>% filter(EVT_PHYS == "Exotic Herbaceous") %>%
  select(-c(EVT_FUEL_N, EVT_LF, EVT_PHYS, EVT_GP_N, EVT_ORDER, EVT_CLASS, EVT_SBCLS, SAF_SRM))

exotic_tree_ca <- ca %>% filter(EVT_PHYS == "Exotic Tree-Shrub") %>%
  select(-c(EVT_FUEL_N, EVT_LF, EVT_PHYS, EVT_GP_N, EVT_ORDER, EVT_CLASS, EVT_SBCLS, SAF_SRM))

hardwood_ca <- ca %>% filter(EVT_PHYS == "Hardwood") %>%
  select(-c(EVT_FUEL_N, EVT_LF, EVT_PHYS, EVT_GP_N, EVT_ORDER, EVT_CLASS, EVT_SBCLS, SAF_SRM))

