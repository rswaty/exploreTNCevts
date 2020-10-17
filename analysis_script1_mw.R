## list of evts that are not included in TNC "portfolio of parcels"
  ## how important are evts? rare? common?
  ## list of unique evts that tnc does not protect
## R compare rank chart
  ## rank top percentages for each and see how they compare
  ## top 10 evts
## Basic stats
  ## average size of ownership -- histogram
  ## maybe compare these between regions
## case studies -- larger preserves
  ## are the preserves mostly one ecosystem or mixed?
  ## like ellsworth for example
      # gonna need a TNC parcels shapefile I think
## characterizing for region

########################################

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
evt_ca <- evt_ca %>%
  mutate(ACRES = round(COUNT * 900 / 4046.86))
tnc_ca <- tnc_ca %>%
  mutate(ACRES = round(COUNT * 900 / 4046.86))

# create a vector of ecosystems tnc does not protect
diff_ca <- setdiff(evt_ca$EVT_NAME, tnc_ca$EVT_NAME)



# arrange by descending count, top 10, remove "open water" from evt bc it's dumb
evt_ca_10 <- evt_ca %>%
  arrange(desc(COUNT)) %>%
  filter(!(EVT_NAME == "Open Water")) %>%
  top_n(10)
tnc_ca_10 <- tnc_ca %>%
  arrange(desc(COUNT)) %>%
  top_n(10)

# check for commonalities
intersect(evt_ca_10, tnc_ca_10)
# no dice..

