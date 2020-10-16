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


## list of evts that are not included in TNC "portfolio of parcels"
  ## how important are evts? rare? common?
  ## list of unique evts that tnc does not protect
## R compare rank chart
  ## rank top percentages for each and see how they compare
  ## top 10 evts
## Basic stats
  ## average size of ownership -- histogram
## case studies -- larger preserves
  ## are the preserves mostly one ecosystem or mixed?
  ## like ellsworht for example
## characterizing for region