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
