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

# filter ecosystems tnc does not protect
no_tnc <- ca %>% filter(is.na(COUNT_TNC))



###################################################################

# calculate percent of each ca ecosystem protected by tnc
ca <- ca %>%
  mutate(PERCENT_PROTECT = round(COUNT_TNC / COUNT_CA, 4)) %>%
  arrange(desc(PERCENT_PROTECT))

# top ten highest percentages protected by tnc
ca_10_protect <- ca %>% top_n(10, PERCENT_PROTECT)
# plot
# labels show overall rank of ca ecosystem
ggplot(data = ca_10_protect, aes(x = EVT_NAME, y = PERCENT_PROTECT)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_label(label = ca_10_protect$CA_RANK) +
  labs(title = "Top 10 CA EVTs that TNC protects the most",
       subtitle = "Labels are rank of most prominent ecosystems in CA (i.e '1' is highest count of evt)",
       x = "EVT name",
       y = "Percent protected (TNC count / CA count)")
# save last plot
ggsave("./top_10_ca_ecosystems_protected_by_tnc.jpg")

  geom_label(label = ca_10_protect$CA_RANK)

