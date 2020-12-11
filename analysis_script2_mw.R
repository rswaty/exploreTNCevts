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
         PERCENT_CA = round(ACRES_CA / sum(ACRES_CA) * 100, 4),
         RANK_CA = 1:pull(tally(evt_ca)))
tnc_ca <- tnc_ca %>%
  rename(COUNT_TNC = COUNT) %>%
  arrange(desc(COUNT_TNC)) %>%
  mutate(ACRES_TNC = round(COUNT_TNC * 900 / 4046.86),
         PERCENT_TNC = round(ACRES_TNC / sum(ACRES_TNC) *100, 4),
         RANK_TNC = 1:pull(tally(tnc_ca)))

# join to single data frame and replace NAs with 0
ca <- left_join(evt_ca, tnc_ca) %>%
  replace(is.na(.), 0)

#################################################
# summarize each evt by evt_phys
phys_ca <- ca %>% group_by(EVT_PHYS) %>%
    summarise(phys_percent_ca = sum(PERCENT_CA),
            phys_percent_tnc = sum(PERCENT_TNC))

# rename and subset some shit
phys_sub_ca <- phys_ca %>% mutate(phys = case_when(
  str_detect(EVT_PHYS, "Developed") ~ "Developed",
  str_detect(EVT_PHYS, "Exotic") ~ "Exotic",
  str_detect(EVT_PHYS, "Quarries") ~ "Quarries and Mining Land",
  EVT_PHYS != c("Developed", "Exotic", "Quarries and Mining Land") ~ EVT_PHYS)) %>%
  group_by(phys) %>%
  summarise(phys_percent_ca = sum(phys_percent_ca),
            phys_percent_tnc = sum(phys_percent_tnc))

# make tidy
phys_tidy_ca <- phys_sub_ca %>%
  pivot_longer(!phys, names_to = "location", values_to = "percent") %>%
  mutate(location = if_else(location == "phys_percent_ca", true = "CA", false = "TNC"))

# double bar chart
ggplot(data = phys_tidy_ca, aes(x = phys, y = percent)) +
  geom_bar(aes(fill = location), stat = "identity", position = position_dodge2(reverse = T)) +
  ylim(c(0, 40)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "LANDFIRE Existing Vegetation Types of California",
       subtitle = "comparison with TNC ownership - grouped by EVT_PHYS",
       x = "",
       y = "Percent (%)",
       fill = "") +
  theme_light()
  
##################################################################
# diverging bar chart based on CA evts over 1%

evt1_ca <- ca %>% filter(PERCENT_CA >= 1) %>%
  dplyr::select(EVT_NAME, PERCENT_CA, PERCENT_TNC) %>%
  mutate(diff = PERCENT_TNC - PERCENT_CA,
         color_score = if_else(diff > 0, "TNC above", "TNC below"))

# plot diverging bar
ggplot(data = evt1_ca, aes(x = EVT_NAME, y = diff)) +
  geom_bar(aes(fill = color_score), stat = "identity") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = seq(-5, 25, by = 5)) +
  labs(title = "LANDFIRE Existing Vegetation Types of California",
       subtitle = "difference between % TNC ownership and % CA values above 1%",
       x = "",
       y = "Percent difference (%)",
       fill = "") +
  theme_light()

################################################################## NOT DONE YET, WAITING 6 HOURS TO DOWNLOAD EVT200
# percent protected map using evt140
library(raster)
library(sf)
library(tmap)

ca_shp <- st_read("D:/Shapefiles/california/ca-state-boundary/CA_State_TIGER2016.shp") %>%
  st_transform(5072) %>%
  st_union() %>%
  st_sf()

lf_evt_ca <- raster("D:/LANDFIRE/US_140EVT/Grid/us_140evt/hdr.adf") %>%
  crop(ca_shp) %>%
  mask(ca_shp)

protect_ca <- ca %>% mutate(protect_percent = round(COUNT_TNC / COUNT_CA * 100, 4)) %>%
  dplyr::select(EVT_NAME, protect_percent) %>%
  rename(CLASSNAME = EVT_NAME)

levels(lf_evt_ca)[[1]] %>% left_join(protect_ca)

levels(lf_evt_ca)[[1]]
