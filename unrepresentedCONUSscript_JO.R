getwd()

library(tidyverse)

evt_conus<-read.csv("./EVT_Percentages/US200EVT_CONUS.csv")
tnc_conus<- read.csv("./EVT_Percentages/US200EVT_CONUS_TNC.csv")

evt_conus<-evt_conus%>%
  rename(COUNT_CONUS=COUNT)%>%
  arrange(desc(COUNT_CONUS))%>%
  mutate(PERCENT_CONUS=round(COUNT_CONUS/sum(COUNT_CONUS)*100,4),
         RANK_CONUS=1:pull(tally(evt_conus)))
tnc_conus<-tnc_conus%>%
  rename(COUNT_TNC=COUNT)%>%
  arrange(desc(COUNT_TNC))%>%
  mutate(PERCENT_TNC=round(COUNT_TNC/sum(COUNT_TNC)*100,4),
         RANK_TNC=1:pull(tally(tnc_conus)))

conus <- left_join(evt_conus, tnc_conus) %>%
  replace(is.na(.), 0)

conus_notowned<-conus%>%
  filter(COUNT_TNC==0)%>%
  select(-COUNT_TNC,-PERCENT_TNC,-RANK_TNC)

conus_tennotowned<-slice(conus_notowned,1:10)

orderconus_tennotowned<-conus_tennotowned%>%
  arrange(desc(PERCENT_CONUS))%>%
  mutate(EVT_NAME = factor(EVT_NAME, levels=EVT_NAME))

library(stringr)

orderconus_tennotowned<-str_replace_all(orderconus_tennotowned, fixed("Sparsely Vegetated"), "SparselyVegetated")

phys_conus_notowned<-conus_notowned %>% group_by(EVT_PHYS)%>%
  summarise(PHYS_PERCENT_CONUS = sum(PERCENT_CONUS))%>%
  arrange(desc(PHYS_PERCENT_CONUS))

phys_conus_notowned<-phys_conus_notowned%>%
  filter(EVT_PHYS!='Agricultural' & EVT_PHYS!='Developed' & EVT_PHYS!='Snow-Ice' & EVT_PHYS!='Exotic Herbaceous' & EVT_PHYS!= 'Exotic Tree-Shrub')

library(ggplot2)

ggplot(phys_conus_notowned, aes(x=EVT_PHYS,y=PHYS_PERCENT_CONUS))+
  geom_bar(stat='identity', position='dodge')+
  xlab("EVT Phys Type")+
  ylab("Percent of CONUS Area")+
  ggtitle("Percent Areas of EVTs in CONUS not owned by TNC")+
  theme_bw()

ggplot(orderconus_tennotowned, aes(x=EVT_NAME, y=PERCENT_CONUS, fill=EVT_PHYS))+
  geom_bar(stat='identity',position='dodge')+
  coord_flip()+
  labs(x="EVT",y="Percent of CONUS",title="Top 10 EVT's in CONUS not owned by TNC")+
  scale_fill_manual(values=c(Conifer="brown4", Riparian="cadetblue1", Grassland="chartreuse2", Shrubland="blueviolet", Hardwood= "brown1"))
