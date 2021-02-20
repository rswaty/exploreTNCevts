getwd()

library(tidyverse)

evt_mal<-read.csv("./EVT_Percentages/US200EVT_MAL.csv")
tnc_mal<- read.csv("./EVT_Percentages/US200EVT_MAL_TNC.csv")

evt_mal<-evt_mal%>%
  rename(COUNT_MAL=COUNT)%>%
  arrange(desc(COUNT_MAL))%>%
  mutate(PERCENT_MAL=round(COUNT_MAL/sum(COUNT_MAL)*100,4),
         RANK_MAL=1:pull(tally(evt_mal)))
tnc_mal<-tnc_mal%>%
  rename(COUNT_TNC=COUNT)%>%
  arrange(desc(COUNT_TNC))%>%
  mutate(PERCENT_TNC=round(COUNT_TNC/sum(COUNT_TNC)*100,4),
         RANK_TNC=1:pull(tally(tnc_mal)))

mal <- left_join(evt_mal, tnc_mal) %>%
  replace(is.na(.), 0)

mal_notowned<-mal%>%
  filter(COUNT_TNC==0 & EVT_PHYS!='Agricultural' & EVT_PHYS!='Developed' & EVT_PHYS!='Snow-Ice' & EVT_PHYS!='Exotic Herbaceous' & EVT_PHYS!= 'Exotic Tree-Shrub')%>%
  select(-COUNT_TNC,-PERCENT_TNC,-RANK_TNC)

mal_tennotowned<-slice(mal_notowned,1:10)

ordermal_tennotowned<-mal_tennotowned%>%
  arrange(desc(PERCENT_MAL))%>%
  mutate(EVT_NAME = factor(EVT_NAME, levels=EVT_NAME))


library(stringr)
EVT_PHYSNS<-str_replace(ordermal_tennotowned$EVT_PHYS," ","")

ordermal_tennotowned$EVT_PHYSNS<-EVT_PHYSNS

ordermal_tennotowned=subset(ordermal_tennotowned, select=-EVT_PHYS)


phys_mal_notowned<-mal_notowned %>% group_by(EVT_PHYS)%>%
  summarise(PHYS_PERCENT_MAL = sum(PERCENT_MAL))%>%
  arrange(desc(PHYS_PERCENT_MAL))

library(ggplot2)

ggplot(phys_mal_notowned, aes(x=EVT_PHYS,y=PHYS_PERCENT_MAL))+
  geom_bar(stat='identity', position='dodge')+
  xlab("EVT Phys Type")+
  ylab("Percent of MAL Area")+
  ggtitle("Percent Areas of EVTs in MAL not owned by TNC")+
  theme_bw()

ggplot(ordermal_tennotowned, aes(x=EVT_NAME, y=PERCENT_MAL, fill=EVT_PHYSNS))+
  geom_bar(stat='identity',position='dodge')+
  coord_flip()+
  labs(x="EVT",y="Percent of MAL",title="Top 10 EVT's in MAL not owned by TNC")+
  scale_fill_manual(values=c(Conifer="olivedrab4", Riparian="navy", Grassland="navajowhite1", Shrubland="orange2", Hardwood= "orange4", SparselyVegetated="olivedrab1"))
