getwd()

library(tidyverse)

evt_ney<-read.csv("./EVT_Percentages/US200EVT_NEY.csv")
tnc_ney<- read.csv("./EVT_Percentages/US200EVT_NEY_TNC.csv")

evt_ney<-evt_ney%>%
  rename(COUNT_NEY=COUNT)%>%
  arrange(desc(COUNT_NEY))%>%
  mutate(PERCENT_NEY=round(COUNT_NEY/sum(COUNT_NEY)*100,4),
         RANK_NEY=1:pull(tally(evt_ney)))
tnc_ney<-tnc_ney%>%
  rename(COUNT_TNC=COUNT)%>%
  arrange(desc(COUNT_TNC))%>%
  mutate(PERCENT_TNC=round(COUNT_TNC/sum(COUNT_TNC)*100,4),
         RANK_TNC=1:pull(tally(tnc_ney)))

ney <- left_join(evt_ney, tnc_ney) %>%
  replace(is.na(.), 0)

ney_notowned<-ney%>%
  filter(COUNT_TNC==0 & EVT_PHYS!='Agricultural' & EVT_PHYS!='Developed' & EVT_PHYS!='Snow-Ice' & EVT_PHYS!='Exotic Herbaceous' & EVT_PHYS!= 'Exotic Tree-Shrub')%>%
  select(-COUNT_TNC,-PERCENT_TNC,-RANK_TNC)

ney_tennotowned<-slice(ney_notowned,1:10)

orderney_tennotowned<-ney_tennotowned%>%
  arrange(desc(PERCENT_NEY))%>%
  mutate(EVT_NAME = factor(EVT_NAME, levels=EVT_NAME))


library(stringr)
EVT_PHYSNS<-str_replace(orderney_tennotowned$EVT_PHYS," ","")

orderney_tennotowned$EVT_PHYSNS<-EVT_PHYSNS

orderney_tennotowned=subset(orderney_tennotowned, select=-EVT_PHYS)


phys_ney_notowned<-ney_notowned %>% group_by(EVT_PHYS)%>%
  summarise(PHYS_PERCENT_NEY = sum(PERCENT_NEY))%>%
  arrange(desc(PHYS_PERCENT_NEY))

library(ggplot2)

ggplot(phys_ney_notowned, aes(x=EVT_PHYS,y=PHYS_PERCENT_NEY))+
  geom_bar(stat='identity', position='dodge')+
  xlab("EVT Phys Type")+
  ylab("Percent of NEY Area")+
  ggtitle("Percent Areas of EVTs in NEY not owned by TNC")+
  theme_bw()

ggplot(orderney_tennotowned, aes(x=EVT_NAME, y=PERCENT_NEY, fill=EVT_PHYSNS))+
  geom_bar(stat='identity',position='dodge')+
  coord_flip()+
  labs(x="EVT",y="Percent of NEY",title="Top 10 EVT's in NEY not owned by TNC")+
  scale_fill_manual(values=c(Conifer="olivedrab4", Riparian="navy", Grassland="navajowhite1", Shrubland="orange2", Hardwood= "orange4", SparselyVegetated="olivedrab1"))
