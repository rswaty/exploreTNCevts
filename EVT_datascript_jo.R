rm(list=ls()) 
library(dplyr)
library(ggplot2)

EVT_NAME_CA<-US200EVT_CA$EVT_NAME
EVT_NAME_CA_TNC<-US200EVT_CA_TNC$EVT_NAME

matches_CA<-intersect(EVT_NAME_CA, EVT_NAME_CA_TNC)

not_owned_CA<-setdiff(EVT_NAME_CA,EVT_NAME_CA_TNC)

#Idea- sort the not owned EVT names by EVT_PHYS and make a pie chart... this would give a visual of developed vs. undeveloped
#How can we incorporate the proporations of the counts that TNC owns of the each EVT?
  ##Maybe a histogram with double-bars of habitat type owned and unowned- show proportions of EVTs owned and 
  ##unowned, even in the habitat types that are owned... would this histogram be too large though?

#Not owned acres using count
#Rank of percentages of EVT- group by 10 most prevalent EVTs?
#Basic statistics- what is the average size of ownership- histogram?
#case study of big preserve- are they all one ecosystem or are they multiple ecosystems
#Characterize preserves, characterize preserve size, and comparing them to the region

evt_ca <- read.csv("C:\\Users\\Owner\\Documents\\CDL\\EVT_Percentages\\EVT_Percentages\\US200EVT_CA.csv")
tnc_ca <- read.csv("C:\\Users\\Owner\\Documents\\CDL\\EVT_Percentages\\EVT_Percentages\\US200EVT_CA_TNC.csv")

library(dplyr)
library(stringr)
rm(list=ls())
file.choose()

#Let's sort CA EVTs by count...
ca_bycount<-evt_ca %>%arrange(count)

#Remove Developed or Agricultural EVTs, because we don't care about these obvsly
ca_undeveloped<-filter(ca_bycount, EVT_PHYS!="Developed", EVT_PHYS!="Agricultural")

#Select 1st 10 rows, which will be the smallest acreage:
ca_undeveloped_smallest<-slice(ca_undeveloped, 1:10)

#Here are the 10 smallest EVTs in CA:

1                       North American Warm Desert Wash Woodland
2   North American Warm Desert Riparian Mesquite Bosque Woodland
3                Southern Rocky Mountain Ponderosa Pine Woodland
4               North Pacific Alpine and Subalpine Dry Grassland
5                Klamath-Siskiyou Xeromorphic Serpentine Savanna
6                          Columbia Plateau Ash and Tuff Badland
7  North American Warm Desert Riparian Mesquite Bosque Shrubland
8                     Mediterranean California Alpine Fell-Field
9                           Sonoran Granite Outcrop Desert Scrub
10   North American Warm Desert Lower Montane Riparian Shrubland  

#Now we need to find out how what percentage of each of these EVTs TNC protects,
##Since these are ~in theory~ the most threatened EVTs...
