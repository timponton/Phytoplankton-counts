library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape2)
library(data.table)
library(tidyr)
library(plotly)
library(patchwork)
library(vegan)


##### Data #####

df <- read.csv("Updated data from April 2020/Combo-as-of-April-2020-OpenRefineCopy.csv")

# fix date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

# merge date and times and coerce to proper format 
df$dateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M")

# Add month and year column

# Add indices


##### All diversity in Primary sump #####


##### Diversity in After Drumfilter ####

ad <- df %>% 
  filter(Site == "After Drumfilter", Classification != "Various") %>%
  group_by(Date, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0))
  
  

# SUmmarise for time period (Day)
# COnvert long to wide and replace NA with 0
# extract grouping columns
# do diversity


#### Divesity between Primary and After Drumfilter 

## Species Richness
df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(NumberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = Date, y = NumberSpecies, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")




## shannon
# wrangle data 
ad <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0))

# create group df
group <-  df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  select(1:2)

# merge groups and diversity
group$shannon <- diversity(ad[, 3:44], "shannon")
group <- group %>% 
  filter(shannon != 0)

# plot shannon
ggplotly(ggplot(data = group, aes(x = Date, y = shannon, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge"))

##### check which species occur in one site but not the other ####

Ps <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  filter(Site == "Primary sump")

AD <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  filter(Site == "After Drumfilter")

# Species in Primary sump but not in After Drumfilter
Ps_only <- Ps %>% 
  group_by(Date) %>% 
  filter(!Species %in% AD$Species)

# Species in After Drumfilter but not in Primary sump on on the same day
AD_only <- AD %>% 
  group_by(Date) %>% 
  filter(!Species %in% Ps$Species)

# Join unequal site species
differentSpecies <- rbind(Ps_only, AD_only)

#### what species occur when after drumfilter > primary filter ####

ggplotly(df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  group_by(Date, Species) %>% 
  filter(`After Drumfilter` > `Primary sump`, `Primary sump` != 0) %>% 
  mutate(difference = `After Drumfilter` - `Primary sump`) %>% 
  group_by(Species) %>% 
  summarise(totalMean = mean(difference)) %>% 
  ggplot(., aes(x = reorder(Species, totalMean), y = totalMean)) +
    geom_point() +
    coord_flip())
    
  ggplot(., aes(x = reorder(Species, difference), y = difference)) +
  geom_point() +
  coord_flip()
  
  
  ggplot(., aes(x = Date, y = difference, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge")
  
  pivot_longer(-c(Date, Species), names_to = "Site", values_to = "MeanCells") %>% 
  ggplot(., aes(x = MeanCells, y = Species, col = Site)) +
  geom_point()



