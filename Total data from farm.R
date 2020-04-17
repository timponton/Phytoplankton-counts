## All the data received from farm, from 2017-2020

##### library load #####

library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(anytime)
library(lubridate)
library(zoo)
library(scales)
library(vegan)
####

##### Read in data and fix date#####
# read in data
all <- read.csv("All data/Final data/Final cleaned data all.csv")

# fix dates column 
all$Date <- as.Date(all$Date, "%d/%m/%Y")
####

#####adding columns related to dates#####
# add month months and year column column
all <- all %>% 
  mutate(months = months(Date), month = month(Date), year = year(Date))

# coerce year column as factor
all$year <- as.factor(all$year)

all$monthYear <- as.yearmon(paste(all$year, all$month), "%Y %m")

# create month year column but this creates character so FIND SOLUTION
all$yearMonth <- format(as.Date(all$Date), "%Y-%m")

####

# read in classification file
classification <- read.csv("All data/Final data/Classification.csv")

##### Plotting general graphs#####

# total number of cells counted per day 
a <- all %>% 
  filter(Site == "Primary sump") %>% 
  group_by(Date) %>% 
  summarise(n = sum(Cells.L)) %>% 
  ggplot(., aes(x = Date, y = n)) +
  geom_point() +
  geom_line()

ggplotly(a)




all %>% 
  filter(Species == "Gonyaulax polygramma") %>% 
  group_by(month, months, year) %>% 
  summarise(average = mean(Cells.L)) %>% 
  ggplot(., aes(x = reorder(months, month), y = average)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~year, nrow = 4, scales = "free_x")


# counts of species per month facetted by year
all %>% 
  group_by(month, months, year) %>% 
  summarise(numberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = reorder(months, month), y = numberSpecies)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~year, nrow = 4, scales = "free_x")



# overall look at phytoplankton species counts throughout the whoel dataset period
all %>% 
  group_by(monthYear) %>% 
  summarise(numberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = monthYear, y = numberSpecies)) +
  geom_line()

####










##### Plotting specific graphs #####

# TotalNumber of diatoms and Dino's
all %>% 
  filter(Classification == "Dinoflagellate" | Classification == "Diatom") %>% 
  group_by(Classification, monthYear) %>% 
  summarise(numberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = monthYear, y = numberSpecies, col = Classification)) +
  geom_line()
  
# mean diatoms vs dino's
DvD <- all %>% 
  filter(Classification == "Dinoflagellate" | Classification == "Diatom") %>% 
  group_by(Classification, monthYear) %>% 
  summarise(MeanCountSpecies = mean(Cells.L)) %>% 
  ggplot(., aes(x = monthYear, y = MeanCountSpecies, col = Classification)) +
  geom_line()
ggplotly(DvD)

# Filtered down to useable classification
noVar <- all %>% 
  filter(Classification != "Various")

noVar %>% 
  group_by(Classification, monthYear) %>% 
  summarise(numberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = monthYear, y = numberSpecies, col = Classification)) +
  geom_line()

####

##### Splitting dates into two groups because of missing data #####
# split groups using Date as a parameter
grp1 <- all %>% 
  filter(yearMonth <= "2018-04")
grp2 <- all %>% 
  filter(yearMonth >= "2018-10")

grp1 %>% 
  filter(Classification == "Dinoflagellate" | Classification == "Diatom") %>% 
  group_by(Classification, monthYear) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = monthYear, y = meanCells, col = Classification)) +
  geom_line()

grp2 %>% 
  filter(Classification == "Dinoflagellate" | Classification == "Diatom") %>% 
  group_by(Classification, monthYear) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = monthYear, y = meanCells, col = Classification)) +
  geom_line()

####




