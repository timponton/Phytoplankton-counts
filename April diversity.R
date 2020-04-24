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
library(zoo)

##### Data #####

df <- read.csv("Updated data from April 2020/Combo-as-of-April-2020-OpenRefineCopy.csv")
order <- read.csv("Updated data from April 2020/Ordered Sample sites - Abagold.csv")
df <- df %>% 
  left_join(order, by = "Site")

# fix date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

# merge date and times and coerce to proper format 
df$dateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M")

# Add month and year column
df <- df %>% 
  mutate(months = months(Date), month = month(Date), year = year(Date)) %>% 
  mutate(Season = ifelse(month %in% c(12, 1, 2), "Summer", 
                         ifelse(month %in% c(3, 4, 5), "Autumn", 
                                ifelse(month %in% c(6, 7, 8), "Winter", "Spring")))) %>% 
  mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
  mutate(SeasonOrder = ifelse(month %in% c(12, 1, 2), 1, 
                              ifelse(month %in% c(3, 4, 5), 2, 
                                     ifelse(month %in% c(6, 7, 8), 3, 4))))



# coerce year column as factor
df$year <- as.factor(df$year)
 
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

ggplot(data = differentSpecies, aes(x = Site, y = meanC, fill = Species)) +
  geom_bar(stat = "Identity") +
  facet_wrap(~Date, ncol = 5)

df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanC, values_fill = list(meanC = 0)) %>% 
  mutate(filteredOut = ifelse(`Primary sump` > `After Drumfilter`, "Good filtered", 
                              ifelse(`Primary sump` == `After Drumfilter`, "same", "Bad filtered")), 
         DifferenceFiltered = `Primary sump` - `After Drumfilter`) %>%
  pivot_longer(-c(Date, Species, filteredOut, DifferenceFiltered), names_to = "Site", values_to = "MeanCells") %>%
  ungroup()  %>%
  group_by(Date) %>%
  arrange(desc(abs(DifferenceFiltered))) %>% 
  top_n(5) %>% 
  ggplot(., aes(x = Date, y = DifferenceFiltered)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~Species)
  
  ggplot(., aes(x = filteredOut, y = MeanCells, col = Species)) +
  geom_jitter() +
  facet_wrap(~Date)
  
  filter(filteredOut == "no") %>% 
  ggplot(., aes(x = Date, y = MeanCells, col = Species)) +
  geom_jitter()
  


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


##### Rounding off times or grouping to daily groups ####
  
  # round off time to certain amount
df$roundTime <- round_date(df$dateTime, "2 hour")


  
# check the time difference when rounding off times
ggplotly(df %>%
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>%
  mutate(timeDiff = (dateTime - roundTime)/60) %>% 
  ggplot(., aes(x = dateTime, y = timeDiff, col = Site)) +
  geom_point())
  
# Total cells when rounded off
ggplotly(df %>%
           group_by(Date) %>% 
           filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
           filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
           filter(Site %in% c("After Drumfilter", "Primary sump")) %>%
           mutate(timeDiff = (dateTime - roundTime)/60) %>% 
           ungroup() %>% 
           group_by(roundTime, Site) %>% 
           summarise(totalCells = sum(Cells.L)) %>%
           filter(roundTime >= as.Date("2019-02-19") & roundTime <= as.Date("2019-02-20")) %>% 
           ggplot(., aes(x = roundTime, y = totalCells, fill = Site)) +
           geom_bar(stat = "Identity", position = "Dodge"))

# difference in time
ggplotly(df %>%
           group_by(Date) %>% 
           filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
           filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
           filter(Site %in% c("After Drumfilter", "Primary sump")) %>%
           mutate(timeDiff = (dateTime - roundTime)/60) %>% 
           filter(roundTime >= as.Date("2019-02-19") & roundTime <= as.Date("2019-02-20")) %>% 
           ggplot(., aes(x = roundTime, y = timeDiff, fill = Site)) +
           geom_bar(stat = "Identity", position = "Dodge", col = "black") +
           geom_hline(yintercept = 0, color = "black"))

  ##### geom_tile for year/monthly diversity #####

df %>% 
  filter(CodeCount == 4605)

# create group df
matrix <- df %>% 
  mutate(months = months(Date), month = month(Date), year = year(Date)) %>%
  filter(Classification != "Various", Site == "Primary sump", Species != "Diatom") %>% 
  group_by(months, month, year, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0))
# subset groups
group <- matrix %>% 
  select(1:3)

# add diveristy to group df
group <- group %>% 
  mutate(shannon = diversity(matrix[, 4:ncol(matrix)], "shannon"), 
         Adjsimpson = 1-(diversity(matrix[, 4:ncol(matrix)], "simpson")), 
         simpson = diversity(matrix[, 4:ncol(matrix)], "simpson"))

h <- ggplot(data = group, aes(x = reorder(months, month), y = year, fill = shannon)) +
  geom_tile(col = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  xlab("Months") +
  ylab("Year") +
  ggtitle("Monthly Shannon index of diversity over 4 years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# vs

l <- ggplot(data = group, aes(x = month, y = shannon, col = as.factor(year))) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", 
                                               "Apr", "May", "Jun", 
                                               "Jul", "Aug", "Sep", 
                                               "Oct", "Nov", "Dec")) +
  xlab("Months") +
  ylab("Shannon Index") +
  labs(col = "Year") +
  theme_classic()

h/l

# vs 
ggplot(data = group, aes(x = shannon, y = reorder(months, month))) +
  geom_density_ridges()

group %>% 
  filter(year != "2020") %>% 
ggplot(., aes(x = shannon, y = as.factor(year), fill = stat(x))) +
  geom_density_ridges_gradient()

install.packages("ggridges")
library(ggridges)




##### daily diversity ####
# create group df
daymatrix <- df %>% 
  mutate(months = months(Date), month = month(Date), year = year(Date)) %>%
  filter(Classification != "Various", Site == "Primary sump", Species != "Diatom") %>% 
  group_by(Date, months, month, year, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0))

# subset groups
daygroup <- daymatrix %>% 
  select(1:4)

# add diveristy to group df
daygroup <- daygroup %>% 
  mutate(shannon = diversity(daymatrix[, 5:ncol(daymatrix)], "shannon"), 
         Adjsimpson = 1-(diversity(daymatrix[, 4:ncol(daymatrix)], "simpson")), 
         simpson = diversity(daymatrix[, 4:ncol(daymatrix)], "simpson")) %>% 
  filter(shannon != 0, simpson != 0, Adjsimpson != 0)

ggplot(data = daygroup, aes(x = Date, y = shannon, col = as.factor(year))) +
  geom_line()

daygroup %>% 
  filter(year != "2020") %>% 
  ggplot(., aes(x = shannon, y = reorder(months, month), fill = stat(x))) +
  geom_density_ridges_gradient() +
  facet_wrap(~year, ncol = 3)

#### dino vs diat



