## load packages
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

### read in data and fix

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

# Add Red Tide categories
# incorporating red tide presence into df
## first set parameters for each cate
RT <- df %>% 
  filter(Site == "Primary sump") %>% 
  group_by(Date) %>% 
  summarise(totalCells = sum(Cells.L)) %>% 
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High")))))))
# left join to main df
df <- df %>% 
  left_join(RT, by = "Date")

# add season_year
sdf <- mutate(df,
             season_year = ifelse(month(Date) == 12, year(Date) + 1, year(Date)),
             seasonOther = case_when(
               month(Date) %in% c(9, 10, 11) ~ "Spring",
               month(Date) %in% c(12, 1, 2) ~ "Summer",
               month(Date) %in% c(3, 4, 5) ~ "Autumn",
               month(Date) %in% c(6, 7, 8) ~ "Winter",
               T ~ NA_character_
             )) %>% 
  mutate(YearMonthOther = paste(season_year, seasonOther)) %>% 
  mutate(SeasonOrder = ifelse(seasonOther %in% "Summer", 1, 
                              ifelse(seasonOther %in% "Autumn", 2, 
                                     ifelse(seasonOther %in% "Winter", 3, 4)))) %>% 
  mutate(seasonRank = dense_rank(YearMonthOther)) %>% 
  group_by(Date, months, month, seasonOther, season_yearOther, SeasonOrder, YearMonthOther) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE))


# subset Primary sump
PS <- df %>% 
  filter(Site == "Primary sump")



### Summarise to daily counts

dailyMeans <- PS %>% 
  group_by(Date, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE))

# round off and summarise to the nearest 3 days
ggplotly(dailyMeans %>% 
  mutate(third = round_date(Date, "3 day")) %>% 
  group_by(third, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(thirdmean = mean(meanBiomass, na.rm = TRUE)) %>% 
  ggplot(., aes(x = third, y = log(thirdmean))) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_line())

SeasonOther_dailymeans <- PS %>% 
  group_by(Date, months, month, seasonOther, season_yearOther, YEarMonth) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE))

### line graph of all dates, trans = "log10" or whatever
ggplotly(ggplot(data = dailyMeans, aes(x = Date, y = log(meanBiomass))) +
  geom_line())

ggplot(data = dailyMeans, aes(x = Date, y = log(meanBiomass))) +
  geom_point() +
  geom_smooth(method = "loess")

### summarise to weekly intervals?

### Boxplots by all months

ggplot(data = dailyMeans, aes(x = as.factor(YEarMonth), y = log(meanBiomass), fill = Season)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

## boxplot by month groups (12 only)
ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()

ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_violin()

# Incorporate both violin and boxplot
ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)

## some sort of matrix to show whcih months are sign different and which aren't

### boxplot plots by season
# this will compare the seasonal difference over the sampling period, therfore only 4 groups (seasons)
ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()


ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()

# violin plot to show how the data are distributed.(Find a better violin example)
# winter and spring are less distributed, less fluctuations
ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_violin() +
  coord_flip()

## seasonOther
ggplot(data = SeasonOther_dailymeans, aes(x = season_yearOther, y = log(meanBiomass), fill = seasonOther)) +
  geom_boxplot()

## THIS IS USED TO SHOW SEASONS WHERE DECEMBER FOLLOWS ON TO THE NEXT YEAR
ggplot(data = sdf, aes(x = reorder(YearMonthOther, Date), y = log(meanBiomass), fill = seasonOther)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)

## stats to show signif diff between seasons. 



### rose chart of dino vs diatom per month


rc <- PS %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  group_by(Date, months, month, Classification) %>% 
  summarise(meanClassification = mean(Cells.L, na.rm = TRUE))

ggplot(data = rc, aes(x = reorder(months, month), y = log(meanClassification), fill = Classification)) +
  geom_boxplot(outlier.shape=NA) +
  coord_polar()

  
  
  
  
  
  
  
  

## stats to show signif diff between dino-diatom