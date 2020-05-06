#### load packages####
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
library(GGally)
install.packages("ggstance")
library(jtools)
library(ggstance)


### read in data and fix####

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
  summarise(totalCells = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High"))))))) %>% 
  mutate(FWC_mod = ifelse(totalCells >= 1 & totalCells <= 1000, "Normal", 
                          ifelse(totalCells >= 1001 & totalCells <= 10000, "Very Low", 
                                 ifelse(totalCells >= 10001 & totalCells <= 100000, "Low", 
                                        ifelse(totalCells >= 100001 & totalCells < 1000000, "Medium", "High")))))   
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


##### subset Primary sump####
PS <- df %>% 
  filter(Site == "Primary sump")

PS <- PS %>% 
  mutate(roundedCounts = round(Cells.L, digits = 0))
### Summarise to daily counts####

dailyMeans <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass)) %>% 
  mutate(whatDay = weekdays(Date))

dailyMeans %>%
  mutate(whatWeek = week(Date)) %>% 
  group_by(whatWeek, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(weekmeanBiomass = mean(log1Mean, na.rm = TRUE)) %>% 
  ggplot(.,aes(x = whatWeek, y = weekmeanBiomass)) +
  geom_point() +
  geom_line() +
  facet_wrap(~year, ncol = 3)
  



model <- glm(log1Mean ~ month + year, family = Gamma(link = "inverse"), data = dailyMeans)

summ(model)

effect_plot(model, pred = month, interval = TRUE, plot.points = TRUE)




dailyMeans %>% 
  mutate(third = round_date(Date, "5 day")) %>% 
  group_by(whatDay, third, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(thirdmean = mean(meanBiomass, na.rm = TRUE)) %>% 
  ggplot(., aes(x = third, y = log(thirdmean))) +
  geom_point(aes(col = whatDay))

PS %>% 
  ggplot(., aes(x = log1p(Cells.L))) +
  geom_histogram() +
  facet_wrap(~FWC_mod, ncol = 1)

PS %>% 
  ggplot(., aes(x = Date, y = log1p(Cells.L))) +
  geom_point(aes(col = FWC_mod))

##### round off and summarise to the nearest 3 days####
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

### line graph of all dates, trans = "log10" or whatever####
ggplotly(ggplot(data = dailyMeans, aes(x = Date, y = log(meanBiomass))) +
  geom_line())

ggplot(data = dailyMeans, aes(x = Date, y = log(meanBiomass))) +
  geom_point() +
  geom_smooth(method = "loess")

### summarise to weekly intervals? ####

### Boxplots by all months ####

ggplot(data = dailyMeans, aes(x = as.factor(YEarMonth), y = log(meanBiomass), fill = Season)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

## boxplot by month groups (12 only)####
ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()

ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_violin()

##### Incorporate both violin and boxplot####
ggplot(data = dailyMeans, aes(x = reorder(months, month), y = log(meanBiomass), fill = Season)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)

###### some sort of matrix to show which months are sign different and which aren't ####

### boxplot plots by season ####
# this will compare the seasonal difference over the sampling period, therfore only 4 groups (seasons)
ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()


ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_boxplot()

# violin plot to show how the data are distributed.(Find a better violin example)
# winter and spring are less distributed, less fluctuations
ggplot(data = dailyMeans, aes(x = reorder(Season, SeasonOrder), y = log(meanBiomass), fill = Season)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2)

###### seasonOther ####
ggplot(data = SeasonOther_dailymeans, aes(x = season_yearOther, y = log(meanBiomass), fill = seasonOther)) +
  geom_boxplot()

###### THIS IS USED TO SHOW SEASONS WHERE DECEMBER FOLLOWS ON TO THE NEXT YEAR ####
ggplot(data = sdf, aes(x = reorder(YearMonthOther, Date), y = log(meanBiomass), fill = seasonOther)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

###### stats to show signif diff between seasons. ####
I


### rose chart of dino vs diatom per month


rc <- PS %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  group_by(Date, months, month, Classification) %>% 
  summarise(meanClassification = mean(Cells.L, na.rm = TRUE))

ggplot(data = rc, aes(x = reorder(months, month), y = log(meanClassification), fill = Classification)) +
  geom_boxplot(outlier.shape=NA)

  
  

  
  
  
  
  

## stats to show signif diff between dino-diatom

# Dino-diatom corr

swing <- rc %>% 
  pivot_wider(names_from = c("Classification"), values_from = "meanClassification", 
              values_fill = list(meanClassification = 0))

ggplot(data = swing, aes(x = Dinoflagellate, y = Diatom)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                    scientific = FALSE)) +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                    scientific = FALSE))

swing <- swing %>% 
  group_by(months, month) %>% 
  summarise_at(c("Dinoflagellate", "Diatom"), mean, na.rm = TRUE) %>% 
  pivot_wider(names_from = "months", values_from = "Diatom")

mC <- PS %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  group_by(Date, months, Classification) %>% 
  summarise(meanClassification = mean(Cells.L, na.rm = TRUE)) %>%
  ungroup() %>%
  unite(Clas_month, c("months", "Classification")) %>% 
  pivot_wider(names_from = "Clas_month", values_from = "meanClassification", 
              values_fill = list(meanClassification = 0))

ggcorr(mC[, 2:25], geom = "circle")

monthsCorr <- cor(mC[, 2:25])

jan <- PS %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  filter(months == "January") %>% 
  group_by(Date, Classification) %>% 
  summarise(meanClassification = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Classification", values_from = "meanClassification")
  








##### Species fluctuations ####

checking <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, months, month, year, Season, YEarMonth, SeasonOrder, FWC_mod) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass)) %>% 
  mutate(whatDay = weekdays(Date))



ggplotly(ggplot(data = maximal, aes(x = Date, y = log1p(totalCells))) +
  geom_point(aes(col = FWC_mod)) +
  geom_line(size = 0.1))


# Maximum values used  ----------------------------------------------------


maximal <- PS %>% 
  group_by(Date, months) %>% 
  summarise(totalCells = max(Cells.L)) %>% 
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High"))))))) %>% 
  mutate(FWC_mod = ifelse(totalCells >= 1 & totalCells <= 1000, "Normal", 
                          ifelse(totalCells >= 1001 & totalCells <= 10000, "Very Low", 
                                 ifelse(totalCells >= 10001 & totalCells <= 100000, "Low", 
                                        ifelse(totalCells >= 100001 & totalCells < 1000000, "Medium", "High")))))

maximal %>% 
  filter(FWC_mod == "Medium" | FWC_mod == "High") %>% 
  group_by(months, FWC_mod) %>% 
  tally() %>% 
  arrange(desc(n))



anotherMean <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, Time, months, month, year, Season, YEarMonth, SeasonOrder, FWC_mod) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass))



# group by times ----------------------------------------------------------------


timing <- PS %>% 
  group_by(Date, Time, months) %>% 
  summarise(totalCells = mean(Cells.L)) %>%
  ungroup() %>%
  group_by(Date, months) %>% 
  summarise(timingCells = mean(totalCells, na.rm = TRUE))
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High"))))))) %>% 
  mutate(FWC_mod = ifelse(totalCells >= 1 & totalCells <= 1000, "Normal", 
                          ifelse(totalCells >= 1001 & totalCells <= 10000, "Very Low", 
                                 ifelse(totalCells >= 10001 & totalCells <= 100000, "Low", 
                                        ifelse(totalCells >= 100001 & totalCells < 1000000, "Medium", "High")))))

MedHighRT <- maximal %>% 
  filter(FWC_mod == "Medium" | FWC_mod == "High") %>% 
  group_by(months, FWC_mod) %>% 
  tally() %>% 
  arrange(desc(n))


# Sum ind species to get total cells/day ----------------------------------

biomass <- PS %>% 
  group_by(Date, months, month, Species) %>% 
  summarise(dayMean = max(Cells.L, na.rm = TRUE)) %>% 
  summarise(dayTotal = sum(dayMean))

ggplot(data = biomass, aes(x = Date, y = log1p(dayTotal))) +
  geom_point()


# Specific species  -------------------------------------------------------

dominantSp <- PS %>% 
  group_by(Date) %>% 
  filter(any(Species == "Lingulodinium polyedra" | Species == "Gonyaulax polygramma"))

dominantSp %>% 
  group_by(Date, months, month, year, Species) %>% 
  summarise(averaging = mean(Cells.L, na.rm = TRUE)) %>% 
  ggplot(., aes(x = Date, y = averaging, col = Species)) +
  geom_point()
