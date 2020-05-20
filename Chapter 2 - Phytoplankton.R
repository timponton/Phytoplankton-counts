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
library(funrar)
library(jtools)
library(ggstance)
library(RColorBrewer)
library(ggpubr)


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
  
ggplot(data = PS, aes(x = log(Cells.L))) +
  geom_density()




shapiro.test(dailyMeans$meanBiomass)

#model <- glm(log1Mean ~ month + year, family = Gamma(link = "inverse"), data = dailyMeans)

#summ(model)

#effect_plot(model, pred = month, interval = TRUE, plot.points = TRUE)




# Total data view ---------------------------------------------------------








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
  geom_point(aes(col = Season)) +
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






# Maximum values used  ----------------------------------------------------


maximal <- PS %>% 
  group_by(Date, months, month) %>% 
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
maximal %>% 
  filter(FWC_mod == "Medium" | FWC_mod == "High" | FWC_mod == "Low") %>% 
  group_by(months, month, FWC_mod) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  ggplot(., aes(x = reorder(months, month), y = n, fill = FWC_mod)) +
  geom_col(position = "dodge")


anotherMean <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, Time, months, month, year, Season, YEarMonth, SeasonOrder, FWC_mod) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass))

ggplot(data = maximal, aes(x = Date, y = log1p(totalCells))) + 
  geom_line(size = 0.1) +
  geom_point(aes(col = FWC_mod), size = 2) +
  theme_classic()


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


# When diveristy can be measured ------------------------------------------

# summarise to daily means for each species
Sp_only <- PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(Date, Species, Classification, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(Date, Species, Classification, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  ggplot(., aes(x = log10(Cells.L))) +
  geom_histogram(bins = 30) +
  theme_classic()


# calculate total density by adding all average species up per day they were sampled to get daily total cells
total_Sp_cells <- Sp_only %>% 
  group_by(Date, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(total_biomass = sum(meanCells)) %>% 
  ungroup()

Sp_only %>% 
  group_by(Date, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(mC = mean(meanCells)) %>% 
  ggplot(., aes(x = Date, y = log1p(mC))) +
  geom_point(aes(col = Season)) +
  geom_line(size = 0.2) +
  theme_classic()

#For 3 days
Sp_only %>% 
  mutate(third = round_date(Date, "3 day")) %>% 
           group_by(third, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
           summarise(thirdmean = mean(meanCells, na.rm = TRUE)) %>% 
           ggplot(., aes(x = third, y = log10(thirdmean))) +
           geom_point(aes(col = Season)) +
           geom_line() +
  theme_classic()

# boxplots

Sp_only %>% 
  group_by(Date, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(mC = mean(meanCells)) %>% 
  ggplot(., aes(x = reorder(months, month), y = log1p(mC), fill = Season)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  theme_classic()

# calculate species richness (S) by calculating number of species per day
richness <- Sp_only %>% 
  group_by(Date) %>% 
  summarise(S = n_distinct(Species))
Sp_only %>% 
  group_by(YEarMonth, Season) %>% 
  summarise(S = n_distinct(Species)) %>% 
  ggplot(., aes(x = as.factor(YEarMonth), y = S)) +
  geom_col(aes(fill = Season), col = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

# diversity of each day by calculating shannon index (H') for each day 
div <- Sp_only %>% 
  ungroup() %>% 
  select(Date, Species, meanCells) %>% 
  pivot_wider(names_from = Species, values_from = meanCells, 
              values_fill = list(meanCells = 0))

group_date <- div %>% 
  select(Date)

group_date <- group_date %>% 
  mutate(shannon2 = diversity(div[, 2:ncol(div)], "shannon", base = 2),
         shannon = diversity(div[, 2:ncol(div)], "shannon"),
         simpson = diversity(div[, 2:ncol(div)], "simpson"), 
         Inv_simpson = diversity(div[, 2:ncol(div)], "inv"))

# evenness  by calculating Pielou’s index (J’) for each day 
# calculated as J' = H'/log(S)

# join S, H' and J' together to total daily biomass df

group_date # H' 
Richness # S
total_Sp_cells # biomass

# join
dailyDesc <- left_join(total_Sp_cells, group_date, by = "Date") %>% 
  left_join(., richness, by = "Date")

# calculate J'
dailyDesc <- dailyDesc %>% 
  mutate(J2 = shannon2/log(S), 
         J = shannon/log(S))

ggplot(data = dailyDesc, aes(x = shannon, y = J)) +
  geom_point(aes(col = FWC_mod)) +
  geom_smooth(method = "lm")


## using graphs to check data 
# total cells over dates when diversity cna be calcualated
ggplot(data = total_Sp_cells, aes(x = Date, y = log1p(total_biomass))) +
  geom_point(aes(col = Season)) +
  geom_line(size = 0.5) +
  theme_classic()


ggplot(data = Sp_only, aes(x = Date, fill = months)) +
  geom_histogram(bins = 300)


# how does diversity change over time, by rate of richness
ggplot(data = dailyDesc, aes(x = Date, y = shannon, col = S)) +
  geom_point() +
  geom_line()

# relationship between biomass and richness
ggplot(data = dailyDesc, aes(x = log1p(total_biomass), y = S)) +
  geom_point() +
  geom_smooth()

# relationship between biomass and evenness
ggplot(data = dailyDesc, aes(x = log1p(total_biomass), y = J)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = dailyDesc, aes(x = as.factor(YEarMonth), y = S, fill = Season)) +
  geom_boxplot()

Sp_only %>% 
  group_by(year) %>% 
  filter(Classification != "Various") %>% 
  summarise(n = n_distinct(Species))

ggplot(data = dailyDesc, aes(x = as.factor(YEarMonth), y = S)) +
  geom_boxplot(aes(fill = Season))

dailyDesc %>% 
  pivot_longer(-c(1:9), names_to = "statistic", values_to = "measure") %>% 
  filter(statistic %in% c("simpson", "shannon")) %>% 
  ggplot(., aes(x = as.factor(YEarMonth), y = measure)) +
  geom_boxplot(aes(fill = Season)) +
  facet_wrap(~statistic, ncol = 1, scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_classic()

dailyDesc %>% 
  pivot_longer(-c(1:9), names_to = "statistic", values_to = "measure") %>% 
  filter(statistic %in% c("S", "J")) %>% 
  ggplot(., aes(x = as.factor(YEarMonth), y = measure)) +
  geom_boxplot(aes(fill = Season)) +
  facet_wrap(~statistic, ncol = 1, scales = "free_y") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  theme_classic()


# Diatom-Dino fluctuations ------------------------------------------------

Sp_only %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  ggplot(., aes(x = reorder(months, month), y = log(meanCells), fill = Classification)) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_classic()

PS %>% 
  filter(Classification %in% c("Dinoflagellate", "Diatom")) %>% 
  ggplot(., aes(x = reorder(months, month), y = log10(Cells.L), fill = Classification)) +
  geom_boxplot()

###

# NMDS on daily densities etc ---------------------------------------------

# NMDS of all data per month

testmonth <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>%
  #mutate(spAb = abbreviate(Species, 5, strict = FALSE)) %>% 
  group_by(months, Species) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>%
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")


NMDStestmonth <- metaMDS(testmonth, k = 3, autotransform = TRUE)

NMDStestmonth
stressplot(NMDStestmonth)



plot(NMDStestmonth)
ordiplot(NMDStestmonth,type="n")
orditorp(NMDStestmonth,display="species",col="red",air=0.01)
orditorp(NMDStestmonth,display="sites",cex=1.25,air=0.01)


# ANOSIM ------------------------------------------------------------------

ANOSIM_month <- Sp_only %>%
  ungroup() %>% 
  select(-Classification) %>% 
  filter(Species != "Diatom") %>%
  group_by(months, year, Species) %>% 
  summarise(cells = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = cells, 
              values_fill = list(cells = 0))

com <- ANOSIM_month[, 3:ncol(ANOSIM_month)]
m_com <- as.matrix(com)

ano <- anosim(m_com, ANOSIM_month$year, distance = "bray", permutations = 9999)
ano
summary(ano)
plot(ano)


# 2019
test <- Sp_only %>%
  ungroup() %>% 
  filter(year == "2019") %>%
  filter(Species != "Diatom") %>% 
  group_by(months, Species) %>% 
  summarise(averageDens = log1p(mean(meanCells, na.rm = TRUE))) %>%
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")






NMDS <- metaMDS(testmonth, k = 2, autotransform = TRUE)

NMDS
stressplot(NMDS)



plot(NMDS)
ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)
orditorp(NMDS,display="sites",cex=1.25,air=0.01)


# ANOSIM of months 



## create data frame sna dplot in ggplot2
# create dataframes
Sp_NMDS <- as.data.frame(NMDS[["species"]]) %>% 
  rownames_to_column(var = "variable") %>% 
  mutate(variableType = "Species")
months_MNDS <- as.data.frame(NMDS[["points"]]) %>% 
  rownames_to_column(var = "variable") %>% 
  mutate(variableType = "Month")

# bind df's
NMDS_2019 <- rbind(months_MNDS, Sp_NMDS)

# plot in ggplot2
ggplot(data = NMDS_2019, aes(x = MDS1, y = MDS2)) +
  geom_point() +
  geom_text(aes(label = variable, col = variableType)) +
  scale_color_manual(values=c("Black", "red"))

plot_ly(NMDS_2019, x = ~MDS1, y = ~MDS2, z = ~MDS3, color = ~variableType, text = ~variable)

scatter3

# 2018

test18 <- Sp_only %>%
  ungroup() %>% 
  filter(year == "2018") %>% 
  group_by(months, Species) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")


NMDS18 <- metaMDS(test18, k = 2)
stressplot(NMDS18)


plot(NMDS18)
ordiplot(NMDS18,type="n")
orditorp(NMDS18,display="species",col="red",air=0.01)
orditorp(NMDS18,display="sites",cex=1.25,air=0.01)

# 2017
test17 <- Sp_only %>%
  ungroup() %>% 
  filter(year == "2017") %>% 
  group_by(months, Species) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")


NMDS17 <- metaMDS(test17, k = 2)
stressplot(NMDS17)


plot(NMDS17)
ordiplot(NMDS17,type="n")
orditorp(NMDS17,display="species",col="red",air=0.01)
orditorp(NMDS17,display="sites",cex=1.25,air=0.01)


## another NMDS method 

another <- Sp_only %>%
  ungroup() %>% 
  filter(year != "2020") %>%
  group_by(year, months, Species) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) 

another_mat <- another[, 3:ncol(another)]
m_another_mat <- as.matrix(another_mat)

another_NMDS <- metaMDS(m_another_mat, k = 3, distance = "bray")
another_NMDS
plot(another_NMDS)
another_data.scores <- as.data.frame(scores(another_NMDS))

another_data.scores$year <- another$year
another_data.scores$months <- another$months
allOf <- ggplot(another_data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 9, aes(shape = year, col = months))

ggplot(another_data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point()


first <- another_data.scores %>% 
  filter(months %in% c("March", "February", "January")) %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 9, aes(shape = year, col = months)) +
  geom_path()

allOf + first


scori <- as.data.frame(another_NMDS[["species"]]) %>% 
  rownames_to_column(var = "Species")

plot_ly(scori, x = ~MDS1, y = ~MDS2, z = ~MDS3)

# Specific species  -------------------------------------------------------

dominantSp <- PS %>% 
  group_by(Date) %>% 
  filter(any(Species == "Lingulodinium polyedra" | Species == "Gonyaulax polygramma"))

dominantSp %>% 
  group_by(Date, months, month, year, Species) %>% 
  summarise(averaging = mean(Cells.L, na.rm = TRUE)) %>% 
  ggplot(., aes(x = Date, y = averaging, col = Species)) +
  geom_point()


