
# Load data and clean -----------------------------------------------------

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
library(jtools)
library(ggstance)
library(funrar)


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
  filter(Site == "Primary sump") %>% 
  filter(Species != "Diatom")

### Summarise to daily counts####

dailyMeans <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass)) %>% 
  mutate(whatDay = weekdays(Date))

# Wrangle data ------------------------------------------------------------

## Species biomass per day
# summarise to daily means for each species
Sp_only <- PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(Date, Species, Classification, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))


## Total biomass per day
# calculate total density by adding all average species up per day they were sampled to get daily total cells
total_Sp_cells <- Sp_only %>% 
  group_by(Date, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(total_biomass = sum(meanCells)) %>% 
  ungroup()

## Species richness
# calculate species richness (S) by calculating number of species per day
richness <- Sp_only %>% 
  group_by(Date) %>% 
  summarise(S = n_distinct(Species))

## Diversity indices
# diversity of each day by calculating shannon index (H') for each day 

# Pivot daily counts for each species wider
div <- Sp_only %>% 
  ungroup() %>% 
  select(Date, Species, meanCells) %>% 
  pivot_wider(names_from = Species, values_from = meanCells, 
              values_fill = list(meanCells = 0))

# create groupings
group_date <- div %>% 
  select(Date)

# Diversity attached to groups
group_date <- group_date %>% 
  mutate(shannon2 = diversity(div[, 2:ncol(div)], "shannon", base = 2),
         shannon = diversity(div[, 2:ncol(div)], "shannon"),
         simpson = diversity(div[, 2:ncol(div)], "simpson"), 
         Inv_simpson = diversity(div[, 2:ncol(div)], "inv"))



# evenness  by calculating Pielou’s index (J’) for each day 
# calculated as J' = H'/log(S)

# join S, H' and J' together to total daily biomass df

# group_date # H' 
# Richness # S
# total_Sp_cells # biomass


# Combine all measures
dailyDesc <- left_join(total_Sp_cells, group_date, by = "Date") %>% 
  left_join(., richness, by = "Date")

# calculate J'
dailyDesc <- dailyDesc %>% 
  mutate(J2 = shannon2/log(S), 
         J = shannon/log(S))




# Create monthly aggregates -----------------------------------------------


  
#'*year-monthly biomass (1) ---------------*
# Plot monthly total biomass, comparing years
Sp_only %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = log10(monthlyMean), col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Log10 phyotplankton counts") + 
  ggtitle("Comparing the log10 monthly mean phytoplankton counts between three consecutive years ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  




# PS %>% 
#   filter(year != "2020") %>% 
#   filter(Classification != "Various") %>% 
#   group_by(months, month, year, Season) %>% 
#   summarise(monthlyMean = mean(Cells.L, na.rm = TRUE)) %>% 
#   ggplot(., aes(x = reorder(months, month), y = log10(monthlyMean), col = year, group = year)) +
#   geom_line(size = 2) +
#   theme_classic()

## Monthly averages for each species
SP_monthly <- PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(months, month, year, 
           YEarMonth, Species, Classification) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

#'*year-monthly metrics (1)*
## comparing years over months for indices



#'*(1) ------------------*
# Richness
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyRichness = mean(S, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyRichness, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Species Richness") + 
  ggtitle("Comparing the monthly species richness of phytoplankton counts between three consecutive years ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'*(1) -----------------*
# Evenness (J')
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyEvenness = mean(J, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyEvenness, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Pielou's J' evenness") + 
  ggtitle("Comparing the monthly species evenness (J') of phytoplankton counts between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#'*(1) ---------------*
# Shannon
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyshannon = mean(shannon, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyshannon, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Shannon Index of Diversity") + 
  ggtitle("Comparing the monthly Shannon (H') Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*(1) ----------------*
# Simpson
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlysimpson = mean(simpson, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlysimpson, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Simpson Index of Diversity") + 
  ggtitle("Comparing the monthly Simpson Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



# Phyto Class diversity ---------------------------------------------------

# stacked percentage bar graph
PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(months, month, year, Season, Classification) %>% 
  summarise(monthlyMean = mean(Cells.L, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  theme_classic()

#'*[Stacked bar graph] of different phyto classes, by year-months -------------*
# from Sp_Only df all years together
Sp_only %>% 
  group_by(months, month, year, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  xlab("Months") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*[Stacked bar graph] of different phyto classes, by months, facetd by year ------------------*
# from Sp_Only df facet_wrap(~year)
Sp_only %>% 
  group_by(months, month, year, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'*Add this in as a comparison ------------*
# All classes plot together
Sp_only %>% 
  group_by(YEarMonth, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = YEarMonth, y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  xlab("Month Year") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



## when were diatoms or dinos dominant
# mutate dominants
DD_dom <- Sp_only %>% 
  group_by(months, month, year, YEarMonth, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Classification", values_from = "monthlyMean",
              values_fill = list(monthlyMean = 0)) %>% 
  mutate(Dominant = ifelse(Diatom > Dinoflagellate, "Diatom", 
                           ifelse(Diatom < Dinoflagellate, "Dinoflagellate", "Other"))) %>% 
  pivot_longer(-c(months, month, year, YEarMonth, Season, Dominant), names_to = "Classification", values_to = "monthlyMean")

all_dom <- DD_dom %>% 
  select(-c(Classification, monthlyMean))

Diatom_dom <- DD_dom %>% 
  filter(Dominant == "Diatom") %>% 
  select(months, month, year, YEarMonth, Season, Dominant)

Dino_dom <- DD_dom %>% 
  filter(Dominant == "Dinoflagellate") %>% 
  select(months, month, year, YEarMonth, Season, Dominant)


#'*When either Diatoms or Dino's are dominant over the other --------*
# plot dominants
ggplot(DD_dom, aes(x = YEarMonth, y = sqrt(monthlyMean), fill = Dominant)) +
  geom_bar(stat = "Identity") +
  xlab("Month Year") +
  ylab("Monthly Mean of all Phytoplankton") + 
  labs(fill = "Dominant phytoplankton class") +
  ggtitle("Investigate which phytoplankton classes are dominant on a monthly basis over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Include -----------------*
# all month aggregated togtehr to see when diatoms or dino's are dominant over the other
Sp_only %>% 
  group_by(months, month, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Classification", values_from = "monthlyMean",
              values_fill = list(monthlyMean = 0)) %>% 
  mutate(Dominant = ifelse(Diatom > Dinoflagellate, "Diatom", 
                           ifelse(Diatom < Dinoflagellate, "Dinoflagellate", "Other"))) %>% 
  pivot_longer(-c(months, month, Season, Dominant), names_to = "Classification", values_to = "monthlyMean") %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Dominant)) +
  geom_bar(stat = "Identity") +
  xlab("Months") +
  ylab("Monthly Mean of all Phytoplankton") + 
  labs(fill = "Dominant phytoplankton class") +
  ggtitle("Investigate which phytoplankton classes are dominant on a monthly basis over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




## Join dominant class to monthly species data

Sp_monthly_with.Dom <- SP_monthly %>% 
  left_join(all_dom, by = c("months", "month", "year"))








# quantifying resources ---------------------------------------------------

#total per time
max_cells_day <- PS %>% 
  filter(year == "2019") %>%   # 2019 subsetted as previous years did not employ fine-scale monitoring
  group_by(Date, Time, months, month, Species) %>% 
  summarise(total.per.time = sum(Cells.L)) %>% 
  ungroup() %>% 
  group_by(Date, Time, months, month) %>% 
  summarise(totTime = sum(total.per.time)) %>% 
  ungroup() %>% 
  group_by(Date, months, month) %>% 
  summarise(totalCells = max(totTime, na.rm = TRUE)) %>% 
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



## create df with red tide classes
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


# calculate the number of samples collected per day (FIND SOLUTIONS)
Time_count <- PS %>% 
  group_by(Date) %>% 
  summarise(count_times = n_distinct(Time))

Time_count %>% 
  left_join(select(PS, Date, months, month, year, YEarMonth), by = "Date", keep = FALSE) %>% 
  group_by(months, month, year) %>% 
  summarise(meanTimeCounts = mean(count_times, na.rm = TRUE))


# make classes as.factor
max_cells_day$FWC_mod <- as.factor(max_cells_day$FWC_mod)
# check factors
levels(max_cells_day$FWC_mod)
# set order of factors 
max_cells_day$FWC_mod <-  factor(max_cells_day$FWC_mod, 
                                     levels = c("High","Medium", "Low", "Very Low", "Normal"))


#'*Compare overall cells vs dates, sized by counting times ------------*
# how do counting times change over time, linked to total cells 
left_join(max_cells_day, Time_count, by = "Date") %>% 
  filter(count_times > 0) %>% 
  ggplot(., aes(x = Date, y = log(totalCells))) +
  geom_line() +
  geom_point(aes(col = FWC_mod, size = count_times)) +
  xlab("Date") +
  ylab("Log Phytoplankton counts") + 
  labs(col = "Red Tide class", size = "Samples collected per day") +
  ggtitle("Investigating how sampling frequency varied in relation to red tide severity over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# calculate average counting times/day
left_join(max_cells_day, Time_count, by = "Date") %>% 
  ungroup() %>% 
  summarise(meanc = mean(count_times, na.rm = TRUE))


# merge red tide dates and number of times sampled per day and filter above avergae counting days
resources <- left_join(max_cells_day, Time_count, by = "Date") %>% 
  filter(count_times > 0) # > average counts per day

#'* USE TO COMPARE EXCLUDING ALL vs BELOW AVERAGE REGRESSIONS [Scatter and relationship line (smooth)] used to show counting times correlation, above daily counting average ------------*
# correlation between total cells counted and numebr of times counted per day
ggplot(left_join(max_cells_day, Time_count, by = "Date") %>% 
         filter(count_times > 0), aes(x = log(totalCells), y = count_times)) +
  geom_point(aes(col = FWC_mod)) +
  geom_smooth(method = "lm", aes(col = FWC_mod), se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Log Cells/L") +
  ylab("Number samples collected per day") + 
  labs(col = "Red Tide class") +
  ggtitle("Relationship between Cells/L counted and number of sampels collected per day, when different Red tide severity classes were experienced") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'* CHANGE PARAMETER [Scatter and relationship line (smooth)] used to show counting times correlation, above daily counting average ------------*
# correlation between total cells counted and numebr of times counted per day
ggplot(left_join(max_cells_day, Time_count, by = "Date") %>% 
         filter(count_times > 3.12), aes(x = log(totalCells), y = count_times)) +
  geom_point(aes(col = FWC_mod)) +
  geom_smooth(method = "lm", aes(col = FWC_mod), se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Log Cells/L") +
  ylab("Number samples collected per day") + 
  labs(col = "Red Tide class") +
  ggtitle("Relationship between Cells/L counted and number of sampels collected per day, when different Red tide severity classes were experienced") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Compare overall cells vs dates above average number of counting times, sized by counting times*
# how do counting times change over time, linked to total cells 
ggplot(data = resources, aes(x = Date, y = log(totalCells))) +
  geom_line() +
  geom_point(aes(col = FWC_mod, size = count_times))



# number of times different red tide classes were experienced per  --------

#total per time
all_max_cells_day <- PS %>% 
  group_by(Date, Time, months, month, Species) %>% 
  summarise(total.per.time = sum(Cells.L)) %>% 
  ungroup() %>% 
  group_by(Date, Time, months, month) %>% 
  summarise(totTime = sum(total.per.time)) %>% 
  ungroup() %>% 
  group_by(Date, months, month) %>% 
  summarise(totalCells = max(totTime, na.rm = TRUE)) %>% 
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



# make classes as.factor
all_max_cells_day$FWC_mod <- as.factor(all_max_cells_day$FWC_mod)
# check factors
levels(all_max_cells_day$FWC_mod)
# set order of factors 
all_max_cells_day$FWC_mod <-  factor(all_max_cells_day$FWC_mod, 
                                 levels = c("High","Medium", "Low", "Very Low", "Normal"))
# join year columns, tally per class per month per year (remove 2020)
RT.classNumbers_monthYear <- all_max_cells_day %>% 
  left_join(select(PS, Date, months, year, YEarMonth), by = "Date", keep = FALSE) %>%
  filter(year != "2020") %>% 
  group_by(months.x, month, year, YEarMonth, FWC_mod) %>% 
  tally() %>% 
  arrange(desc(n))

#'*Compare Red tide classes per month ---------*
# Compare the proportion of differnt red tide classes per month over the three years
ggplot(RT.classNumbers_monthYear, aes(x = reorder(months.x, month), y = n, fill = FWC_mod)) +
  geom_bar(stat = "Identity", position = "fill") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion number of days") + 
  labs(fill = "Red Tide class") +
  ggtitle("Compare the proportion of different red tide classes per month over the three years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




# NMDS --------------------------------------------------------------------



# finding relative abundance across total samples
te <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
  mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
  group_by(Sp_abb) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Sp_abb, values_from = averageDens, 
              values_fill = list(averageDens = 0)) 

tet <- make_relative(as.matrix(te))

tet <- as.data.frame(tet)

se <- tet %>% 
  gather(key = "Sp_abb", value = "abun") %>% 
  arrange(desc(abun)) %>% 
  filter(abun > 0.003)

se %>% 
  summarise(S = n_distinct(Sp_abb))


ggplotly(ggplot(data = se, aes(x = reorder(Sp_abb, abun), y = abun)) +
  geom_col() +
  coord_flip())


# relative abudance per month
mte <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
  mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
  group_by(months, Sp_abb) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Sp_abb, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")

mtet <- make_relative(as.matrix(mte))

mtet <- as.data.frame(mtet)%>% 
  rownames_to_column(var = "months")



mse <- mtet %>%
  pivot_longer(-months, names_to = "Sp_abb", values_to = "abun") %>% 
  group_by(months) %>% 
  arrange(desc(abun)) %>% 
  filter(abun > 0.01)

#'*Use to show all species 97 sp --*
# All species
ggplot(data = mtet %>%
         pivot_longer(-months, names_to = "Sp_abb", values_to = "abun") %>% 
         group_by(months) %>% 
         arrange(desc(abun)), aes(x = reorder(Sp_abb, abun), y = abun)) +
  geom_col(position = "Dodge") +
  coord_flip() +
  xlab("Species") +
  ylab("Relative abundance") +
  ggtitle("Relative abundance of species") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Use to show dom species species 73 sp --*
# Species above relative abundance of 0.01 per month
ggplot(data = mtet %>%
         pivot_longer(-months, names_to = "Sp_abb", values_to = "abun") %>% 
         group_by(months) %>% 
         arrange(desc(abun)) %>% 
         filter(abun > 0.01), aes(x = reorder(Sp_abb, abun), y = abun)) +
  geom_col(position = "Dodge") +
  coord_flip() +
  xlab("Species") +
  ylab("Relative abundance") +
  ggtitle("Species with a relative abundance > 0.01 per month") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


mse %>% 
  ungroup() %>% 
  summarise(S = n_distinct(Sp_abb))



## Overall months combined
# summarise species to monthly aggre. and abbrev. species names after log10 transforming
monthly.species <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
  mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
  group_by(months, Sp_abb) %>% 
  summarise(averageDens = log10(mean(meanCells, na.rm = TRUE))) %>% 
  semi_join(., mse, by = "Sp_abb")


monthly.species %>% 
  semi_join(., mse, by = "Sp_abb") %>% 
  ungroup() %>% 
  summarise(S = n_distinct(Sp_abb))

# Pivot wider and change months column to rowname
monthly.species_wide <- monthly.species %>% 
  pivot_wider(names_from = Sp_abb, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="months")



# whether to use relative abundance for NMDS or not
# calculate relative abundances per month
monthly.species_wide <- as.matrix(monthly.species_wide)
rel_monthly.Sp <- make_relative(monthly.species_wide)


# run NMDS (check to see what data are being used, rel abund or ave counts)
NMDStestmonth <- metaMDS(monthly.species_wide, k = 3, autotransform = FALSE, trymax = 100)

# check results
NMDStestmonth
# stressplot of NMDS results
stressplot(NMDStestmonth)


#'*[NMDS] check what it is based on i.e relative abundance or actual counts ------*
plot(NMDStestmonth)
ordiplot(NMDStestmonth,type="n")
orditorp(NMDStestmonth,display="species",col="red",air=0.01)
orditorp(NMDStestmonth,display="sites",cex=1.25,air=0.01)

###

# PS %>% 
#   filter(Species != "Diatom") %>% 
#   filter(Classification != "Various") %>% 
#   group_by(months, Species) %>% 
#   summarise(averageDens = mean(Cells.L, na.rm = TRUE))



## Original try, with no Abrev. Kept for just incase 
testmonth <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
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


# ## individual months of individual years (group_by YEarMonth) (DOES NOT WORK< KEEPING FOR INCASE)
# 
# YM <- Sp_only %>%
#   ungroup() %>%
#   filter(Species != "Diatom") %>%
#   mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>%
#   group_by(YEarMonth, Sp_abb) %>%
#   summarise(averageDens = log10(mean(meanCells, na.rm = TRUE))) %>% 
#   semi_join(., mse, by = "Sp_abb")
# 
# 
# YM %>% 
#   semi_join(., mse, by = "Sp_abb") %>% 
#   ungroup() %>% 
#   summarise(S = n_distinct(Sp_abb))
# 
# # Pivot wider and change months column to rowname
# YMmonthly.species_wide <- YM %>% 
#   pivot_wider(names_from = Sp_abb, values_from = averageDens, 
#               values_fill = list(averageDens = 0)) %>% 
#   remove_rownames %>% 
#   column_to_rownames(var="YEarMonth")
# 
# # whether to use relative abundance for NMDS or not
# # calculate relative abundances per month
# YMmonthly.species_wide <- as.matrix(YMmonthly.species_wide)
# YMrel_monthly.Sp <- make_relative(YMmonthly.species_wide)
# 
# 
# # run NMDS (check to see what data are being used, rel abund or ave counts)
# NMDStestmonth <- metaMDS(YMrel_monthly.Sp, k = 3, autotransform = FALSE)
# 
# # check results
# NMDStestmonth
# # stressplot of NMDS results
# stressplot(NMDStestmonth)
# 
# plot(NMDStestmonth)
# ordiplot(NMDStestmonth,type="n")
# orditorp(NMDStestmonth,display="species",col="red",air=0.01)
# orditorp(NMDStestmonth,display="sites",cex=1.25,air=0.01)
# 
# 
# 
# mte <- Sp_only %>%
#   ungroup() %>% 
#   filter(Species != "Diatom") %>% 
#   mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
#   group_by(YEarMonth, Sp_abb) %>% 
#   summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
#   pivot_wider(names_from = Sp_abb, values_from = averageDens, 
#               values_fill = list(averageDens = 0)) %>% 
#   remove_rownames %>% 
#   column_to_rownames(var="YEarMonth")
# 
# mtet <- make_relative(as.matrix(mte))
# 
# mtet <- as.data.frame(mtet)%>% 
#   rownames_to_column(var = "YEarMonth")
# 
# 
# 
# mse <- mtet %>%
#   pivot_longer(-YEarMonth, names_to = "Sp_abb", values_to = "abun") %>% 
#   group_by(YEarMonth) %>% 
#   arrange(desc(abun)) %>% 
#   filter(abun > 0.01)




