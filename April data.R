##### libraries ####
library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape2)
library(data.table)
library(vegan)
library(tidyr)
library(plotly)
library(patchwork)

#### data read in and dates fixed, times merged ####

df <- read.csv("Updated data from April 2020/Combo-as-of-April-2020-OpenRefineCopy.csv")

# fix date
df$Date <- as.Date(df$Date, "%d/%m/%Y")

# merge date and times and coerce to proper format 
df$dateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%Y-%m-%d %H:%M")


##### filter out various ####
# filter out 'various' records, where no species are recorded
noVarious_df <- df %>% 
  filter(Classification != "Various")

#### Testing sites and filters ####
# filter days when sites more than PRimary were sampled (Only records from 2019, when that sampling style began)
MoreSite <- noVarious_df %>% 
  group_by(Date) %>% 
  filter("After Drumfilter" %in% Site)

averMoreSite <- MoreSite %>% 
  group_by(Date, Site, Species, Classification) %>% 
  summarise(average_cells.L = mean(Cells.L))

ggplot(data = averMoreSite, aes(x = Date, y = average_cells.L, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge")

## summarise to total cells/ day and include 'various' too, then compare different sites
# original df, summarise to daily total (i.e deal with two samples per day)
df_daily <- df %>% 
  group_by(Date, Site, Species, Classification) %>% 
  summarise(dailyCells = mean(Cells.L))

# Filter out where after drumfilter occurs to get days with >1 count site
df_daily_moreSite <- df_daily %>% 
  group_by(Date) %>% 
  filter("After Drumfilter" %in% Site)

# sum all cells
sum_df_daily_moreSite <- df_daily_moreSite %>% 
  group_by(Date, Site) %>% 
  summarise(TotalCells = sum(dailyCells))

ggplot(data = sum_df_daily_moreSite, aes(x = Date, y = TotalCells)) +
  geom_line()

# number of different sampling sites on a specific date
numberSites <- sum_df_daily_moreSite %>% 
  group_by(Date) %>% 
  summarise(n = n_distinct(Site))

ggplot(data = numberSites, aes(x = Date, y = n)) +
  geom_bar(stat = "Identity")

ggplotly(df %>% 
  group_by(Date, Site) %>% 
  summarise(meanDailyCells = mean(Cells.L)) %>% 
  ungroup() %>%
  group_by(Date) %>% 
  summarise(n = n_distinct(Site)) %>% 
    filter(n >=2, Date != "2019-12-17") %>% 
  ggplot(., aes(x = Date, y = n)) +
  geom_bar(stat = "Identity"))

ggplotly(df %>% 
  filter(Date >= as.Date("2019-02-24") & Date <= as.Date("2019-03-01")) %>% 
  group_by(Date, Site) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = Date, y = meanCells, col = Site)) +
  geom_line())

##### Filter where just two sites are present #####
# subset dates where just 2 sites are present
TwoSiteClassify <- numberSites %>% 
  filter(n == 2)
# use dates as classifier to subset (And exclude December record)
TwoSite <- sum_df_daily_moreSite %>% 
  filter(Date %in% TwoSiteClassify$Date) %>% 
  filter(Date != "2019-12-17")

ggplot(data = TwoSite, aes(x = Date, y = TotalCells, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")

# pivot sites wide to find percentages
wide_TwoSite <- TwoSite %>% 
  pivot_wider(names_from = Site, values_from = TotalCells)
 #calculate percentage of afterdrum to primary counts
percentage <- wide_TwoSite %>% 
  mutate(PercentDrumFilter = (`After Drumfilter`/`Primary sump`)*100) %>% 
  mutate(MoreDrum = ifelse(`After Drumfilter` > `Primary sump`, "Greater", "Less"))

ggplotly(ggplot(data = percentage, aes(x = Date, y = PercentDrumFilter)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 100, linetype="dashed", color = "red"))

# subset if percentage is over 100%
# selected dates
OverHund <- df %>% 
  filter(Date == "2019-03-11" | Date == "2019-03-20")
ggplot(data = OverHund, aes(x = Time, y = Cells.L, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~Date, ncol = 1, scales = "free_y")

# all dates 

alloverHund <- percentage %>% 
  filter(PercentDrumFilter >= 100)
# all dates where % counted is higher in drumfilter than primary
df %>% 
  filter(Date %in% alloverHund$Date) %>% 
  ggplot(., aes(x = Time, y = Cells.L, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~Date, scales = "free")

# most samples in one day are taken 
df %>% 
  filter(Date == "2019-03-19") %>% 
  ggplot(., aes(x = Time, y = Cells.L, col = Site)) +
  geom_point()


df %>% 
  filter(Date == "2019-03-19") %>% 
  ggplot(., aes(x = Time, y = Cells.L, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")



##### Investigate which species are getting through during high percentages between primary sump and drumfilter #####
# filter original (daily averaged) data from dates where both filter sites are present 

all_twosites <- df_daily %>% 
  filter(Date %in% TwoSite$Date) %>% 
  filter( Classification != "Various")
b <- ggplot(data = all_twosites, aes(x = Date, y = dailyCells, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge")

sp <- ggplot(data = all_twosites, aes(x = Date, y = dailyCells, col = Species)) +
  geom_line() +
  facet_wrap(~Site, ncol = 1)

p/sp

ggplotly(p)
ggplotly(sp)


##### The most sites #####


mostSiteClassify <- numberSites %>% 
  filter(n > 10)

mostSites <- df_daily %>% 
  filter(Date %in% mostSiteClassify$Date) %>% 
  group_by(Date, Site) %>% 
  summarise(totalCells = sum(dailyCells))

mostSites %>% 
    filter(Date == "2019-02-26") %>% 
    ggplot(., aes(x = reorder(Site, -totalCells), y = totalCells)) + 
    geom_bar(stat = "Identity") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))

df %>% 
  filter(Date == "2019-02-12") %>% 
  group_by(Site) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = reorder(Site, -meanCells), y = meanCells)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
  

#### list of all sites sampled, to be ordered by position ####

sites <- df %>%
  group_by(Site) %>% 
  tally() %>% 
  arrange(desc(n))
write.csv(sites, file = "Sample sites - Abagold.csv")

# top sites where n is met
topSites <- df %>%
  group_by(Site) %>% 
  tally() %>% 
  filter(n >= 20)

# filter from a group only when all conditions are present and met 
only <- df %>% 
  group_by(Date) %>% 
  filter(all(topSites$Site %in% Site)) %>%
  filter(Site %in% topSites$Site) 

ggplotly(ggplot(only, aes(x = dateTime, y = Cells.L, col = Site)) +
  geom_line())



ggplot(data = numberSites, aes(x = Date, y = n)) +
  geom_bar(stat = "Identity", position = "Dodge")

##### highest peak in March 2019 #####

big <- df %>% 
  filter(Date >= as.Date("2019-03-18") & Date <= as.Date("2019-03-27")) %>% 
  filter(Site %in% c("Primary sump", "After Drumfilter")) %>% 
  group_by(dateTime, Site) %>% 
  summarise(totalCells = sum(Cells.L))


ggplotly(ggplot(data = big, aes(x = dateTime, y = totalCells, col = Site)) +
  geom_line())
  
ggplotly(df %>% 
  filter(Date >= as.Date("2019-03-18") & Date <= as.Date("2019-03-27")) %>% 
  filter(Site %in% c("Primary sump", "After Drumfilter")) %>% 
  ggplot(., aes(x = as.factor(Date), y = Cells.L, fill = Site)) +
  geom_boxplot(position = "Dodge"))






