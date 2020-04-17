library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape)
library(data.table)
library(vegan)
library(plotly)
library(flexdashboard)





#Read in data and combine
dat <- read.csv("Cleaned count data.csv")
col <- read.csv("Details of collection.csv")
combine <- merge(col, dat, "Sample")

#fix date format 
combine$Date <- as.Date(combine$Date, "%d/%m/%Y")

# remove columns where counts were labelled as 'various'
combine_cond <- subset(combine, Classification != "Various")

march04 <- subset(combine, Date == "2019-03-04")

ggplot(data = march04, aes(x = Time, y = march04$DO..mg.L., fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black")

ggplot(data = march04, aes(x = Time, y = march04$Temperature...C., fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black")


p <- ggplot(data = march04, aes(x = Time, y = march04$Total.Count, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black") +
  facet_wrap(~Site)

ggplotly(p)



################ Gauge of when Dino's are dominating

# remove various in classification
march04 <- subset(march04, Classification != "Various")


dino <- march04 %>% 
  group_by(Site, Classification, Time) %>% 
  tally(Total.Count) %>% 
  mutate(prop = prop.table(n)*100) %>% 
  filter(Classification == "Dinoflagellate")

# give parameters a grouping, where we set the percentages of colours
dino <- dino %>% 
  mutate(group = ifelse(prop > 50, "red", ifelse(prop >= 25 & prop <= 49, "orange", "green")))

# round off numbers in prop colum
dino$prop <- round(dino$prop, digits = 2)

# subset to site and time
AD_8 <- subset(dino, Site == "After Drumfilter" & Time == "08:00:00", select = prop)

AD_13 <- subset(dino, Site == "After Drumfilter" & Time == "	13:00:00", select = prop)

AD_17 <- subset(dino, Site == "After Drumfilter" & Time == "	17:00:00", select = prop)

PS_8 <- subset(dino, Site == "Primary sump" & Time == "08:00:00", select = prop)

PS_13 <- subset(dino, Site == "Primary sump" & Time == "13:00:00", select = prop)

PS_17 <- subset(dino, Site == "Primary sump" & Time == "17:00:00", select = prop)

# create gauge

gauge(as.numeric(AD_8), min = 0, max = 100, symbol = "%", gaugeSectors(
  success = c(0, 24), warning = c(25, 49), danger = c(50, 100)
))

# amalgamate graphs together 

rate <- round(dino$prop, digits = 2)

  
rate

gauge(rate, min = 0, max = 100, symbol = "%", gaugeSectors(
  success = c(0, 24), warning = c(25, 49), danger = c(50, 100)
))

gauge(rate, min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))



#################################################################

## Most common species 

combine <- combine %>% 
  filter(Classification != "Various")

s <- combine %>% 
  group_by(Site, Species, Classification) %>% 
  filter(Classification != "Various" & Site == "Primary sump") %>% 
  summarise(mean = mean(Total.Count, na.rm = TRUE)) %>% 
  arrange(desc(mean)) %>% 
  head(16) %>% 
  ggplot(., aes(x = reorder(Species, -mean), y = mean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge", color = "black") +
  xlab("Species") +
  ylab("Average cells/liter")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
  
ggplotly(s)

g <- counts %>% 
  group_by(Item) %>% 
  summarise(n = sum(Total.Count.land.and.underwater, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(., aes(x = reorder(Item, -n), y = n)) +
  geom_bar(stat = "Identity", position = "Dodge", fill = "blue") +
  xlab("Items collected") +
  ylab("Item")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggplotly(g)

#####################

## Different locations

# Subset days where sampling all over the farm took place
con <- combine %>% 
  filter(Date >= as.Date("2019-02-10") & Date <= as.Date("2019-03-30"))


# summarise by group by time date lcoation species 

# Eyeball which are the most common species over the time period 
f <- con %>% 
  filter(Site == "Primary sump") %>% 
  arrange(desc(Total.Count)) %>% 
  ggplot(., aes(x = Date, y = Total.Count, col = Species)) +
  geom_line()

ggplotly(f)

d <- con %>%
  arrange(desc(-Total.Count)) %>%
  group_by(Date) %>%
  top_n(n = 10, wt = Species)


a <- ggplot(data = d, aes(x = Date, y = Total.Count, col = Species)) +
  geom_line()

ggplotly(a)


# summarise times in the day 

time <- con %>% 
  group_by(Date, Species, Site, Classification) %>% 
  summarise(mean = mean(Total.Count))


t <- time %>% 
  filter(Site == "Primary sump") %>% 
  arrange(desc(mean)) %>% 
  group_by(Date) %>% 
  top_n(10, wt = Species)

at <- ggplot(data = t, aes(x = Date, y = mean, col = Species)) +
  geom_line()

ggplotly(at)

###############

# Find the nth most common species
k <- con %>% 
  filter(Site == "Primary sump") %>% 
  group_by(Species) %>% 
  summarise(mean = mean(Total.Count)) %>% 
  arrange(desc(mean)) %>% 
  head(4)
  
# Filter the original data frame to only show the most common species we just calculated
filt <- con %>% 
  filter(Species %in% k$Species)

# summarise times per date and create plot
ak <- filt %>% 
  filter(Site == c("Primary sump")) %>% 
  group_by(Date, Site, Species, Classification) %>% 
  summarise(mean = mean(Total.Count)) %>% 
  arrange(desc(mean)) %>% 
  ggplot(., aes(x = Date, y = mean, col = Species)) +
  geom_line() +
  facet_wrap(~ reorder(Species, -mean))

# ggplot it
ggplotly(ak)



