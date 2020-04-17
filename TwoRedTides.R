
library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape)
library(data.table)
library(plotly)


## info added

info <- read.csv("info.csv")

#Read in data and combine
dat <- read.csv("Cleaned count data.csv")
col <- read.csv("Details of collection.csv")
combine <- merge(col, dat, "Sample")

#fix date format 
combine$Date <- as.Date(combine$Date, "%d/%m/%Y")

# add month label column {month() vs months() function important}
combine <- combine %>%
  mutate(months = months(Date), month = month(Date))

# subset red tide

twoRed <- combine %>% 
  filter(Date >= as.Date("2019-02-08") & Date <= as.Date("2019-02-15"))

ggplot(data = twoRed, aes(x = Date, y = Total.Count, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")


red_PA <- combine %>% 
  filter(Date >= as.Date("2019-02-11") & Date <= as.Date("2019-03-04"))

ggplot(data = red_PA, aes(x = Date, y = Total.Count, col = Site)) +
  geom_line()




noPS <- combine %>% 
  filter(Site != "Primary sump" & Site != "After Drumfilter")

z <- ggplot(data = noPS, aes(x = Date, y = Date, col = Site)) +
  geom_jitter()


ggplotly(z)

com <- ggplot(data = combine, aes(x = Date, y = Date, col = Site)) +
  geom_jitter()

ggplotly(com)


PSAD <- combine %>% 
  subset(Site == "Primary sump" | Site == "After Drumfilter")

j <- ggplot(data = PSAD, aes(x = Date, y = Total.Count, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")
ggplotly(j)

cat <- ggplot(data = PSAD, aes(x = Date, y = Date, col = Site)) +
  geom_jitter()

ggplotly(cat)


red_PA <- PSAD %>% 
  filter(Date >= as.Date("2019-02-11") & Date <= as.Date("2019-03-04")) %>% 
  group_by(Date, Site) %>% 
  summarise(mean_counting = mean(Total.Count)) %>% 
  ggplot(., aes(x = Date, y = mean_counting, col = Site)) +
  geom_line() +
  geom_point()

ggplotly(red_PA)

ggplot(data = red_PA, aes(x = Date, y = Total.Count, col = Site)) +
  geom_line()



