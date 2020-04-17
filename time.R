
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

red <- combine %>% 
  filter(Date >= as.Date("2019-02-10") & Date <= as.Date("2019-02-25"))

ggplot(data = red, aes(x = reorder(Species, -Total.Count), y = Total.Count)) +
  geom_bar(stat = "Identity", position = "Dodge")

red_mean <- red %>% 
  group_by(Species, Classification) %>% 
  summarise(mean_count = mean(Total.Count)) %>% 
  arrange(desc(mean_count)) %>% 
  head(10)

s <- red %>% 
  filter(Species %in% red_mean$Species) %>% 
  ggplot(., aes(x = Date, y = Total.Count, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  theme(axis.text.x=element_text(angle=35, hjust=1)) +
  facet_wrap(~Site)


ggplotly(s)


k <- red %>% 
  filter(Species %in% red_mean$Species) %>% 
  ggplot(., aes(x = Date, y = Total.Count, col = Species)) +
  geom_point()


ggplotly(k)

mutate(mean_size = mean(c(Low, Up)))

dom_sp <- red %>% 
  filter(Species %in% red_mean$Species)
dom_sp <- merge(dom_sp, info, "Species")

m <- dom_sp %>% 
  group_by(Species) %>% 
  mutate(mean_size = mean(c(Low, Up))) %>% 
  ggplot(., aes(x = Date, y = mean_size, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  theme(axis.text.x=element_text(angle=35, hjust=1)) +
  facet_wrap(~Site)

ggplotly(m)

ggplot(data = t, aes(x = Date, y = Total.Count, size = mean_size)) +
  geom_point() +
  facet_wrap(~Classification)

t <- dom_sp %>% 
  group_by(Species) %>% 
  mutate(mean_size = mean(c(Low, Up)))

# creating size classes
max_meanSizeT <- max(t$mean_size, na.rm = TRUE)
q <- max_meanSizeT/3
q1T <- q*1
q2T <- q*2
q3T <- q*3

tt <- t %>% 
  mutate(size_class = ifelse(mean_size <= q1T, "small", ifelse(mean_size > q2T, "Large", "Medium")))

ggplot(data = tt, aes(x = Date, y = Total.Count, col = size_class)) +
  geom_jitter(na.rm = TRUE) +
  facet_wrap(~Site, scales = "free_y")

ggplot(data = red, aes(x = Date, y = ))
