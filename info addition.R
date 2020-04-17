library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape)
library(data.table)
library(plotly)
library(patchwork)

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

infoCombine <- merge(combine, info, "Species")

infoCombine <- infoCombine %>% 
  filter(Classification != "Various")

infoCombine_dino <- infoCombine %>% 
  filter(Classification == "Dinoflagellate")




call <- infoCombine %>% 
group_by(Site, Species, Classification) %>% 
  filter(Classification != "Various" & Site == "Primary sump") %>% 
  summarise(mean = mean(Total.Count, na.rm = TRUE)) %>% 
  arrange(desc(mean)) %>% 
  head(16)



call2 <- merge(call, info, "Species")


call2 <- call2 %>% 
  group_by(Species) %>% 
  mutate(mean_size = mean(c(Low, Up)))

# creating size classes
max_meanSize <- max(call2$mean_size)
q <- max_meanSize/3
q1 <- q*1
q2 <- q*2
q3 <- q*3

call3 <- call2 %>% 
  mutate(size_class = ifelse(mean_size <= q1, "small", ifelse(mean_size > q2, "Large", "Medium")))


ggplot(aes(x = Species, y = mean_size)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


big <- ggplot(data = call2, aes(x = reorder(Species, -mean_size), y = mean_size, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

small <- ggplot(data = call2, aes(x = reorder(Species, mean_size), y = mean_size, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

small/big

(small + big + small)/ big

ggplotly(small)



cat <- infoCombine %>% 
  group_by(Site, Species, Classification) %>% 
  filter(Classification != "Various" & Site == "Primary sump") %>% 
  summarise(mean = mean(Total.Count, na.rm = TRUE)) %>% 
  arrange(desc(mean))

det <- call3 %>% 
  select(Species, mean_size, size_class)

cat <- merge(cat, det, "Species")


ggplot(data = cat, aes(x = mean_size, y = mean, col = Classification)) +
  geom_point()

ggplot(data = infoCombine, aes(x = Date, y = Total.Count, col = Classification)) +
  geom_point()


