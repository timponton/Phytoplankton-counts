install.packages("tidyverse")
install.packages("anytime")
install.packages("dplyr")
install.packages("reshape")
install.packages("data.table")
install.packages("vegan")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape)
library(data.table)
library(vegan)
library(flexdashboard)




#Read in data and combine
dat <- read.csv("Cleaned count data.csv")
col <- read.csv("Details of collection.csv")
combine <- merge(col, dat, "Sample")

#fix date format 
combine$Date <- as.Date(combine$Date, "%d/%m/%Y")

# remove columns where counts were labelled as 'various'
combine_cond <- subset(combine, Classification != "Various")


# FIlter out counts lower than a specific number
combine_cond <- combine_cond %>% 
  filter(Total.Count > 50000)


ggplot(data = combine_cond, aes(x = Total.Count, fill = Classification)) +
  geom_density()

## subset different seasons and add season label

#Autumn
autumn <- subset(combine, Date> "2019--04-24" & Date < "2019-05-23")
autumn$Season <- 'Autumn'

# Winter
winter <- subset(combine, Date> "2019-07-18" & Date < "2019-08-15")
winter$Season <- 'Winter'

# spring
spring <- subset(combine, Date> "2019-10-10" & Date < "2019-11-06")
spring$Season <- 'Spring'


#summer

# combine seasons
all_seasons <- rbind(autumn, winter, spring)

ggplot(data = all_seasons, aes(x = Species, y = Total.Count)) +
  geom_bar(stat = "Identity",col = "Black") +
  coord_flip() +
  facet_wrap(~Season)


ggplot(data = autumn, aes(x = Date, y = Total.Count, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black")

# summarising by season
seasons_mean <- all_seasons%>% 
  group_by(Season, Species) %>%
  summarise(mean_count = mean(Total.Count, na.rm = TRUE))

ggplot(data = seasons_mean, aes(x = Species, y = mean_count)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black") +
  coord_flip() +
  facet_wrap(~Season)


# cast table wide
seasons_mean_wide <- dcast(seasons_mean, Season ~ Species, value.var = "mean_count")

#replace NA with 0's
seasons_mean_wide[is.na(seasons_mean_wide)] <- 0

# subset grouping coloumn
season_group <- subset(seasons_mean_wide, select = c(1:1))
# Shannon diversity
season_group$shannon <- diversity(seasons_mean_wide[, 2:50], "shannon")
#simpson
season_group$simpson <- diversity(seasons_mean_wide[, 2:50], "simpson")



