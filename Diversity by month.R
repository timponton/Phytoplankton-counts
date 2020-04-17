install.packages("reshape2")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(anytime)
library(dplyr)
library(reshape2)
library(data.table)
library(vegan)

## diversity by month

combine <- merge(col, dat, "Sample")

#fix date format 
combine$Date <- as.Date(combine$Date, "%d/%m/%Y")

combine <- combine %>% 
  filter(Classification != "Various")

# add month label column {month() vs months() function important}
combine <- combine %>%
  mutate(months = months(Date), month = month(Date)) 

#remove grouped species
combine <- subset(combine, Species!="HAB Total Count")

# subset just Primary sump
combine_PS <- subset(combine, Site == "Primary sump")

#summarise by month 
month_mean <- combine_PS%>%
  group_by(month, months, Site, Species, Classification) %>% 
  summarise(mean_count = mean(Total.Count, na.rm = TRUE))

# cast wide
month_wide <- reshape2::dcast(month_mean, month + months + Site + Classification ~ Species, value.var = "mean_count")

#replace NA with 0's
month_wide[is.na(month_wide)] <- 0

# month grouping
month_group <- subset(month_wide, select = c(1:4))
month_group$shannon <- diversity(month_wide[, 5:50], "shannon")
month_group$simpson <- diversity(month_wide[, 5:50], "simpson")

ggplot(data = month_group, aes(x = months, y = shannon, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  coord_flip()

# Primary sump data only
primary_div <- subset(month_group, Site == "Primary sump")

ggplot(data = primary_div, aes(x = reorder(months, month), y = shannon)) +
  geom_bar(stat = "Identity", fill = "orange", col = "black")
ggplot(data = primary_div, aes(x = reorder(months, month), y = simpson, fill = Classification)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black")



#Primary sump only
primary <- subset(month_mean, Site == "Primary sump")

ggplot(data = primary, aes(x = Species, y = mean_count)) +
  geom_bar(stat = "Identity", position = "dodge") +
  coord_flip()

feb <- subset(month_group, months == "February")

ggplot(data = feb, aes(x = Site, y = shannon)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  coord_flip()


february <- subset(combine, months == "February")
ggplot(data = february, aes(x = Date, y = Total.Count, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  facet_wrap(~Site) +
  coord_flip()

ggplot(data = february, aes(x = Date, y = Total.Count, col = Species)) +
  geom_line() +
  facet_wrap(~Site)

february_wide <- reshape2::dcast(february, Date + Site ~ Species, value.var = "Total.Count")
february_group <- subset(february_wide, select = c(1:2))
february_group$shannon <- diversity(february_wide[, 3:40], "shannon")
february_group$simpson <- diversity(february_wide[, 3:40], "simpson")

ggplot(data = february_group, aes(x = Site, y = shannon)) +
  geom_bar(stat = "Identity", col = "black", position = "Dodge") +
  facet_wrap(~Date) +
  coord_flip()

#subset just the 12th of Feb
twelve <- subset(february, Date == "2019-02-12")
ggplot(data = twelve, aes(x = Time, y = Total.Count, fill = Species)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  facet_wrap(~Site) +
  coord_flip()

twelve_wide <- dcast(twelve, Site + Time ~ Species, value.var = "Total.Count")
twelve_wide[is.na(twelve_wide)] <- 0
twelve_group <- subset(twelve_wide, select = c(1:2))

twelve_group$shannon <- diversity(twelve_wide[, 3:14], "shannon")
twelve_group$simpson <- diversity(twelve_wide[, 3:14], "simpson")

ggplot(data = twelve_wide, aes(x = Time, y = Polyedrum)) +
  geom_bar(stat = "Identity", position = "Dodge")

ggplot(data = twelve_group, aes(x = Site, y = simpson)) +
  geom_bar(stat = "Identity", position = "Dodge")
