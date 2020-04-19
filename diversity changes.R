
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




#Read in data and combine
dat <- read.csv("Cleaned count data.csv")
col <- read.csv("Details of collection.csv")
combine <- merge(col, dat, "Sample")

#fix date format 
combine$Date <- as.Date(combine$Date, "%d/%m/%Y")

# add month label column {month() vs months() function important}
combine <- combine %>%
  mutate(months = months(Date), month = month(Date)) 

# remove unnec rows
combine_clean <- combine %>% 
  filter(Classification != "Various" & Site == "Primary sump")


# summarise Total.Count grouped by
combine_mean <- combine_clean %>% 
  group_by(Species, month, months) %>% 
  summarise(mean_cells = mean(Total.Count, na.rm = TRUE))

# quick plot to check data 
ggplot(data = combine_mean, aes(x = reorder(months, month), y = mean_cells)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  facet_wrap(~Classification, scales = "free_y")

# change df from long to wide, controlling for 

# months and month, and using mean_cells column 
combine_mean_wide <- combine_mean %>% 
  pivot_wider(names_from = Species, values_from = mean_cells)

#replace NA with 0's
combine_mean_wide[is.na(combine_mean_wide)] <- 0


# diversity calcs
combine_group <- subset(combine_mean_wide, select = c(1:2))

combine_group$shannon <- diversity(combine_mean_wide[, 3:92], "shannon")
combine_group$simpson <- diversity(combine_mean_wide[, 3:92], "simpson")


# gather for better plotting

gather <- gather(combine_group, "Index", "Diversity", 3:4)

ggplot(data = gather, aes(x = reorder(months, month), y = Diversity, fill = Index)) + 
  geom_bar(stat = "Identity", position = "Dodge")

# subset classes
# summarise
# long to wide
# diversity
# join
# plot

# Diatom
 diatom <- combine_clean %>% 
   subset(Classification == "Diatom")

diatom_mean <- diatom %>% 
  group_by(Species, month, months) %>% 
  summarise(mean_cells = mean(Total.Count, na.rm = TRUE))


diatom_wide <- diatom_mean %>% 
  pivot_wider(names_from = Species, values_from = mean_cells)

#replace NA with 0's
diatom_wide[is.na(diatom_wide)] <- 0

diatom_group <- subset(diatom_wide, select = c(1:2))
diatom_group$shannon <- diversity(diatom_wide[, 3:37], "shannon")
diatom_group$simpson <- diversity(diatom_wide[, 3:37], "simpson")

# Dino
dino <- combine_clean %>% 
   subset(Classification == "Dinoflagellate")

dino_mean <- dino %>% 
   group_by(Species, month, months) %>% 
   summarise(mean_cells = mean(Total.Count, na.rm = TRUE))
 

dino_wide <- dino_mean %>% 
  pivot_wider(names_from = Species, values_from = mean_cells)

#replace NA with 0's
dino_wide[is.na(dino_wide)] <- 0

dino_group <- subset(dino_wide, select = c(1:2))
dino_group$shannon <- diversity(dino_wide[, 3:53], "shannon")
dino_group$simpson <- diversity(dino_wide[, 3:53], "simpson")




# Dictyocho
dict <- combine_clean %>% 
  subset(Classification == "Dictyochophyceae")

dict_mean <- dict %>% 
  group_by(Species, month, months) %>% 
  summarise(mean_cells = mean(Total.Count, na.rm = TRUE))

dict_wide <- dict_mean %>% 
  pivot_wider(names_from = Species, values_from = mean_cells)

#replace NA with 0's
dict_wide[is.na(dict_wide)] <- 0

dict_group <- subset(dict_wide, select = c(1:2))
dict_group$shannon <- diversity(dict_wide[, 3:4], "shannon")
dict_group$simpson <- diversity(dict_wide[, 3:4], "simpson")

dict_group$class <- "Dictyochophyceae"

# add class column
dict_group$class <- "Dictyochophyceae"
dino_group$class <- "Dinoflagellate"
diatom_group$class <- "Diatom"

# bind all 3 df's together
div <- rbind(dino_group, diatom_group, dict_group)

# wide to long for betetr plotting

div_gather <- gather(div, "Index", "Diversity", 3:4)

# plot data

ggplot(data = div_gather) +
  geom_bar(aes(x = reorder(months, month), 
                 y = Diversity, fill = class), stat = "Identity", position = "Dodge") +
  geom_line(aes(x = month, 
            y = Diversity, col = class), size = 1) +
  facet_wrap(~Index,  scales = "free_y", ncol = 1) +
  xlab("Months in 2019") +
  theme(axis.text.x=element_text(angle=35, hjust=1))

