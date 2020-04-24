# Understanding feasibility of filter project


df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>%
  group_by(Site, Order, Species, Season, SeasonOrder) %>% 
  summarise(TotalCells = mean(Cells.L, na.rm = T)) %>% 
  ungroup() 



  ggplot(., aes(x = reorder(Season, SeasonOrder), y = TotalCells, fill = reorder(Site, Order))) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Season") +
  ylab(" Total Cells/L") +
  ggtitle("Comparison of Cells/L counted between the 
          Primary sump and After Drumfilter during the summer and autumn of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(test)




#### diversity





mat <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>%
  group_by(Site, Order, Species, Season, SeasonOrder) %>% 
  summarise(TotalCells = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = TotalCells, 
            values_fill = list(TotalCells = 0))

group <- mat %>%
  select(1:4)

group <- group %>% 
  mutate(shannon = diversity(mat[, 5:ncol(mat)], "shannon"), 
         Adjsimpson = 1-(diversity(mat[, 5:ncol(mat)], "simpson")), 
         simpson = diversity(mat[, 5:ncol(mat)], "simpson"))

ggplot(data = group, aes(x = reorder(Season, SeasonOrder), y = shannon, fill = reorder(Site, Order))) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  xlab("Season") +
  ylab("Shannon Diversity") +
  ggtitle("Shannon diversity index between the 
          Primary sump and After Drumfilter during the summer and autumn of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
##### Richness

df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Site, Order, Season, SeasonOrder) %>% 
  summarise(NumberSpecies = n_distinct(Species)) %>% 
  ggplot(., aes(x = reorder(Season, SeasonOrder), y = NumberSpecies, fill = reorder(Site, Order))) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  xlab("Season") +
  ylab("Species Richness") +
  ggtitle("Comparison between the number of species between the 
          Primary sump and After Drumfilter during the summer and autumn of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#####

df %>% 
  group_by(Date) %>% 
  filter(Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(TotalCells = sum(Cells.L)) %>% 
  ungroup() %>% 
  group_by(Site) %>% 
  tally()

##### check if ADf is higher than PS in summer 


df %>% 
  filter(Season == "Summer") %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Order, Species, Season, SeasonOrder) %>% 
  summarise(TotalCells = mean(Cells.L, na.rm = TRUE)) %>%
  ggplot(., aes(x = Date, y = TotalCells, fill = reorder(Site, Order))) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black")

ggplot(., aes(x = Date, y = TotalCells, col = Species)) +
  geom_line() +
  facet_wrap(~Site, ncol = 1)


###### comapring times 


df %>% 
  filter(Date == "2019-03-19") %>% 
  mutate(LagDateTime = if_else(Site == "After Drumfilter", dateTime + minutes(0), dateTime)) %>% 
  ggplot(., aes(x = LagDateTime, y = Cells.L, col = reorder(Site, Order))) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Time") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L counted throughout the day from Primary and Secondary ('After Drumfilter') 
          sump on the 19/03/2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  
df %>% 
  filter(Date == "2019-03-19") %>% 
  ggplot(., aes(x = Time, y = Cells.L, fill = reorder(Site, Order))) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Time") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L counted throughout the day from Primary and Secondary ('After Drumfilter') 
          sump on the 19/03/2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

  
  
  
### try time warp distance 
install.packages("dtw")
library(dtw)

tstry <- df %>% 
  filter(Date == "2019-03-19") %>% 
  mutate(roundedTime = round_date(dateTime, "30 min")) %>% 
  group_by(roundedTime, Site) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanCells, 
              values_fill = list(meanCells = 0))

ggplot(data = tstry, aes(x = `Primary sump`, y = `After Drumfilter`)) +
  geom_point() +
  geom_smooth(method = "lm")

roundTime <- round_date(df$dateTime, "2 hour")



#### species richness for 4 specific dates


com <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  filter(Date >= as.Date("2019-02-10") & Date <= as.Date("2019-04-20")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Order, Proper.Site, month, months, year, Season, SeasonOrder) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

ggplot(data = com, aes(x = Date, y = meanCells, col = reorder(Site, Order))) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(trans = "log10") +
  theme_classic()




  
df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  filter(Date >= as.Date("2019-02-10") & Date <= as.Date("2019-04-20")) %>% 
  ungroup() %>% 
  group_by(Date, Site, month, months, year, Season, SeasonOrder) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanCells, 
              values_fill = list(meanCells = 0)) %>%
  mutate(ADfBig = ifelse(`After Drumfilter` > `Primary sump`, "Yes", 
                         ifelse(`After Drumfilter` < `Primary sump`, "No", "Same"))) %>% 
  ggplot(., aes(x = `Primary sump`, y = `After Drumfilter`, col = reorder(months, month))) +
  geom_point(size = 2) +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  facet_wrap(~ADfBig, ncol = 1) +
  xlab("Cells/L in Primary sump (log10)") +
  ylab("Cells/L After Drumfilter (log10)") +
  ggtitle("Indication for days when After Drumfilter Cells/L were 
  more (Yes) or less (No) than Primary sump, 
          highlighted by the month") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  
  
  
  
  
  ggplot(., aes(x = `Primary sump`, y = `After Drumfilter`, col = reorder(months, month))) +
  geom_point(size = 2) +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  geom_smooth(method = "lm", se = FALSE)
  
  
### position on farm
  
pos <- df %>% 
  filter(Date == "2019-02-26") %>% 
  filter(Site != "Unknown") %>% 
  filter(CodeCount >= "3522" & CodeCount <= "3550") %>% 
  group_by(Location, Order) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = T))
  
pos$Location <- factor(pos$Location, levels = c("Incoming seawater", "Seaview farm", "Hatchery farm", "Bergsig farm"))

ggplot(pos, aes(x = Location, y = meanCells)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  xlab("Location") +
  ylab("Total Cells/L") +
  ggtitle("Cells/L measured on each subfarm on the 
          26/02/2019 between 16:20 and 20:00, with farms 
          ordered in flow position") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  

df %>% 
    filter(Date == "2019-02-26") %>%
    filter(Site != "Unknown") %>% 
    group_by(Location) %>% 
    summarise(meanCells = mean(Cells.L))

ggplot(pos, aes(x = Location, y = meanCells)) +
    geom_bar(stat = "Identity", position = "Dodge") +
    scale_x_discrete(guide = guide_axis(n.dodge = 3))

  
### dino vs diato
SpeciesClass <- df %>% 
  distinct(Species, Classification) 
  
dateInfo <- df %>% 
  distinct(Date, Season, months, month, SeasonOrder)

di <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  left_join(SpeciesClass, by = "Species") %>% 
  left_join(dateInfo, by = "Date") %>% 
  mutate(ADfBig = ifelse(`After Drumfilter` > `Primary sump`, "Yes", 
                         ifelse(`After Drumfilter` < `Primary sump`, "No", "Same"))) %>% 
  filter(`After Drumfilter` !=0, `Primary sump` != 0)


ggplot(data = di, aes(x = `Primary sump`, y = `After Drumfilter`, col= Classification)) +
  geom_point() +
  geom_abline(col = "red", size = 1.5) +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  facet_wrap(~reorder(months, month), ncol = 1)


phy <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  left_join(SpeciesClass, by = "Species") %>% 
  left_join(dateInfo, by = "Date") %>% 
  mutate(ADfBig = ifelse(`After Drumfilter` > `Primary sump`, "Yes", 
                         ifelse(`After Drumfilter` < `Primary sump`, "No", "Same"))) %>% 
  filter(`After Drumfilter` !=0, `Primary sump` != 0) %>% 
  group_by(Classification, months, month, Season, SeasonOrder, ADfBig) %>% 
  pivot_longer(-c(Date, Species, Classification, months, month, Season, SeasonOrder, ADfBig), names_to = "Site", values_to = "meanC")


phy$Site <- factor(phy$Site, levels = c("Primary sump", "After Drumfilter"))
pos$Location <- factor(pos$Location, levels = c("Incoming seawater", "Seaview farm", "Hatchery farm", "Bergsig farm"))

### both yes and no comparisons for Dino's
phy %>% 
  filter(Classification == "Dinoflagellate") %>% 
  ggplot(., aes(x = reorder(Species, meanC), y = meanC, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  facet_wrap(reorder(Season, SeasonOrder) ~ ADfBig) +
  coord_flip()

phy %>% 
  filter(Classification == "Dinoflagellate", ADfBig == "Yes") %>% 
  ggplot(., aes(x = reorder(Species, meanC), y = meanC, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  facet_wrap(~reorder(months, month), scales = "free_x") +
  coord_flip() +
  xlab("Species") +
  ylab("Total Cells/L") +
  ggtitle("Comparisons of months where dinoflagellate species measure more 
          after Drumfilters than Primary sump") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


phy %>% 
  filter(Species != "Diatom", Classification == "Diatom", ADfBig == "Yes") %>% 
  ggplot(., aes(x = reorder(Species, meanC), y = meanC, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  facet_wrap(~reorder(months, month), scales = "free_x") +
  coord_flip() +
  xlab("Species") +
  ylab("Total Cells/L") +
  ggtitle("Comparisons of months where diatom species measure more 
          after Drumfilters than Primary sump") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))




tb <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site, Species) %>% 
  summarise(meanC = mean(Cells.L, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  left_join(SpeciesClass, by = "Species") %>% 
  left_join(dateInfo, by = "Date") %>% 
  mutate(ADfBig = ifelse(`After Drumfilter` > `Primary sump`, "Yes", 
                         ifelse(`After Drumfilter` < `Primary sump`, "No", "Same"))) %>% 
  filter(`After Drumfilter` !=0, `Primary sump` != 0) %>% 
  group_by(Classification, months, month, Season, SeasonOrder, ADfBig) %>% 
  tally()

ggplot(data = tb, aes(x = Classification, y = n, fill = ADfBig)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "Black") +
  facet_wrap(~reorder(months, month), ncol = 1)




group_by(Date, Species) %>% 
  filter(`After Drumfilter` > `Primary sump`, `Primary sump` != 0) %>% 
  mutate(difference = `After Drumfilter` - `Primary sump`) %>% 
  group_by(Species) %>% 
  summarise(totalMean = mean(difference)) %>%
  ungroup() %>% 
  left_join(select(df, Species, Classification), by = "Species") %>% 
  ggplot(., aes(x = reorder(Species, totalMean), y = totalMean, col = Classification)) +
  geom_point(size = 3) +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                    scientific = FALSE))  +
  coord_flip() +
  xlab("Species") +
  ylab("Total Cells/L") +
  ggtitle("Log 10 transformed difference in number of cells/L 
          found in Secondary sump compared to Primary sump for specific species") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  
  
ggplot(data = di, aes(x = `Primary sump`, y = `After Drumfilter`, col = Species)) +
  geom_point() +
  scale_x_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE)) +
  scale_y_continuous(trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE))
