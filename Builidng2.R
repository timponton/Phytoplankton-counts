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
  summarise(TotalCells = mean(Cells.L, na.rm = T)) %>% 
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
