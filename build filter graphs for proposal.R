# Graph building

df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(TotalCells = sum(Cells.L)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = Date, y = TotalCells, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Date") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L counted on days when both the Primary sump and 
          Secondary sump (Here denoted as 'After Drumfilter') were sampled, 
          with individual species counts occuring") +
  theme_classic()

Sys.Date()

df %>% 
  filter(Date == "2019-03-19") %>% 
  ggplot(., aes(x = Time, y = Cells.L, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Time") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L counted throughout the day from Primary and Secondary ('After Drumfilter') 
          sump on the 19/03/2019") +
  theme_classic()

####
# round off time to certain amount
df$roundTime <- round_date(df$dateTime, "1 hour")

ggplotly(df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>%
  mutate(timeDiff = (dateTime - roundTime)/60) %>% 
  ungroup() %>% 
  group_by(roundTime, Site) %>% 
  summarise(totalCells = sum(Cells.L)) %>%
  filter(roundTime >= as.Date("2019-02-19") & roundTime <= as.Date("2019-02-20")) %>% 
  ggplot(., aes(x = roundTime, y = totalCells, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                               scientific = FALSE)) +
  xlab("Date and Time") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L counted throughout the day from Primary and Secondary ('After Drumfilter') 
          sump on the 19/03/2019") +
  theme_classic()


####
df %>% 
  filter(Date == "2019-02-26") %>% 
  group_by(Site, roundTime) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = roundTime, y = meanCells, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge", col = "black") +
    scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Time") +
  ylab("Total Cells/L") +
  ggtitle("Total Cells/L counted at different sites on the 26/02/2019") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

class(d$roundTime)

manySites <- df %>% 
  filter(Date == "2019-02-12") %>% 
  group_by(Site) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = reorder(Site, -meanCells), y = meanCells)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Site") +
  ylab(" Total Cells/L") +
  ggtitle("Total Cells/L at various positions on the farm on the 12/02/2019, ordered by total cells/L") +
  theme_classic()

df %>% 
  filter(Date == "2019-02-26") %>% 
  group_by(Site) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  ggplot(., aes(x = reorder(Site, -meanCells), y = meanCells)) +
  geom_bar(stat = "Identity", position = "Dodge") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3))

####

percentage <- wide_TwoSite %>% 
  mutate(PercentDrumFilter = (`After Drumfilter`/`Primary sump`)) %>% 
  mutate(MoreDrum = ifelse(`After Drumfilter` > `Primary sump`, "Greater", "Less"))

ggplot(data = percentage, aes(x = Date, y = PercentDrumFilter)) +
  geom_point() +
  geom_label( 
    data = percentage %>% filter(PercentDrumFilter > 1), # Filter data first
    aes(label=Date)
  ) +
  geom_line() +
  geom_hline(yintercept = 1, linetype="dashed", color = "red") +
  xlab("Date") +
  ylab("Proportion Cells/L") +
  ggtitle("Proportion Cells/L in Secondary sump when compared to Primary sump") +
  theme_classic()
####

df %>% 
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
  group_by(Date, Species) %>% 
  filter(`After Drumfilter` > `Primary sump`, `Primary sump` != 0) %>% 
  mutate(difference = `After Drumfilter` - `Primary sump`) %>% 
  group_by(Species) %>% 
  summarise(totalMean = mean(difference)) %>% 
  ggplot(., aes(x = reorder(Species, totalMean), y = totalMean)) +
  geom_point() +
  coord_flip()



df %>% 
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
  group_by(Date, Species) %>% 
  filter(`After Drumfilter` > `Primary sump`, `Primary sump` != 0) %>% 
  mutate(difference = `After Drumfilter` - `Primary sump`) %>% 
  group_by(Species) %>% 
  summarise(totalMean = mean(difference)) %>%
  ungroup() %>% 
  left_join(select(df, Species, Classification), by = "Species") %>% 
  ggplot(., aes(x = reorder(Species, totalMean), y = totalMean, col = Classification)) +
  geom_point(size = 3) +
  scale_y_continuous(, trans = "log10", labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE))  +
  coord_flip() +
  xlab("Species") +
  ylab("Total Cells/L") +
  ggtitle("Log 10 transformed difference in number of cells/L 
          found in Secondary sump compared to Primary sump for specific species") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
# Center align title
  
  
cat <- df %>% 
  select(Species, Classification)
