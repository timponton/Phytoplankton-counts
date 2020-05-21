# run  lines from 'Building Chp2' until before subsetting PS only 

## load data
# size classes of dominant species 
sp_sizes <- read.csv("Updated data from April 2020/Dominant sizes.csv", stringsAsFactors = FALSE)

# subset dates for when both Ps and ADF were sampled ----------------------

ps_adf <- df %>% 
  group_by(Date) %>% 
  filter(Classification != "Various", Date != "2019-12-17", Date != "2019-02-17") %>% 
  filter(all(c("After Drumfilter", "Primary sump") %in% Site)) %>% 
  filter(Site %in% c("After Drumfilter", "Primary sump")) %>% 
  filter(Species != "Diatom")


# correct the order of sites (PS before ADf)
ps_adf$Site <- as.factor(ps_adf$Site)
ps_adf$Site <-  factor(ps_adf$Site, 
                       levels = c("Primary sump", "After Drumfilter"))


# correct levels of FWC_mod classes
ps_adf$FWC_mod <- as.factor(ps_adf$FWC_mod)
ps_adf$FWC_mod <-  factor(ps_adf$FWC_mod, 
                   levels = c("High","Medium", "Low", "Very Low", "Normal"))



# Daily metrics of different sites ----------------------------------------

## calculate biomass evennes richness diversity per day/ week for each site
# average counts for each species per day
Sp.only_filter <- ps_adf %>% 
  group_by(Date, Site, Species, Classification, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

# total biomass per day (all species added together per day)
total_sp_filter <- Sp.only_filter %>% 
  group_by(Date, Site, months, month, year, YEarMonth, Season, FWC_mod) %>% 
  summarise(total_biomass = sum(meanCells)) %>% 
  ungroup()

# richness
richness_filter <- Sp.only_filter %>% 
  group_by(Date, Site) %>% 
  summarise(S = n_distinct(Species))

ggplot(richness_filter, aes(x = Date, y = S, col = Site)) +
  geom_line()

# diversity
# pivot species wider
div_filter <- Sp.only_filter %>% 
  ungroup() %>% 
  select(Date, Site, Species, meanCells) %>% 
  pivot_wider(names_from = Species, values_from = meanCells, 
              values_fill = list(meanCells = 0))

# create group
group_filter <- div_filter %>% 
  select(Date, Site)

# diversity for each day, and site
group_filter <- group_filter %>% 
  mutate(shannon2 = diversity(div_filter[, 3:ncol(div_filter)], "shannon", base = 2),
       shannon = diversity(div_filter[, 3:ncol(div_filter)], "shannon"),
       simpson = diversity(div_filter[, 3:ncol(div_filter)], "simpson"), 
       Inv_simpson = diversity(div_filter[, 3:ncol(div_filter)], "inv"))


# Join all metrics

dailydesc_filter <- left_join(total_sp_filter, group_filter, by = c("Date", "Site")) %>% 
  left_join(., richness_filter, by = c("Date", "Site"))


# calculate evenness
dailydesc_filter <- dailydesc_filter %>% 
  mutate(J2 = shannon2/log(S), 
       J = shannon/log(S))



## summarise down to monthly measurements to compare sites [boxplot graph]

#'* Graphs for Aim 1a -------------*
# biomass
ggplot(data = dailydesc_filter, aes(x = reorder(months, month), y = total_biomass, fill = Site)) +
  geom_boxplot() +
  xlab("Months") +
  ylab("Cells/L") + 
  labs(fill = "Site") +
  ggtitle("Compare the difference in biomass of phytoplankton between the two sites, for three consecutive months") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# richness
ggplot(data = dailydesc_filter, aes(x = reorder(months, month), y = S, fill = Site)) +
  geom_boxplot() +
  xlab("Months") +
  ylab("Number of species") + 
  labs(fill = "Site") +
  ggtitle("Compare the difference in species richness of phytoplankton between 
          the two sites, for three consecutive months of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# evenness
ggplot(data = dailydesc_filter, aes(x = reorder(months, month), y = J, fill = Site)) +
  geom_boxplot() +
  xlab("Months") +
  ylab("Species Evenness") + 
  labs(fill = "Site") +
  ggtitle("Compare the difference in species evenness (J') of phytoplankton between 
          the two sites, for three consecutive months of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# diversity
ggplot(data = dailydesc_filter, aes(x = reorder(months, month), y = shannon, fill = Site)) +
  geom_boxplot() +
  xlab("Months") +
  ylab("Shannon Index of Diversity") + 
  labs(fill = "Site") +
  ggtitle("Compare the difference in Shannon index of diversity between phytoplankton species between 
          the two sites, for three consecutive months of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))





# Comparing biomass at different points on farm  --------------------------------------------------------

# subset specific day when many sites had been sampled (2019-02-26)
## Filter unknown sites

# codecounts are filtered for all sampels after 12pm (afternoon)
# mutated time period to indicae when sampels were taken 
pos <- df %>% 
  filter(Date == "2019-02-26") %>% 
  filter(Site != "Unknown") %>% 
  # filter(CodeCount >= "3518" & CodeCount <= "3550") %>% 
  mutate(`Day Time period` = ifelse(Time %in% c("01:00", "03:00", "05:00", 
                                    "06:00", "07:45", "09:30", "10:15", "10:45"), 
                                    "Morning", "Afternoon"))
  
# Fix order of time period samples
pos$`Day Time period` <- as.factor(pos$`Day Time period`)
pos$`Day Time period` <-factor(pos$`Day Time period`, levels = c("Morning", "Afternoon"))

# fix order of Locations
pos$Location <- factor(pos$Location, levels = c("Incoming seawater", "Seaview farm", "Hatchery farm", "Bergsig farm"))

#'* Graphs for Aim 1b --------------*
ggplot(pos, aes(x = Location, y = log10(Cells.L))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = `Day Time period`), size = 2) +
  xlab("Location") +
  ylab(" Log10 Total Cells/L") +
  labs(col = "Period of Day") +
  ggtitle("Cells/L measured on each subfarm on the 
          26/02/2019, with farms 
          ordered in flow position, and points indicating relative sampling period") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




# Ind species fluctuations ------------------------------------------------

# daily means
# pivot sites wider
# calculate Ps minus ADf
sp_site_diff_daily <- Sp.only_filter %>% 
  pivot_wider(names_from = Site, values_from = meanCells, 
              values_fill = list(meanCells = 0)) %>% 
  mutate(site.Diff = `Primary sump` - `After Drumfilter`)

# filter top n species
sp_site_diff_daily <- sp_site_diff_daily %>% 
  filter(Species %in% top_species$Species)


# Average species to monthly means
# pivot sites wider
# calculate Ps minus ADf
sp_site_diff_monthly <- Sp.only_filter %>% 
  group_by(Site, months, month, Species, Classification) %>% 
  summarise(monthly.mean = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = monthly.mean, 
              values_fill = list(monthly.mean = 0)) %>% 
  mutate(site.Diff = `Primary sump` - `After Drumfilter`)

# pick top n species 
top_species <- Sp.only_filter %>% 
  group_by(Species, Classification) %>% 
  summarise(Sp_average = mean(meanCells, na.rm = TRUE)) %>% 
  arrange(desc(Sp_average)) %>% 
  ungroup() %>% 
  top_n(3) %>% 
  ungroup()

# calculate mean size class
sp_sizes <- sp_sizes %>% 
  group_by(Species) %>% 
  mutate(mean_length = mean(min_length:max_length, na.rm = TRUE))

top_species <- top_species %>% 
  left_join(sp_sizes, by = "Species")


site_size <- Sp.only_filter %>% 
  group_by(Site, months, month, Species, Classification) %>% 
  #summarise(monthly.mean = mean(meanCells, na.rm = TRUE)) %>% 
  filter(Species %in% top_species$Species) %>% 
  left_join(top_species, by = "Species") %>% 
  ungroup()
  


#'* Graph for Aim 2 -----------*
# [Stacked bargraph] of Top n species, comparing monthly averages between two sites
Sp.only_filter %>% 
  group_by(Site, months, month, Species, Classification) %>% 
  summarise(monthly.mean = mean(meanCells, na.rm = TRUE)) %>% 
  filter(Species %in% top_species$Species) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthly.mean, fill = Site)) +
  geom_bar(stat = "Identity", position = "fill") +
  geom_hline(yintercept = 0.5, col = "black") +
  #coord_flip() +
  facet_wrap(~ reorder(Species, -monthly.mean), ncol = 2) +
  xlab("Months") +
  ylab("Proportion momthly mean Cells/L") + 
  labs(fill = "Site") +
  ggtitle("The proportion of cells counted between the Primary sump and After Drumfilter for the 
          top four dominant species for three consecutive months of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#'* used to back up ind spe graph (Aim 2), can possibly do stats on this ---------*

# [Stacked bar graph] comparing PS and ADf biomass on monthly level (more broad than above)
Sp.only_filter %>% 
  group_by(Site, months, month) %>% 
  summarise(monthly.mean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthly.mean, fill = Site)) +
  geom_bar(stat = "Identity", position = "fill") +
  geom_hline(yintercept = 0.5, col = "black") +
  xlab("Months") +
  ylab("Proportion momthly mean Cells/L") + 
  labs(fill = "Site") +
  ggtitle("The proportion of cells counted between the Primary sump and After Drumfilter 
          for three consecutive months of 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# number of red tide days per month
## THIS IS NOT HELPFUL 
Sp.only_filter %>%
  ungroup() %>% 
  select(Date, months, month, FWC_mod) %>% 
  distinct() %>% 
  group_by(month, months, FWC_mod) %>%
  summarise(S = n()) %>% 
  ggplot(., aes(x = reorder(months, month), y = S, fill = FWC_mod)) +
  geom_bar(stat = "Identity")





# NMDS if sites influence species biomass --------------------------------





