
###### Chapter 2 Phytoplanktpn #########

#'*year-monthly biomass (1) ---------------*
# Plot monthly total biomass, comparing years
Sp_only %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = log10(monthlyMean), col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Log10 phyotplankton counts") + 
  labs(col = "Year") +
  ggtitle("Comparing the log10 monthly mean phytoplankton counts between three consecutive years ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*year-monthly metrics (1)*
## comparing years over months for indices



#'*(1) ------------------*
# Richness
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyRichness = mean(S, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyRichness, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Species Richness") + 
  ggtitle("Comparing the monthly species richness of phytoplankton counts between three consecutive years ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'*(1) -----------------*
# Evenness (J')
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyEvenness = mean(J, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyEvenness, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Pielou's J' evenness") + 
  ggtitle("Comparing the monthly species evenness (J') of phytoplankton counts between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#'*(1) ---------------*
# Shannon
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyshannon = mean(shannon, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyshannon, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Shannon Index of Diversity") + 
  ggtitle("Comparing the monthly Shannon (H') Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*(1) ----------------*
# Simpson
dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlysimpson = mean(simpson, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlysimpson, col = year, group = year)) +
  geom_line(size = 2) +
  xlab("Months") +
  ylab("Simpson Index of Diversity") + 
  ggtitle("Comparing the monthly Simpson Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



# Phyto Class diversity ---------------------------------------------------


#'*[Stacked bar graph] of different phyto classes, by year-months -------------*
# from Sp_Only df all years together
Sp_only %>% 
  group_by(months, month, year, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  xlab("Months") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between 
          phytoplankton classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*[Stacked bar graph] of different phyto classes, by months, facetd by year ------------------*
# from Sp_Only df facet_wrap(~year)
Sp_only %>% 
  group_by(months, month, year, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton 
          classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'*Add this in as a comparison ------------*
# All classes plot together
Sp_only %>% 
  group_by(YEarMonth, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = YEarMonth, y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill") +
  xlab("Month Year") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton classes over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




#'*When either Diatoms or Dino's are dominant over the other --------*
# plot dominants
ggplot(DD_dom, aes(x = YEarMonth, y = sqrt(monthlyMean), fill = Dominant)) +
  geom_bar(stat = "Identity") +
  xlab("Month Year") +
  ylab("Monthly Mean of all Phytoplankton") + 
  labs(fill = "Dominant phytoplankton class") +
  ggtitle("Investigate which phytoplankton classes are dominant on a 
          monthly basis over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Include -----------------*
# all month aggregated together to see when diatoms or dino's are dominant over the other
Sp_only %>% 
  group_by(months, month, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Classification", values_from = "monthlyMean",
              values_fill = list(monthlyMean = 0)) %>% 
  mutate(Dominant = ifelse(Diatom > Dinoflagellate, "Diatom", 
                           ifelse(Diatom < Dinoflagellate, "Dinoflagellate", "Other"))) %>% 
  pivot_longer(-c(months, month, Season, Dominant), names_to = "Classification", values_to = "monthlyMean") %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Dominant)) +
  geom_bar(stat = "Identity") +
  xlab("Months") +
  ylab("Monthly Mean of all Phytoplankton") + 
  labs(fill = "Dominant phytoplankton class") +
  ggtitle("Investigate which phytoplankton classes are dominant on a 
          monthly basis over three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




#'*Compare overall cells vs dates, sized by counting times ------------*
# how do counting times change over time, linked to total cells 
left_join(max_cells_day, Time_count, by = "Date") %>% 
  filter(count_times > 0) %>% 
  ggplot(., aes(x = Date, y = log(totalCells))) +
  geom_point(aes(col = FWC_mod, size = count_times)) +
  geom_line() +
  xlab("Date") +
  ylab("Log Phytoplankton counts") + 
  labs(col = "Red Tide class", size = "Samples collected per day") +
  ggtitle("Investigating how sampling frequency varied in relation to 
          red tide severity in 2019") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'* USE TO COMPARE EXCLUDING ALL vs BELOW AVERAGE REGRESSIONS [Scatter and relationship line (smooth)] used to show counting times correlation, above daily counting average ------------*
# correlation between total cells counted and numebr of times counted per day
ggplot(left_join(max_cells_day, Time_count, by = "Date") %>% 
         filter(count_times > 0), aes(x = log(totalCells), y = count_times)) +
  geom_point(aes(col = FWC_mod)) +
  geom_smooth(method = "lm", aes(col = FWC_mod), se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Log Cells/L") +
  ylab("Number samples collected per day") + 
  labs(col = "Red Tide class") +
  ggtitle("Relationship between Cells/L counted and number of sampels collected per day, 
          when different Red tide severity classes were experienced") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'* CHANGE PARAMETER [Scatter and relationship line (smooth)] used to show counting times correlation, above daily counting average ------------*
# correlation between total cells counted and numebr of times counted per day
ggplot(left_join(max_cells_day, Time_count, by = "Date") %>% 
         filter(count_times > 3.12), aes(x = log(totalCells), y = count_times)) +
  geom_point(aes(col = FWC_mod)) +
  geom_smooth(method = "lm", aes(col = FWC_mod), se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  xlab("Log Cells/L") +
  ylab("Number samples collected per day") + 
  labs(col = "Red Tide class") +
  ggtitle("Relationship between Cells/L counted and above average number of samples collected per day, 
          when different Red tide severity classes were experienced") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Compare Red tide classes per month ---------*
# Compare the proportion of differnt red tide classes per month over the three years
ggplot(RT.classNumbers_monthYear, aes(x = reorder(months.x, month), y = n, fill = FWC_mod)) +
  geom_bar(stat = "Identity", position = "fill") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion number of days") + 
  labs(fill = "Red Tide class") +
  ggtitle("The proportion of different red tide classes per month over the three years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



# NMDS --------------------------------------------------------------------



#'*Use to show all species 97 sp --*
# All species
allsp <- ggplot(data = mtet %>%
         pivot_longer(-months, names_to = "Sp_abb", values_to = "abun") %>% 
         group_by(months) %>% 
         arrange(desc(abun)), aes(x = reorder(Sp_abb, abun), y = abun)) +
  geom_col(position = "Dodge") +
  coord_flip() +
  xlab("Species") +
  ylab("Relative abundance") +
  ggtitle("Relative abundance of species (97)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*Use to show dom species species 73 sp --*
# Species above relative abundance of 0.01 per month
domsp <- ggplot(data = mtet %>%
         pivot_longer(-months, names_to = "Sp_abb", values_to = "abun") %>% 
         group_by(months) %>% 
         arrange(desc(abun)) %>% 
         filter(abun > 0.01), aes(x = reorder(Sp_abb, abun), y = abun)) +
  geom_col(position = "Dodge") +
  coord_flip() +
  xlab("Species") +
  ylab("Relative abundance") +
  ggtitle("Species with a relative abundance > 0.01 per month (73)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


allsp / domsp


#'*[NMDS] check what it is based on i.e relative abundance or actual counts ------*
plot(NMDStestmonth)
ordiplot(NMDStestmonth,type="n")
orditorp(NMDStestmonth,display="species",col="red",air=0.01)
orditorp(NMDStestmonth,display="sites",cex=1.25,air=0.01)





####### Chapter 3 Filters ################  
  
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
  ylab("Pielou's (J') Evenness") + 
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

#'* Graphs for Aim 1b --------------*
ggplot(pos, aes(x = Location, y = log10(Cells.L))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col = `Day Time period`), size = 2) +
  xlab("Location") +
  ylab(" Log10 Total Cells/L") +
  labs(col = "Period of Day") +
  ggtitle("Cells/L measured on each subfarm on the 26/02/2019, with farms ordered in flow position, 
          and points indicating relative sampling period") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



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



#'*This was added late*
# Plot dominants with others greyed out 
ggplot() +
  geom_line(data = month.rel, 
            aes(x = reorder(months, month), 
                y = abun, group = Species, col = Species), 
            colour = alpha("grey", 0.7)) +
  geom_line(data = month.rel %>% 
              filter(Sp_abb %in% dom_sp$Sp_abb), 
            aes(x = reorder(months, month), 
                y = abun, group = Species, col = Species), size = 1) +
  xlab("Months") +
  ylab("Relative abundance") + 
  labs(col = "Phytoplankton species") +
  ggtitle("Fluctuations in the relative abundances of dominant phytoplankton species on a monthly aggregate") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



