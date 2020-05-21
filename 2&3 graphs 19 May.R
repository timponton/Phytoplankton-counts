

## Run 'Building Chp2' and 'Building Chp3" first ###


## Load data




###### Chapter 2 Phytoplanktpn #########
## PHYTOPLANKTON COMMUNITY DIVERSITY ETC -----------------------------------





#'*year-monthly biomass (1) ---------------*
# Plot monthly total biomass, comparing years
biomass.YM <- Sp_only %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyMean = mean(log10(meanCells), na.rm = TRUE), 
            monthlyN = n(),
            monthlySD = sd(log10(meanCells), na.rm = TRUE), 
            monthlySE = sd(log10(meanCells), na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = (monthlyMean), col = year, group = year)) +
  geom_point(size = 1) +
  #geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = monthlyMean - monthlySE, ymax = monthlyMean + monthlySE), width=0.2) +
  xlab("Months") +
  ylab("Log10 Cells/L") + 
  labs(col = "Year") +
  ggtitle("Comparing the log10 monthly mean phytoplankton counts between three consecutive years, with their standard error bars") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

biomass.YM


# STATS for biomass differences between years and months 

sp_mat <- Sp_only %>% 
  group_by(month, year, Species) %>% 
  summarise(avg = mean(meanCells, na.rm = TRUE)) %>% 
  pivot_wider(names_from = "Species", values_from = "avg", 
              values_fill = list(avg = 0))

sp_mat <- Sp_only %>% 
  ungroup() %>% 
  select(Date, months, year, Species, meanCells) %>% 
  pivot_wider(names_from = "Species", values_from = "meanCells", 
              values_fill = list(meanCells = 0))


com <- sp_mat[, 4:ncol(sp_mat)]
com <- as.matrix(com)
com <- vegdist(com, distance = "bray")
sp_grou <- sp_mat[, 2:3]


ano <- anosim(com, grouping = sp_grou$year, distance = "bray")
ano


adon <- adonis2(com ~ months, data = sp_grou, method = "bray", permutations = 999)


anosim(m_com, pc$Type, distance = "bray", permutations = 9999)


###############


#'*year-monthly metrics (1)*
## comparing years over months for indices



#'*(1) ------------------*
# Richness
richness.YM <- dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyRichness = mean(S, na.rm = TRUE), 
            monthlyRichnessN = n(),
            monthlyRichnessSD = sd(S, na.rm = TRUE), 
            monthlyRichnessSE = sd(S, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = (monthlyRichness), col = year, group = year)) +
  geom_point(size = 1) +
  #geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = monthlyRichness - monthlyRichnessSE, ymax = monthlyRichness + monthlyRichnessSE), width=0.2) +
  xlab("Months") +
  ylab("Richness") + 
  # ggtitle("Comparing the monthly species richness of phytoplankton counts between three consecutive years ") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))



#'*(1) -----------------*
# Evenness (J')
evenness.YM <- dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyEvenness = mean(J, na.rm = TRUE), 
            monthlyEvennessN = n(),
            monthlyEvennessD = sd(J, na.rm = TRUE), 
            monthlyEvennessSE = sd(J, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = (monthlyEvenness), col = year, group = year)) +
  geom_point(size = 1) +
  #geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = monthlyEvenness - monthlyEvennessSE, ymax = monthlyEvenness + monthlyEvennessSE), width=0.2) + 
  xlab("Months") +
  ylab("J'") + 
  # ggtitle("Comparing the monthly species evenness (J') of phytoplankton counts between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#'*(1) ---------------*
# Shannon
shannon.YM <- dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlyshannon = mean(shannon, na.rm = TRUE), 
            monthlyshannonN = n(),
            monthlyshannonsD = sd(shannon, na.rm = TRUE), 
            monthlyshannonSE = sd(shannon, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = (monthlyshannon), col = year, group = year)) +
  geom_point(size = 1) +
  #geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = monthlyshannon - monthlyshannonSE, ymax = monthlyshannon + monthlyshannonSE), width=0.2) +
  xlab("Months") +
  ylab("H'") + 
  # ggtitle("Comparing the monthly Shannon (H') Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#'*(1) ----------------*
# Simpson
simpson.YM <- dailyDesc %>% 
  group_by(months, month, year, Season) %>% 
  summarise(monthlysimpson = mean(simpson, na.rm = TRUE), 
            monthlysimpsonN = n(),
            monthlysimpsonsD = sd(simpson, na.rm = TRUE), 
            monthlysimpsonSE = sd(simpson, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = (monthlysimpson), col = year, group = year)) +
  geom_point(size = 1) +
  #geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = monthlysimpson - monthlysimpsonSE, ymax = monthlysimpson + monthlysimpsonSE), width=0.2) + 
  xlab("Months") +
  ylab("Simpson Index of Diversity") + 
  ggtitle("Comparing the monthly Simpson Index of Diversity of phytoplankton between three consecutive years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


# Figure out how to plot these
# Must 1 legend be shown? ANd remove middle ad top x axis labels?
# Capitalise legend title (labs(col = ""))
biomass.YM
richness.YM/evenness.YM/shannon.YM
simpson.YM



#'*This was added late*
# Plot dominants with others greyed out 
# HAB species vs everything else relative abundance 
# Get relative abundance per month for each species
month.rel <- as.data.frame(
  make_relative(
    as.matrix(
      Sp_only %>%
        ungroup() %>% 
        filter(Species != "Diatom") %>% 
        mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
        group_by(YEarMonth, Sp_abb) %>% 
        summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
        pivot_wider(names_from = Sp_abb, values_from = averageDens, 
                    values_fill = list(averageDens = 0)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="YEarMonth")))) %>% 
  rownames_to_column(var = "YEarMonth")

# add month order into df
monthOrder <- Sp_only %>% 
  ungroup() %>% 
  select(YEarMonth, months, month, year)

monthOrder$YEarMonth <- as.factor(monthOrder$YEarMonth)

Sp_abbre <- Sp_only %>%
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
  select(Species) %>% 
  mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
  distinct()


# join month order, reorder columns and pivot longer
month.rel <- month.rel %>%
  ungroup() %>% 
  left_join(monthOrder, by = "YEarMonth", keep = FALSE) %>% 
  select(YEarMonth, months, month, year, everything()) %>% 
  pivot_longer(-c(YEarMonth, months, month, year), names_to = "Sp_abb", values_to = "abun") %>% 
  inner_join(Sp_abbre, by = "Sp_abb")


# find dom species
dom_sp <- month.rel %>% 
  group_by(months) %>%
  top_n(1, abun) %>% 
  ungroup()

# calc top species for each month
dsp <- month.rel %>% 
  group_by(month, Species) %>% 
  summarise(meanRel = mean(abun, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  top_n(1, meanRel)


# plot yearly fluctuations for each of the top species
ggplot() + 
  geom_point(data = month.rel %>% 
               filter(Species %in% dsp$Species), 
             aes(x = reorder(months, month), 
                 y = abun, group = year, col = year), size = 1) +
  geom_line(data = month.rel %>% 
              filter(Species %in% dsp$Species), 
            aes(x = reorder(months, month), 
                y = abun, group = year, col = year), size = 1) +
  facet_wrap(~Species, ncol = 2) +
  theme_classic()

xlab("Months") +
  ylab("Relative abundance") + 
  labs(col = "Phytoplankton species") +
  ggtitle("Fluctuations in the relative abundances of dominant phytoplankton species on a monthly aggregate") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))






# phyto class diversity --------------------------------------------------

#'*[Stacked bar graph] of different phyto classes, by year-months -------------*
# from Sp_Only df all years together
Sp_only %>% 
  group_by(months, month, Season, Classification) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE)) %>% 
  ggplot(., aes(x = reorder(months, month), y = monthlyMean, fill = Classification)) +
  geom_bar(stat = "Identity", position = "fill", col = "black") +
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
  geom_bar(stat = "Identity", position = "fill", col = "Black") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion monthly mean") + 
  labs(fill = "Phytoplankton class") +
  ggtitle("Comparing the proportion of mean monthly counts between phytoplankton 
          classes over three consecutive years") +
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




## RED TIDE CLASSES --------------------------------------------------------


#'*Compare Red tide classes per month ---------*
# Compare the proportion of differnt red tide classes per month over the three years
ggplot(RT.classNumbers_monthYear, aes(x = reorder(months.x, month), y = n, fill = FWC_mod)) +
  geom_bar(stat = "Identity", position = "fill", col = "black") +
  facet_wrap(~year, ncol = 1) +
  xlab("Months") +
  ylab("Proportion number of days") + 
  labs(fill = "Red Tide class") +
  ggtitle("The proportion of different red tide classes per month over the three years") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




## RESOURCES ---------------------------------------------------------------


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
  geom_hline(yintercept = 3.17, size = 1) +
#  geom_smooth(method = "lm", aes(col = FWC_mod), se = FALSE) +
#  geom_smooth(method = "loess", se = FALSE) +
  xlab("Log Cells/L") +
  ylab("Number samples collected per day") + 
  labs(col = "Red Tide class") +
  ggtitle("Relationship between Cells/L counted and number of samples collected per day, 
          when different Red tide severity classes were experienced") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))







####### Chapter 3 Filters ################  


## PHYTOPLANKTON FLUCTUATIONS BETWEEN SITES --------------------------------



# summarise down to monthly measurements to compare sites [boxplot graph]

#'* Graphs for Aim 1a -------------*

# DO NOT USE 
# biomass
ggplot(data = dailydesc_filter, aes(x = reorder(months, month), y = total_biomass, fill = Site)) +
  geom_boxplot() +
  xlab("Months") +
  ylab("Cells/L") + 
  labs(fill = "Site") +
  ggtitle("Compare the difference in biomass of phytoplankton between the two sites, for three consecutive months") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# Line version
ggplot(data = dailydesc_filter, aes(x = Date, y = total_biomass, col = Site)) +
  geom_line(size = 1) +
  facet_wrap(~reorder(months, month), ncol = 1, scales = "free_x")


# Point with errorbar version
Sp.only_filter %>% 
  group_by(months, month, year, YEarMonth, Site) %>% 
  summarise(mean_cells = mean(meanCells, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(meanCells, na.rm = TRUE), 
            se_cells = sd(meanCells, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = reorder(months, month), y = mean_cells, col = Site, group = Site)) +
  geom_point() +
#  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2) +
  xlab("Months") +
  ylab("Cells/L") + 
  labs(fill = "Site") +
#  ggtitle("") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))




## Point errorbars can be done for the metrics  too if necessary ##

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






## DOMINANT SPECIES DENSITIES BETWEEN SITES --------------------------------




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








