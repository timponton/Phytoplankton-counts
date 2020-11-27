

### Total density and looking at some species 

# Figure out what dates where sampled for each site
# Remove the times in each of those days where only one of the sites were sampled
# analyse and plot 




# Dates when both sites were sampled
Total_dates <- df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter")) %>% 
  select(Date, Site) %>% 
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1, 
         Date != "2019-11-20", 
         Date != "2019-11-21", 
         Date != "2019-11-22")

# Use date to reference what to filter from main df
Density_total <- df %>% 
  filter(Date %in% Total_dates$Date, 
         Site %in% c("Primary sump", "After Drumfilter"))

## Find out if times can be reduced
# First find out which dates are available and reduced times so that each site is equal
# Also removed dates when number of available times are less than 3

adequate_dates <- Density_total %>% 
  select(Date, Time, Site) %>%
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1) %>% 
  pivot_longer(-c(Date, Time), names_to = "Site", values_to = "dumm") %>% 
  select(Date, Site, Time) %>% 
  distinct() %>% 
  group_by(Date, Site) %>% 
  count() %>% 
  filter(n >= 3)
  
# Use above to filter dates, but first work through and get the same numebr of times

adequate_date_times <- Density_total %>% 
  select(Date, Time, Site) %>%
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1) %>% 
  pivot_longer(-c(Date, Time), names_to = "Site", values_to = "dumm") %>% 
  select(Date, Site, Time) %>% 
  filter(Date %in% adequate_dates$Date) %>% 
  left_join(df %>% 
              select(Date, Time, dateTime), by = c("Date", "Time")) %>% 
  distinct()
  


# Use main df and filter the specific Date and Time and Site

Perfect_df <- df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter")) %>% 
  filter(dateTime %in% adequate_date_times$dateTime) %>% 
  mutate(Site = factor(Site, levels = c("Primary sump", "After Drumfilter")))




## Get Time totals for each day
Perfect_df %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  group_by(Date, Site) %>% 
  count()


# Perform paired Wilcox between sites grouping for Date
wilcox_dates <- Perfect_df %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Date, Site, sumC) %>% 
  group_by(Date) %>% 
  wilcox_test(sumC ~ Site, paired = TRUE) %>% 
  add_significance() %>% 
  select(Date, p.signif)



## Plot daily differences between Sites for each day (mean ± ste)
pd <- position_dodge(width = 0.4)

daily_plot_data <- Perfect_df %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>%
  left_join(wilcox_dates, by = "Date") %>% 
  mutate(Site = recode(Site, `After Drumfilter` = "Secondary Sump", `Primary sump` = "Primary Sump"))





smaller_than <- ggplot(daily_plot_data, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  coord_cartesian(ylim = c(0, 2500000)) +
  xlab("Date") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))





  
bigger_than <- ggplot(daily_plot_data, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_hline(yintercept = 2500000, linetype = "dashed") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face="bold", 
                                   size=13))

bigger_than/smaller_than  
  


# Plot the delta between sites for each time 
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`) %>%
  left_join(wilcox_dates, by = "Date") %>% 
  filter(Date >= "2019-02-01" & Date <= "2019-03-15") %>% 
  ggplot(., aes(x = dateTime, y = delta, col = p.signif)) +
  geom_point() +
  coord_cartesian(ylim = c(-2500000, 2500000))



#### Species data

df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter"), 
         Species == "Gonyaulax polygramma") %>% 
  select(Date, Site) %>% 
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1, 
         Date != "2019-11-20", 
         Date != "2019-11-21", 
         Date != "2019-11-22") %>% 
  view()


df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter")) %>% 
  filter(Species == "Ceratium furca") %>% 
  select(Date, Time, Site) %>% 
  distinct() %>% 
  group_by(Date, Site) %>% 
  count() %>% 
  pivot_wider(names_from = Site, values_from = n, values_fill = list(n = 0)) %>% 
  filter(`Primary sump` > 2, 
         `After Drumfilter` > 2) %>% 
  view()





################################################################
## Lingulodinium polyedra ####

# Dates when both sites were sampled
Total_dates_LP <- df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter"), 
         Species == "Ceratium furca") %>% 
  select(Date, Site) %>% 
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1, 
         Date != "2019-11-20", 
         Date != "2019-11-21", 
         Date != "2019-11-22")

# Use date to reference what to filter from main df
Density_total_LP <- df %>% 
  filter(Date %in% Total_dates_LP$Date, 
         Site %in% c("Primary sump", "After Drumfilter"), 
         Species == "Ceratium furca")


adequate_dates_LP <- Density_total_LP %>%
  #filter(Species == "Lingulodinium polyedra") %>% 
  select(Date, Time, Site) %>%
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1) %>% 
  pivot_longer(-c(Date, Time), names_to = "Site", values_to = "dumm") %>% 
  select(Date, Site, Time) %>% 
  distinct() %>% 
  group_by(Date, Site) %>% 
  count() %>% 
  filter(n >= 2)

# Use above to filter dates, but first work through and get the same numebr of times

adequate_date_times_LP <- Density_total_LP %>% 
  #filter(Species == "Lingulodinium polyedra") %>% 
  select(Date, Time, Site) %>%
  distinct() %>% 
  mutate(dum = 1) %>% 
  pivot_wider(names_from = Site, values_from = dum, values_fill = list(dum = 0)) %>% 
  filter(`Primary sump` == 1, 
         `After Drumfilter` == 1) %>% 
  pivot_longer(-c(Date, Time), names_to = "Site", values_to = "dumm") %>% 
  select(Date, Site, Time) %>% 
  filter(Date %in% adequate_dates_LP$Date) %>% 
  left_join(df %>% 
              select(Date, Time, dateTime), by = c("Date", "Time")) %>% 
  distinct()

adequate_date_times_LP %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  count()

# Use main df and filter the specific Date and Time and Site

Perfect_df_LP <- df %>% 
  filter(Classification != "Ciliate", 
         Classification != "Spirotrichea", 
         Classification != "Litostomatea", 
         Site %in% c("Primary sump", "After Drumfilter")) %>% 
  filter(dateTime %in% adequate_date_times_LP$dateTime) %>% 
  filter(Species == "Ceratium furca") %>% 
  mutate(Site = factor(Site, levels = c("Primary sump", "After Drumfilter")))
  


## Get Time totals for each day
Perfect_df_LP %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  group_by(Date, Site) %>% 
  count()



# Perform paired Wilcox between sites grouping for Date
wilcox_dates_LP <- Perfect_df_LP %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Date, Site, sumC) %>% 
  group_by(Date) %>% 
  wilcox_test(sumC ~ Site, paired = TRUE, p.adjust.method = "none") %>% 
  add_significance() %>% 
  select(Date, p.signif)



## Plot daily differences between Sites for each day (mean ± ste)

daily_plot_data_LP <- Perfect_df_LP %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>%
  left_join(wilcox_dates_LP, by = "Date") %>% 
  mutate(Site = recode(Site, `After Drumfilter` = "Secondary Sump", `Primary sump` = "Primary Sump"))





smaller_than_LP <- ggplot(daily_plot_data_LP, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  coord_cartesian(ylim = c(0, 50000)) +
  xlab("Date") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))






bigger_than_LP <- ggplot(daily_plot_data_LP, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_hline(yintercept = 50000, linetype = "dashed") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face="bold", 
                                   size=13))

bigger_than_LP/smaller_than_LP 



df %>% 
  filter(Date %in% Total_dates_LP$Date, 
         Site %in% c("Primary sump", "After Drumfilter")) %>% 
  filter(Species == "Lingulodinium polyedra") %>% 
  select(Date, Site, Time, Correct.Original) %>% 
  group_by(Date, Time, Site) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>% 
  ggplot(., aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd)





