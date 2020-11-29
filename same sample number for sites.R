

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
  select(Date, p.signif, n1)



## Plot daily differences between Sites for each day (mean ± ste)
pd <- position_dodge(width = 0.4)

daily_plot_data <- Perfect_df %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site, months) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>%
  left_join(wilcox_dates, by = "Date") %>% 
  mutate(Site = recode(Site, `After Drumfilter` = "Secondary Sump", `Primary sump` = "Primary Sump")) %>% 
  mutate(months = factor(months, levels = c("February", "March", "April"))) %>% 
  filter(months != "April")





smaller_than <- ggplot(daily_plot_data, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  coord_cartesian(ylim = c(0, 2200000)) +
  geom_text(inherit.aes = FALSE, x = as.Date("2019-02-15"), y = 1750000, label = "B", size = 10) +
  facet_wrap(~months, nrow = 1, scales = "free_x", strip.position="bottom") +
  xlab("Date") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13), 
        legend.position='none', panel.spacing = unit(0, "lines"))


smaller_than


  
bigger_than <- ggplot(daily_plot_data, aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_date(date_breaks = "2 day", date_labels = "%d") +
  geom_hline(yintercept = 2200000, linetype = "dashed") +
  geom_text(inherit.aes = FALSE, x = as.Date("2019-02-15"), y = 10000000, label = "A", size = 10) +
  facet_wrap(~months, nrow = 1, scales = "free_x") +
  ylab("Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(face="bold", 
                                   size=13), 
        strip.background = element_blank(), 
        strip.text.x = element_blank(), 
        legend.position = "top", panel.spacing = unit(0, "lines"))

## This will be USED 
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

## Check filtered/primary sump (Or percentage of PS)
# daily (Use this to show relationship between density and filtration ability)


## This will be USED
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanC, values_fill = list(meanC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, n1 >= 4, p.signif != "ns") %>% 
  ggplot(., aes(x = `Primary sump`, y = filtered_to_PS)) +
  geom_point(aes(col = p.signif), size = 4) +
  geom_smooth(span = 2, se = FALSE) +
  geom_label_repel(aes(label = factor(Date)),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "grey50") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  xlab("Primary Sump Cells/L") +
  ylab("Percentage Filtered (%)") +
  labs(col = paste("P-value", "\n", "Significance")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))




Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanC, values_fill = list(meanC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, n1 >= 4, p.signif != "ns") %>% 
  ggplot(., aes(x = `Primary sump`, y = filtered_to_PS)) +
  geom_point(aes(col = p.signif), size = 4) +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  xlab("Primary Sump Cells/L") +
  ylab("Percentage Filtered (%)") +
  labs(col = "P Significance") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))



### Maybe try and select adjacent days and check filtered patterns. selected days should have lots of samples. 

# Fluctuations in a specifc day 
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Date >= "2019-02-18" & Date <= "2019-02-23") %>%
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = 0)) %>% 
  view()
  
ggplot(., aes(x = dateTime, y = sumC, fill = Site)) +
  geom_bar(stat = "Identity", position = "Dodge")
geom_point(position = pd) +
  geom_line()




## Just check filtered vs adf for all
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #group_by(Date, Site) %>% 
  #summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, 
         `After Drumfilter` < 2500000) %>%
  filter(delta > 0, n1 > 4, p.signif != "ns") %>% 
  ggplot(., aes(x = filtered_to_PS, y = `After Drumfilter`)) +
  geom_point(aes(col = p.signif), size = 4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  geom_hline(yintercept = 10000) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  ylab("Secondary Sump Cells/L") +
  xlab("Percentage Filtered (%)") +
  labs(col = "P Significance") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))





####


Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #group_by(Date, Site) %>% 
  #summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, 
         `After Drumfilter` < 2500000) %>%
  filter(delta > 0, n1 > 4, p.signif != "ns") %>% 
  mutate(perc_case = case_when(filtered_to_PS > 0 & filtered_to_PS < 50 ~ "<50",
                               filtered_to_PS >= 50 & filtered_to_PS < 80 ~ "50-79", 
                               filtered_to_PS >= 80 ~ ">80",
                               TRUE ~ "Other"), 
         perc_case = factor(perc_case, levels = c("<50", "50-79", ">80"))) %>% 
  ggplot(., aes(x = `Primary sump`, y = `After Drumfilter`)) +
  geom_point(aes(col = p.signif)) +
  facet_wrap(~perc_case, ncol = 1, scales = "free") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  coord_cartesian(xlim = c(0, 2500000)) +
  ylab("Secondary Sump Cells/L") +
  xlab("Primary Sump Cells/L") +
  labs(col = "Percentage Filtered (%)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))

Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanC, values_fill = list(meanC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, 
         `After Drumfilter` < 2500000) %>%
  filter(delta > 0, n1 > 4, p.signif != "ns") %>% 
  mutate(perc_case = case_when(filtered_to_PS > 0 & filtered_to_PS < 50 ~ "<50",
                               filtered_to_PS >= 50 & filtered_to_PS < 55 ~ "50-54", 
                               filtered_to_PS >= 55 & filtered_to_PS < 60 ~ "55-59",
                               filtered_to_PS >= 60 & filtered_to_PS <= 65 ~ "60-65",
                               TRUE ~ "Other"), 
         perc_case = factor(perc_case, levels = c("<50", "50-54", "55-59", "60-65"))) %>% 
  ggplot(., aes(x = `Primary sump`, y = `After Drumfilter`)) +
  geom_point(aes(col = perc_case), size = 4) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  #coord_cartesian(xlim = c(0, 2500000)) +
  ylab("Secondary Sump Cells/L") +
  xlab("Primary Sump Cells/L") +
  labs(col = "Percentage Filtered (%)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))


###


## Just check filtered vs adf for when ps was reduced to < 10 000


## This will be USED
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #group_by(Date, Site) %>% 
  #summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(delta > 0, 
         `After Drumfilter` < 10000, 
         `Primary sump` > 10000) %>%
  mutate(`Primary sump` = format(`Primary sump`, big.mark = ",", scientific = FALSE)) %>% 
  #filter(delta > 0, n1 > 4, p.signif != "ns") %>% 
  ggplot(., aes(x = filtered_to_PS, y = `After Drumfilter`)) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = paste("PS", ":", `Primary sump`, "Cells/L")),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "grey50") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), 
                     breaks = seq(0, 10000, 750)) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  ylab("Secondary Sump Cells/L") +
  xlab("Percentage Filtered (%)") +
  labs(col = "Primary Sump Cells/L") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))


################### Below this is scrap work ####


# Time-ly
Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #group_by(Date, Site) %>% 
  #summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  filter(p.signif != "ns", 
         delta > 0) %>% 
  ggplot(., aes(x = `Primary sump`, y = filtered_to_PS)) +
  geom_point(aes(col = p.signif), size = 2) +
  geom_smooth(method = "loess", span = 2) +
  coord_cartesian(xlim = c(0, 2000000), ylim = c(0, 100)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))







Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site) %>% 
  summarise(meanC = mean(sumC, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Site, values_from = meanC, values_fill = list(meanC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  ggplot(., aes(x = Date, y = filtered_to_PS)) +
  geom_point(aes(col = p.signif, size = `Primary sump`)) +
  geom_line() +
  #coord_cartesian(xlim = c(0, 200000)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))


# Faceted days 

Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>%
  #filter(`Primary sump` < 200000, 
         #filtered_to_PS < 100) %>% 
  ggplot(., aes(x = `Primary sump`, y = filtered_to_PS)) +
  geom_point(aes(col = p.signif)) +
  geom_smooth() +
  facet_wrap(~Date, scales = "free")
  






##


# DateTime with filtered percentage
ggplotly(Perfect_df %>% 
  group_by(Date, Time, Site, months, month, dateTime) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = NA)) %>% 
  mutate(abs_delta = abs(`Primary sump` - `After Drumfilter`), 
         delta = `Primary sump` - `After Drumfilter`, 
         filtered_to_PS = abs_delta/`Primary sump`*100) %>% 
  left_join(wilcox_dates, by = "Date") %>% 
  ggplot(., aes(x = dateTime, y = filtered_to_PS)) +
  geom_point(aes(col = factor(Date))) +
  geom_line())





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
         Species == "Lingulodinium polyedra") %>% 
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
         Species == "Lingulodinium polyedra")


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
  filter(Species == "Lingulodinium polyedra") %>% 
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
  wilcox_test(sumC ~ Site, paired = TRUE, p.adjust.method = "bonferroni") %>% 
  add_significance() %>% 
  select(Date, p.signif)



## Plot daily differences between Sites for each day (mean ± ste)

daily_plot_data_LP <- Perfect_df_LP %>% 
  group_by(Date, Time, Site, months, month) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site, months) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>%
  left_join(wilcox_dates_LP, by = "Date") %>% 
  mutate(Site = recode(Site, `After Drumfilter` = "Secondary Sump", `Primary sump` = "Primary Sump")) %>% 
  mutate(starting = row_number(Date)) %>% 
  mutate(date_2 = factor(format(Date, "%d/%m"))) %>% 
  mutate(months = factor(months, levels = c("February", "March", "April"))) %>% 
  filter(months != "April")


daily_plot_data_LP$date_2 <- as.factor(daily_plot_data_LP$date_2)
reorder(date_2, starting)

smaller_than_LP <- ggplot(daily_plot_data_LP, aes(x = reorder(date_2, starting), y = mean_cells, col = Site, group = Site)) +
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






bigger_than_LP <- ggplot(daily_plot_data_LP, aes(x = format.Date(Date, "%d"), y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  #  geom_line() +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.2, position = pd) +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary Sump" & p.signif != "ns", p.signif, "")), 
            col = "Black", size = 7, vjust = -0.5) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  facet_wrap(~months, scales = "free_x", strip.position="bottom") +
  ylab("Cells/L") +
  xlab("Date") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17),
        axis.text.y = element_text(face="bold", 
                                   size=13), panel.spacing = unit(0, "lines"))

## This will be USED
bigger_than_LP






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





