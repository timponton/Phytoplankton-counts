
### Look at species data for significance

sp_base_sig <- ps_adf %>% 
  filter(Date %in% small_date_sig$Date) %>% 
  select(Date, Site, Species, Classification, Correct.Original) %>%
  filter(Classification != "Various", 
         Species != "Diatom") %>%
  ungroup() %>% 
  group_by(Date, Species, Site) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = 0)) %>% 
  filter(`Primary sump` > 0, 
         `After Drumfilter` > 0) %>% 
  pivot_longer(-c(Date, Species), names_to = "Site", values_to = "sumC") %>% 
  group_by(Date, Species) %>% 
  wilcox_test(sumC ~ Site, p.adjust.method = "none", exact = TRUE, detailed = TRUE) %>% 
  select(Date, Species)


## For classification
class_base_sig <- ps_adf %>% 
  filter(Date %in% small_date_sig$Date) %>% 
  select(Date, Site, Classification, Correct.Original, Time) %>%
  filter(Classification != "Various") %>%
  ungroup() %>% 
  group_by(Date, Time, Classification, Site) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Site, values_from = sumC, values_fill = list(sumC = 0)) %>% 
  filter(`Primary sump` > 0, 
         `After Drumfilter` > 0) %>% 
  pivot_longer(-c(Date, Classification, Time), names_to = "Site", values_to = "sumC") %>% 
  select(Date, Classification, Time, Site, sumC)

group_by(Date, Classification) %>% 
  wilcox_test(sumC ~ Site, p.adjust.method = "none", exact = TRUE, detailed = TRUE) %>% 
  add_significance()

# Get full signfic for Classification
ps_adf %>% 
  filter(Date %in% small_date_sig$Date) %>% 
  select(Date, Site, Classification, Correct.Original, Time) %>%
  filter(Classification != "Various", 
         Classification != "Dictyochophyceae") %>%
  ungroup() %>% 
  select(Date, Site, Correct.Original, Classification, Time) %>% 
  group_by(Date, Time, Site, Classification) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Date, Site, sumC, Classification) %>% 
  filter(Date %in% class_base_sig$Date, 
         Date != "2019-03-01") %>% 
  group_by(Date, Classification) %>%
  wilcox_test(sumC ~ Site, p.adjust.method = "none", exact = TRUE, detailed = TRUE) %>% 
  add_significance() %>% 
  view()

# MEans and ste for classifications

ps_adf %>% 
  filter(Date %in% small_date_sig$Date) %>% 
  select(Date, Site, Classification, Correct.Original, Time) %>%
  filter(Classification != "Various", 
         Classification != "Dictyochophyceae") %>%
  ungroup() %>% 
  select(Date, Site, Correct.Original, Classification, Time) %>% 
  group_by(Date, Time, Site, Classification) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Date, Site, Classification) %>% 
  summarise(mean_cells = mean(sumC, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(sumC, na.rm = TRUE), 
            se_cells = sd(sumC, na.rm = TRUE)/sqrt(n())) %>%
  filter(Date != "2019-02-23") %>% 
  ggplot(., aes(x = Date, y = mean_cells, col = Site, group = Site)) +
  geom_point(position = pd) +
  facet_wrap(~Classification, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.4, position = pd) +
  theme_classic()



###############



# Dates when specific species were significantly reduced between PS and ADF
sig_sp <- ps_adf %>% 
  filter(Date %in% small_date_sig$Date) %>% 
  select(Date, Site, Species, Classification, Correct.Original) %>%
  filter(Classification != "Various") %>% 
  right_join(sp_base_sig, by = c("Date", "Species")) %>% 
  group_by(Date, Species) %>% 
  wilcox_test(Correct.Original ~ Site, p.adjust.method = "none", exact = TRUE, detailed = TRUE) %>% 
  add_significance() %>% 
  filter(p.signif != "ns") %>% 
  select(Date, Species, p.signif) %>% 
  filter(Species %in% c("Gonyaulax polygramma", "Lingulodinium polyedra", 
                        "Ceratium furca"))



test_it <- ps_adf %>% 
  filter(Date %in% small_date_sig$Date, 
         Species %in% c("Gonyaulax polygramma", "Lingulodinium polyedra", 
                        "Ceratium furca")) %>% 
  group_by(Species, Date, Site) %>% 
  summarise(mean_cells = mean(Correct.Original, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(Correct.Original, na.rm = TRUE), 
            se_cells = sd(Correct.Original, na.rm = TRUE)/sqrt(n()))

manage <- sig_sp %>% 
  select(Date, Species, p.signif) %>% 
  mutate(refe = if_else(Species == "Gonyaulax polygramma", 1000000, if_else(Species == "Lingulodinium polyedra", 210000, if_else(Species == "Ceratium furca", 75000, 6000)))) %>% 
  filter(Species %in% test_it$Species)




ggplot() +
  geom_point(data = test_it, aes(x = Date, y = mean_cells, col = Site, group = Site), position = pd) +
  #geom_line() +
  geom_errorbar(data = test_it, aes(x = Date, y = mean_cells, col = Site, group = Site, ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.4, position = pd) +
  facet_wrap(~Species, ncol = 1, scales = "free_y") +
  geom_text(data = manage, aes(x = Date, y = refe,label = p.signif)) +
  theme_classic()


## Figure out better way to plot species
small_time_date_sig <- ps_adf %>% 
  filter(Date %in% small_diff_dates$Date) %>%
  group_by(Date, Time, Site) %>% 
  summarise(sumC = sum(Correct.Original, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Site, sumC, Date, Time) %>% 
  mutate(Site = recode(Site, `After Drumfilter` = "Secondary Sump", `Primary sump` = "Primary Sump")) %>% 
  group_by(Date) %>% 
  wilcox_test(sumC ~ Site, p.adjust.method = "none", exact = TRUE, detailed = TRUE) %>% 
  add_significance() %>% 
  select(Date, p.signif) %>% 
  mutate(refe = 1700000)


df_test <- ps_adf %>% 
  filter(Date >= "2019-02-15" & Date <= "2019-02-24", 
         Species %in% c("Gonyaulax polygramma", "Lingulodinium polyedra", 
                        "Ceratium furca")) %>% 
  mutate(Species = factor(Species, levels = c("Gonyaulax polygramma", "Lingulodinium polyedra", 
                                                "Ceratium furca"))) %>% 
  group_by(Species, Date, Site) %>% 
  summarise(mean_cells = mean(Correct.Original, na.rm = TRUE), 
            n_cells = n(),
            sd_cells = sd(Correct.Original, na.rm = TRUE), 
            se_cells = sd(Correct.Original, na.rm = TRUE)/sqrt(n())) %>% 
  left_join(sig_sp, by = c("Date", "Species")) %>%
  filter(Date != "2019-02-16", 
         Date != "2019-02-22") %>% 
  ungroup() %>% 
  mutate(starting = row_number(Date)) %>% 
  mutate(date_2 = factor(format(Date, "%d/%m")))

df_test$date_2 <- as.factor(df_test$date_2)


ggplot(df_test, aes(x = reorder(date_2, starting), y = mean_cells, col = Site, group = Site), position = pd) +
  geom_point(position = pd) +
  geom_errorbar(aes(ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.4, position = pd) +
  facet_wrap(~Species, ncol = 1, scales = "free_y") +
  geom_text(aes(y = mean_cells + se_cells, 
                label = if_else(Site == "Primary sump" & p.signif != "NA", p.signif, "")), 
            col = "Black", size = 7) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_color_discrete(labels = c("Primary Sump", "Secondary Sump")) + 
  ylab("Cells/L") +
  xlab("Date") +
  theme_classic() +
  theme(strip.text.x = element_text(face = "italic"), 
        plot.title = element_text(hjust = 0.5), 
        text=element_text(size=17), 
        axis.text.x = element_text(face="bold", 
                                   size=13),
        axis.text.y = element_text(face="bold", 
                                   size=13))




ggplot() +
  geom_point(data = test_it, aes(x = Date, y = mean_cells, col = Site, group = Site), position = pd) +
  #geom_line() +
  geom_errorbar(data = test_it, aes(x = Date, y = mean_cells, col = Site, group = Site, ymin = mean_cells - se_cells, ymax = mean_cells + se_cells), width=0.4, position = pd) +
  facet_wrap(~Species, ncol = 1, scales = "free_y") +
  geom_text(data = manage, aes(x = Date, y = refe,label = p.signif)) +
  theme_classic()

