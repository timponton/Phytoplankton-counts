
### Predicting red tide cases using species counts ###
library(rpart)
library(rpart.plot)


dom_sp <- Sp_only %>% 
  group_by(Species, months) %>% 
  summarise(me = mean(meanCells, na.rm = TRUE)) %>% 
  arrange(desc(-me)) %>% 
  filter(me > 1000) %>% 
  ungroup() %>% 
  group_by(Species) %>% 
  summarise(me = mean(me, na.rm = TRUE)) %>% 
  distinct()


red <- Sp_only %>% 
  ungroup() %>% 
  #filter(year == "2019") %>% 
  filter(Species %in% dom_sp$Species) %>% 
  pivot_wider(names_from = "Species", values_from = "meanCells", 
              values_fill = list(meanCells = 0)) %>% 
  select(-Date, -Classification, -year, 
         -months, -month, -YEarMonth, 
         -Season, -SeasonOrder, -FWC_mod) %>% 
  # #mutate(rt_class = if_else(RedTide == "High" | 
  #                             RedTide == "Medium" | 
  #                             RedTide == "Low (B)" |
  #                             RedTide == "Low (A)", "High","Low")) %>% 
  select(RedTide, everything())#, rt_class, everything()) #%>% 
  #select(-RedTide)


fit <- rpart(RedTide ~ ., data = red, method = "class")
rpart.plot(fit, extra= 104, fallen.leaves = TRUE, type = 5)



## Double check Red tide days 

check_rt <- df %>% 
  filter(Site == "Primary sump", year != "2020") %>% 
  group_by(Time, Date, months, month, year) %>% 
  summarise(totalCells = sum(Cells.L)) %>% 
  ungroup() %>% 
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High"))))))) %>% 
  mutate(FWC_mod = ifelse(totalCells >= 1 & totalCells <= 1000, "Normal", 
                          ifelse(totalCells >= 1001 & totalCells <= 10000, "Very Low", 
                                 ifelse(totalCells >= 10001 & totalCells <= 100000, "Low", 
                                        ifelse(totalCells >= 100001 & totalCells < 1000000, "Medium", "High")))))

rt_days <- check_rt %>% 
  group_by(Date) %>% 
  filter(totalCells == max(totalCells))
  
  

new_rt <- df %>% 
  filter(Site == "Primary sump", year != "2020") %>% 
  group_by(Date, months, month, Species, FWC_mod) %>% 
  summarise(meanCells = mean(Cells.L)) %>% 
  semi_join(rt_days, by = "Date")

dom_dates <- new_rt %>% 
  group_by(Date) %>% 
  summarise(me = mean(meanCells, na.rm = TRUE)) %>% 
  filter(me >= 3000)

dom_spec <- new_rt %>% 
  filter(Species != "HAB Total Count", 
         Species != "All species", 
         Species != "Other", 
         Species != "Diatom") %>% 
  group_by(Species) %>% 
  summarise(me = mean(meanCells, na.rm = TRUE)) %>% 
  arrange(desc(me)) %>% 
  slice(1:10)


z <- new_rt %>% 
  filter(Species %in% dom_spec$Species, Date %in% dom_dates$Date) %>% 
  pivot_wider(names_from = "Species", values_from = "meanCells", 
              values_fill = list(meanCells = 0)) %>% 
  ungroup() %>% 
  select(-Date, -months, -month)


control <- rpart.control(minsplit = 5,
                         minbucket = 5/3,
                        cp = 0.001)


fit <- rpart(FWC_mod ~ ., data = z, method = "class", control = control)
rpart.plot(fit, extra= 104)

group_by(Date, months, month, year) %>% 
  summarise(totalCells = mean(time_total, na.rm = TRUE))
group_by(Date) %>% 
  filter(max())


ggplot(.,aes(x = reorder(months, month), y = daily_mean, fill = year)) +
  geom_boxplot() +
  geom_smooth(method = "loess") +
  scale_y_log10()





