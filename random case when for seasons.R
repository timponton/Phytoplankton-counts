


as.data.frame(
  make_relative(
    as.matrix(
      Sp_only %>%
        mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
        ungroup() %>% 
        filter(Species != "Diatom") %>% 
        #mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
        group_by(YEarMonth, Species) %>% 
        summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>% 
        pivot_wider(names_from = Species, values_from = averageDens, 
                    values_fill = list(averageDens = 0)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="YEarMonth")))) %>% 
  rownames_to_column(var = "YEarMonth") %>% 
  pivot_longer(-YEarMonth, names_to = "Species", values_to = "abun") %>% 
  ungroup() %>% 
  left_join(monthOrder, by = "YEarMonth", keep = TRUE) %>% 
  distinct() %>% 
  mutate(season = case_when(Date >= "2018-09-01" & Date <= "2018-11-30" ~ "Spring 2018",
                            month == "2" ~ "Summer",
                            month == "3" ~ "Autumn",
                            month == "4" ~ "Autumn",
                            month == "5" ~ "Autumn",
                            month == "6" ~ "Winter",
                            month == "7" ~ "Winter",
                            month == "8" ~ "Winter",
                            month == "9" ~ "Spring",
                            month == "10" ~ "Spring",
                            month == "11" ~ "Spring",
                            month == "12" ~ "Summer",
                            TRUE ~ "Other")) %>% 
  view()


season_ordered_exact <- c("Spring 2018", 
                          "Summer 2018/2019", 
                          "Autumn 2019", 
                          "Winter 2019", 
                          "Spring 2019", 
                          "Summer 2019/2020")

most_dom <- as.data.frame(
  make_relative(
    as.matrix(
      Sp_only %>%
        mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
        ungroup() %>% 
        filter(Species != "Diatom") %>% 
        group_by(Species, YEarMonth) %>% 
        summarise(meanC = mean(meanCells, na.rm = TRUE)) %>%
        pivot_wider(names_from = Species, values_from = meanC, 
                    values_fill = list(meanC = 0)) %>% 
        remove_rownames %>% 
        column_to_rownames(var="YEarMonth")))) %>% 
  rownames_to_column(var = "YEarMonth") %>% 
  pivot_longer(-YEarMonth, names_to = "Species", values_to = "abun") %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(per = abun*100) %>% 
  filter(per > 1) %>%
  select(Species) %>% 
  distinct()

as.data.frame(
  make_relative(
    as.matrix(
      Sp_only %>%
        mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
        mutate(season_exact = case_when(Date >= "2018-09-01" & Date <= "2018-11-30" ~ "Spring 2018",
                                        Date >= "2018-12-01" & Date <= "2019-02-28" ~ "Summer 2018/2019", 
                                        Date >= "2019-03-01" & Date <= "2019-05-31" ~ "Autumn 2019", 
                                        Date >= "2019-06-01" & Date <= "2019-08-31" ~ "Winter 2019", 
                                        Date >= "2019-09-01" & Date <= "2019-11-30" ~ "Spring 2019",
                                        Date >= "2019-12-01" & Date <= "2019-12-31" ~ "Summer 2019/2020",
                                        TRUE ~ "Other")) %>% 
  ungroup() %>% 
  filter(Species != "Diatom") %>% 
  #mutate(Sp_abb = abbreviate(Species, 5, strict = FALSE)) %>% 
  group_by(season_exact, Species) %>% 
  summarise(averageDens = mean(meanCells, na.rm = TRUE)) %>%
  pivot_wider(names_from = Species, values_from = averageDens, 
              values_fill = list(averageDens = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="season_exact")))) %>% 
  rownames_to_column(var = "season_exact") %>% 
  pivot_longer(-season_exact, names_to = "Species", values_to = "abun") %>% 
  ungroup() %>% 
  distinct() %>%
  mutate(abun = round(abun*100, 2)) %>%
  filter(Species %in% most_dom$Species) %>% 
  pivot_wider(names_from = season_exact, values_from = abun, 
              values_fill = list(abun = 0)) %>%
  relocate(Species, `Spring 2018`,`Summer 2018/2019`, `Autumn 2019`, `Winter 2019`, `Spring 2019`, `Summer 2019/2020`) %>% 
  view()


  
  
  
most_dom <- as.data.frame(
    make_relative(
      as.matrix(
        Sp_only %>%
          mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
          ungroup() %>% 
          filter(Species != "Diatom") %>% 
          group_by(Species, YEarMonth) %>% 
          summarise(meanC = mean(meanCells, na.rm = TRUE)) %>%
          pivot_wider(names_from = Species, values_from = meanC, 
                      values_fill = list(meanC = 0)) %>% 
          remove_rownames %>% 
          column_to_rownames(var="YEarMonth")))) %>% 
    rownames_to_column(var = "YEarMonth") %>% 
    pivot_longer(-YEarMonth, names_to = "Species", values_to = "abun") %>% 
    ungroup() %>% 
    distinct() %>% 
  mutate(per = abun*100) %>% 
  filter(per > 1) %>%
  select(Species) %>% 
  distinct()
  
Sp_only %>% 
  select(Species) %>% 
  distinct() %>% 
  anti_join(most_dom)
  








as.data.frame(
    make_relative(
      as.matrix(
        Sp_only %>%
          mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
          group_by(Species, YEarMonth) %>% 
          summarise(meanC = mean(meanCells, na.rm = TRUE)) %>%
          pivot_wider(names_from = Species, values_from = meanC, 
              values_fill = list(meanC = 0)) %>% 
  remove_rownames %>% 
  column_to_rownames(var="YEarMonth")))) %>% 
  view()





Sp_only %>%
  mutate(YEarMonth = as.yearmon(paste(year, month), "%Y %m")) %>% 
  mutate(season_exact = case_when(Date >= "2018-09-01" & Date <= "2018-11-30" ~ "Spring 2018",
                                  Date >= "2018-12-01" & Date <= "2019-02-28" ~ "Summer 2018/2019", 
                                  Date >= "2019-03-01" & Date <= "2019-05-31" ~ "Autumn 2019", 
                                  Date >= "2019-06-01" & Date <= "2019-08-31" ~ "Winter 2019", 
                                  Date >= "2019-09-01" & Date <= "2019-11-30" ~ "Spring 2019",
                                  Date >= "2019-12-01" & Date <= "2019-12-31" ~ "Summer 2019/2020",
                                  TRUE ~ "Other")) %>%
  group_by(season_exact, Date, Classification) %>% 
  summarise(meanC = mean(meanCells, na.rm = TRUE)) %>% 
  filter(Classification != "Dictyochophyceae") %>% 
  ggplot(., aes(x = Date, y = meanC, col = season_exact)) +
  geom_line() +
  facet_wrap(~Classification, ncol = 1, scales = "free")


month.rel$season_exact <- factor(month.rel$season_exact, levels = c("Spring 2018", 
                                                          "Summer 2018/2019", 
                                                          "Autumn 2019", 
                                                          "Winter 2019", 
                                                          "Spring 2019", 
                                                          "Summer 2019/2020"))









Sp_only %>% 
  filter(Species == "Chaetoceros sp.") %>% 
  group_by(months, month, year) %>% 
  summarise(monthlyMean = mean(meanCells, na.rm = TRUE), 
            monthlyN = n(),
            monthlySD = sd(meanCells, na.rm = TRUE), 
            monthlySE = sd(meanCells, na.rm = TRUE)/sqrt(n())) %>% 
  view()






