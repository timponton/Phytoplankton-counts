
### Predicting red tide cases using species counts ###
library(rpart)
library(rpart.plot)
library(tune)
library(tidyverse)
library(ggplot2)
library(tidymodels)



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

decision_tree(mode = "classification", cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart")


group_by(Date, months, month, year) %>% 
  summarise(totalCells = mean(time_total, na.rm = TRUE))
group_by(Date) %>% 
  filter(max())


ggplot(.,aes(x = reorder(months, month), y = daily_mean, fill = year)) +
  geom_boxplot() +
  geom_smooth(method = "loess") +
  scale_y_log10()


##### New try


set.seed(123)

red_split <- initial_split(z, strata = FWC_mod)
red_train <- training(red_split)
red_test <- testing(red_split)

red_rec <- recipe(FWC_mod ~ ., data = red_train) %>% 
  #step_downsample(FWC_mod) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

test_proc <- bake(red_rec, new_data = red_test)

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")
  
tree_fit  <- tree_spec %>%
  fit(FWC_mod ~ ., data = juice(red_rec))

tree_fit$fit

juice(red_rec) %>% 
  count(FWC_mod)

rpart.plot(tree_fit$fit, roundint=FALSE)

rp_classi <- decision_tree(mode = "classification") %>% 
  set_engine("rpart")

red_wf <- workflow() %>% 
  add_recipe(red_rec) %>% 
  add_model(rp_classi)

s <- red_wf %>% 
  fit(red_test)

rpart.plot(s$fit)



red_kknn <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

kknn_fit <- red_kknn %>% 
  fit(FWC_mod ~ ., data = juice(red_rec))


kknn_fit

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit  <- tree_spec %>%
  fit(FWC_mod ~ ., data = juice(red_rec))

set.seed(1234)
validation_splits <- mc_cv(juice(red_rec), prop = 0.9, strata = FWC_mod)

validation_splits

knn_result <- fit_resamples(
  red_kknn,
  FWC_mod ~ ., 
  validation_splits, 
  control = control_resamples(save_pred = TRUE)
)

knn_result %>% 
  collect_metrics()


knn_result %>% 
  unnest(.predictions) %>%
  mutate(model = "kknn") %>% 
  group_by(model) %>% 
  roc_curve(FWC_mod, .pred_class) %>% 
  autoplot()


tree_result <- fit_resamples(
  FWC_mod ~ ., 
  tree_spec,
  validation_splits, 
  control = control_resamples(save_pred = TRUE)
)

tree_result %>% 
  collect_metrics()




#######



rt_class

set.seed(1234)

red_split <- initial_split(red, strata = rt_class)
red_train <- training(red_split)
red_test <- testing(red_split)

red_rec <- recipe(rt_class ~ ., data = red_train) %>% 
  step_downsample(rt_class) %>% 
  step_zv(all_numeric()) %>% 
  step_normalize(all_numeric()) %>% 
  prep()

test_proc <- bake(red_rec, new_data = red_test)

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit  <- tree_spec %>%
  fit(rt_class ~ ., data = juice(red_rec))

tree_fit$fit
rpart.plot(tree_fit$fit)

juice(red_rec) %>% 
  count(rt_class)

rpart.plot(tree_fit$fit, roundint=FALSE)

rp_classi <- decision_tree(mode = "classification") %>% 
  set_engine("rpart")

red_wf <- workflow() %>% 
  add_recipe(red_rec) %>% 
  add_model(rp_classi)

s <- red_wf %>% 
  fit(red_test)

rpart.plot(s$fit)



red_kknn <- nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

kknn_fit <- red_kknn %>% 
  fit(rt_class ~ ., data = juice(red_rec))


kknn_fit

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit  <- tree_spec %>%
  fit(rt_class ~ ., data = juice(red_rec))

set.seed(1234)
validation_splits <- mc_cv(juice(red_rec), prop = 0.9, strata = rt_class)

validation_splits

knn_result <- fit_resamples(
  red_kknn,
  rt_class ~ ., 
  validation_splits, 
  control = control_resamples(save_pred = TRUE)
)

knn_result %>% 
  collect_metrics()

knn_result %>% 
  unnest(.metrics)

knn_result %>% 
  unnest(.predictions) %>%
  #mutate(model = "kknn") %>% 
  #group_by(model) %>% 
  roc_curve(rt_class, .pred_High) %>% 
  autoplot()

knn_result %>% 
  unnest(.predictions) %>%
  #mutate(model = "kknn") %>% 
  #group_by(model) %>% 
  conf_mat(rt_class, .pred_class) %>% 
  autoplot(type = "heatmap")

kknn_fit %>% 
  predict(new_data = test_proc, type = "prob") %>% 
  mutate(truth = red_test$rt_class) %>% 
  roc_auc(truth, .pred_High)



tree_result <- fit_resamples(
  tree_spec,
  rt_class ~ ., 
  validation_splits, 
  control = control_resamples(save_pred = TRUE)
)

tree_result %>% 
  collect_metrics()
