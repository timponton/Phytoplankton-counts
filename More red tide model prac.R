
library(caret)
library(glmnet)
library(tidymodels)






##

##### subset Primary sump####


in_rt <- PS %>% 
  group_by(Date, Time) %>% 
  summarise(totalCells = sum(Cells.L)) %>% 
  #summarise(totalCells = mean(time_total, na.rm = TRUE)) %>% 
  mutate(RedTide = ifelse(totalCells <= 1000, "None detected", 
                          ifelse(totalCells >= 1001 & totalCells <= 5000, "Very Low (A)", 
                                 ifelse(totalCells >= 5001 & totalCells <= 10000, "Very Low (B)", 
                                        ifelse(totalCells >= 10001 & totalCells <= 50000, "Low (A)", 
                                               ifelse(totalCells >= 50001 & totalCells <= 100000, "Low (B)", 
                                                      ifelse(totalCells >= 100001 & totalCells <= 1000000, "Medium", "High"))))))) %>% 
  mutate(FWC_mod = ifelse(totalCells >= 1 & totalCells <= 1000, "Normal", 
                          ifelse(totalCells >= 1001 & totalCells <= 10000, "Very Low", 
                                 ifelse(totalCells >= 10001 & totalCells <= 100000, "Low", 
                                        ifelse(totalCells >= 100001 & totalCells < 1000000, "Medium", "High"))))) %>% 
  ungroup() %>% 
  group_by(Date, FWC_mod, RedTide) %>% 
  summarise(totalCells = max(totalCells))






PS <- df %>% 
  filter(Site == "Primary sump") %>% 
  filter(Species != "Diatom")

### Summarise to daily counts####

dailyMeans <- PS %>% 
  filter(year != "2020") %>% 
  group_by(Date, months, month, year, Season, YEarMonth, SeasonOrder) %>% 
  summarise(meanBiomass = mean(Cells.L, na.rm = TRUE)) %>% 
  mutate(log1Mean = log1p(meanBiomass)) %>% 
  mutate(whatDay = weekdays(Date))

# Wrangle data ------------------------------------------------------------

## Species biomass per day
# summarise to daily means for each species
Sp_only <- PS %>% 
  filter(year != "2020") %>% 
  filter(Classification != "Various") %>% 
  group_by(Date, Species, Classification, months, month, year, 
           YEarMonth, Season, SeasonOrder, RedTide, FWC_mod) %>% 
  summarise(meanCells = mean(Cells.L, na.rm = TRUE))

##



set.seed(123)



red <- Sp_only %>% 
  ungroup() %>% 
  mutate(meanCells = standardize(meanCells)) %>% 
  pivot_wider(names_from = "Species", values_from = "meanCells", 
              values_fill = list(meanCells = 0)) %>% 
  select(-Date, -Classification, -year, 
         -months, -month, -YEarMonth, 
         -Season, -SeasonOrder, -FWC_mod) %>% 
  mutate(rt_class = if_else(RedTide == "High" | 
                               RedTide == "Medium" | 
                               RedTide == "Low (B)" |
                               RedTide == "Low (A)", "High","Low")) %>% 
  mutate(rt_class = as.factor(rt_class)) %>% 
  select(rt_class, everything()) %>% 
  select(-RedTide)




red_split <- initial_split(red, strata = rt_class)
red_train <- training(red_split)
red_test <- testing(red_split)


x <- model.matrix(rt_class ~ ., red_train)[, -1]
y <- ifelse(red_train$rt_class == "High", 1, 0)

cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)


# Fit the final model on the training data
model <- cv.glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda)

coef(model)

x.test <- model.matrix(rt_class ~ ., red_test)[, -1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "High", "Low")
# Model accuracy
observed.classes <- red_test$rt_class
mean(predicted.classes == observed.classes)

coef(cv.lasso, cv.lasso$lambda.min)




lasso.model <- cv.glmnet(x, y, alpha = 0, family = "binomial",
                      lambda = cv.lasso$lambda)

x.test <- model.matrix(rt_class ~ ., red_test)[, -1]
probabilities <- lasso.model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "High", "Low")
# Model accuracy
observed.classes <- red_test$rt_class
mean(predicted.classes == observed.classes)



# Fit the model

full.model <- glm(rt_class ~ ., data = red_train, family = binomial)
# Make predictions
probabilities <- full.model %>% predict(red_test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "High", "Low")
# Model accuracy
observed.classes <- red_test$rt_class
mean(predicted.classes == observed.classes)



