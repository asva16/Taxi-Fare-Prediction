library(tidyverse)
library(tidymodels)
library(bonsai)
library(lubridate)
library(timetk)
library(doParallel)
library(finetune)
library(parsnip)
library(treesnip)
library(doParallel)
theme_set(theme_minimal())
options(tidymodels.dark = TRUE)

taxi_ft_train = my_recipe %>% bake(new_data = NULL) %>%
  mutate_if(is.integer, as.numeric)
str(taxi_ft_train)
taxi_ft_test = my_recipe %>% bake(new_data = testing(taxi_split)) %>%
  mutate_if(is.integer, as.numeric)
str(taxi_ft_test)
taxi_ft_train_folds = taxi_ft_train %>% vfold_cv(v=10)
basic_recipe = recipe(fare_amount~., data = taxi_ft_train) %>%
  step_nzv(all_predictors()) %>% prep()

lightgbm = boost_tree(mtry = tune(), trees = tune(), tree_depth = tune(), 
                 learn_rate = tune(), min_n = tune(), loss_reduction = tune()) %>% 
  set_engine("lightgbm") %>% 
  set_mode("regression")

finalize(mtry(), taxi_ft_train)

lightgbm_wflow <-
  workflow() %>%
  add_model(lightgbm) %>%
  add_recipe(basic_recipe)

ctrl_bo <- control_bayes(verbose = TRUE, no_improve = 10L, save_workflow = FALSE, 
                              parallel_over = 'everything', save_pred = FALSE)

gbm_param = lightgbm_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(mtry = mtry(range = c(1, NCOL(taxi_ft_train))))

all_cores <- parallel::detectCores(logical = FALSE) - 1
registerDoParallel(cores = all_cores)

lightgbm_bo = lightgbm_wflow %>% 
  tune_bayes(
    resamples = taxi_ft_train_folds,
    metrics = yardstick::metric_set(rmse, rsq),
    iter = 50,
    control = ctrl_bo,
    initial = 50,
    param_info = gbm_param
  )

show_best(lightgbm_bo) # rmse 3.997943
collect_metrics(lightgbm_bo)
autoplot(lightgbm_bo)
autoplot(lightgbm_bo, type = 'performance')

ctrl_sa <- control_sim_anneal(verbose = TRUE, no_improve = 10L, save_workflow = FALSE, 
                         parallel_over = 'everything', save_pred = FALSE)

lightgbm_sa = lightgbm_wflow %>% 
  tune_sim_anneal(
    resamples = taxi_ft_train_folds,
    metrics = yardstick::metric_set(rmse, rsq),
    iter = 50,
    control = ctrl_sa,
    initial = 50,
    param_info = gbm_param
  )

show_best(lightgbm_sa) # rmse 3.966641
collect_metrics(lightgbm_sa) 
autoplot(lightgbm_sa)
autoplot(lightgbm_sa, type = 'parameters') #“marginals”, “parameters”, “performance”

########## no feature engineering
no_ft_recipe = recipe(fare_amount~., data = taxi_ft_train) %>%
  step_nzv(all_predictors()) %>%
  step_rm(distance, key_year:key_wday_x_key_hour) %>%
  prep()
no_ft_recipe %>% bake(new_data = taxi_ft_train)
no_fe_wflow <-
  workflow() %>%
  add_model(lightgbm) %>%
  add_recipe(no_ft_recipe)

gbm_param = no_fe_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(mtry = mtry(range = c(1, 5)))

# hyperparameter tuning
no_fe_sa = no_fe_wflow %>% 
  tune_sim_anneal(
    resamples = taxi_ft_train_folds,
    metrics = yardstick::metric_set(rmse, rsq),
    iter = 50,
    control = ctrl_sa,
    initial = 50,
    param_info = gbm_param
  )

show_best(no_fe_sa) # rmse 4.4866
collect_metrics(no_fe_sa) %>% filter(.metric=='rmse') %>% arrange(mean)
autoplot(no_fe_sa)
autoplot(no_fe_sa, type = 'parameters') #“marginals”, “parameters”, “performance”

no_fe_rmse = show_best(no_fe_sa) %>% slice_head() %>% select(mean)
fe_rmse = show_best(lightgbm_sa) %>% slice_head() %>% select(mean)
(no_fe_rmse-fe_rmse)/no_fe_rmse # 11.5% improv

#
predict(lightgbm_sa, taxi_ft_train %>% slice_head(n=10))
lightgbm_best_param = lightgbm_sa %>%
  select_best(metric='rmse')

lightgbm_best_wf=workflow() %>%
  add_recipe(basic_recipe) %>%
  add_model(lightgbm) %>% 
  finalize_workflow(lightgbm_best_param)

lightgbm_best_model = fit(lightgbm_best_wf, taxi_ft_train)

predict(lightgbm_best_model, head(taxi_ft_test))
pred_func(lightgbm_best_model, head(taxi_ft_test))

pred_func <- function(model, newdata) {
  predict(model, newdata) %>% unlist() %>% as.numeric()
}

explainer = DALEXtra::explain_tidymodels(
  model = lightgbm_best_model,
  data = taxi_ft_train %>% dplyr::select(-fare_amount) %>% as.data.frame(),
  y = taxi_ft_train %>% dplyr::select(fare_amount) %>% unlist() %>% as.numeric(),
  predict_function = pred_func,
  residual_function = res_func,
  label = 'LightGBM')

modelStudio::modelStudio(explainer, parallel = TRUE, 
                         new_observation = head(taxi_ft_test, n=100) %>% as.data.frame(),
                         new_observation_y = head(taxi_ft_test$fare_amount, n=100))

fi_lightgbm <- ingredients::feature_importance(x = explainer, type = "difference")
plot(fi_lightgbm) # distance, year, and geographical coordinate are the most importance feature
bd_lightgbm = breakDown::break_down(explainer, taxi_ft_train[12, ]) 
plot(bd_lightgbm) # distance, coordinate,and year contribute the most
bdu_lightgbm = iBreakDown::break_down_uncertainty(explainer, taxi_ft_train[12, ])
plot(bdu_lightgbm) # if break_down tells us about each var estimate, break_down_uncertainty explains the variance of each var
# the coordinate vars have the highest variance
pdp_lightgbm_distance <- ingredients::partial_dependency(explainer, variables = "distance")
plot(pdp_lightgbm_distance) # long distance equal expensive fare, generally
pdp_lightgbm_year <- ingredients::partial_dependency(explainer, variables = "key_year")
plot(pdp_lightgbm_year) # the average fare prediction increase as the year moves
pdp_lightgbm_do_lat <- ingredients::partial_dependency(explainer, variables = "dropoff_latitude")
plot(pdp_lightgbm_do_lat) # as we move to the north, we estimate that the fare increase
summary(taxi_ft_train$dropoff_latitude)
pdp_lightgbm_do_long <- ingredients::partial_dependency(explainer, variables = "dropoff_longitude")
plot(pdp_lightgbm_do_long) # -120 longitude is between nevada and california, if we exclude this, perhaps dropoff longitude would has lower contribution
plot(pdp_lightgbm_distance, pdp_lightgbm_year, pdp_lightgbm_do_lat, pdp_lightgbm_do_long)

plot(ingredients::partial_dependence(explainer, variables = 'passenger_count'))
cp_lightgbm = ingredients::ceteris_paribus(explainer, taxi_ft_train[12, ])
plot(cp_lightgbm, variables='distance')
