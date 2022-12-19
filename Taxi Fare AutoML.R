library(data.table) # fastest way to read csv files
library(tidyverse)
library(tidymodels)
library(timetk)
library(lubridate)
library(ggdist)
library(leaflet)
library(maps)
library(geosphere)
library(MASS)
library(agua)
library(bundle)
library(DALEX)
library(DALEXtra)
theme_set(theme_minimal())

taxi = fread("train.csv") %>%
  sample_n(10^5)

# feature engineering
taxi = taxi %>%
  mutate('year' = year(pickup_datetime),
         'month' = month(pickup_datetime, label = T),
         'week' = week(pickup_datetime),
         'day' = day(pickup_datetime),
         'wday' = wday(pickup_datetime, label = T),
         'hour' = hour(pickup_datetime),
         'minute' = minute(pickup_datetime))
taxi %>% slice_head(n=10)

# remove weird and unusual value of coordinate
taxi = taxi %>%
  filter(pickup_longitude<(0-10) & pickup_longitude>-360 & pickup_latitude>10 & pickup_latitude<360 &
           dropoff_longitude<(0-10) & dropoff_longitude>-360 & dropoff_latitude>10 & dropoff_latitude<360)
taxi %>% select(pickup_longitude:dropoff_latitude) %>% summary()

# calculate distance between two geographical coordinates
for (i in 1:NROW(taxi)) {
  taxi$distance[i] = distm(c(taxi$pickup_longitude[i], taxi$pickup_latitude[i]), 
                        c(taxi$dropoff_longitude[i], taxi$dropoff_latitude[i]),
                        fun=distHaversine)
}

# create a interactive map plot
taxi1000 = sample_n(taxi,1000) %>%
  arrange(fare_amount)
mytext <- paste(
  "Fare: ", taxi1000$fare_amount) %>%
  lapply(htmltools::HTML)

mypalette = colorNumeric(palette = 'viridis',
                         domain = taxi1000$fare_amount, 
                         reverse = T)

m <- leaflet(taxi1000) %>% 
  addTiles()  %>% 
  setView(lat=median(taxi1000$pickup_latitude), lng=median(taxi1000$pickup_longitude) , zoom=10) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~pickup_longitude, ~pickup_latitude, 
                   fillColor = ~mypalette(fare_amount), fillOpacity = 0.7, color="white", radius=8, 
                   stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~fare_amount, opacity=0.9, title = "Fare Amount", 
             position = "bottomright" ) %>%
  addControl('Pickup Location', position = "topright")
m

n <- leaflet(taxi1000) %>% 
  addTiles()  %>% 
  setView( lat=median(taxi1000$dropoff_latitude), lng=median(taxi1000$dropoff_longitude) , zoom=10) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~dropoff_longitude, ~dropoff_latitude, 
                   fillColor = ~mypalette(fare_amount), fillOpacity = 0.7, color="white", radius=8, 
                   stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~fare_amount, opacity=0.9, title = "Fare Amount", 
             position = "bottomright" ) %>%
  addControl('Dropoff Location', position = "topright")

leafsync::sync(m,n, ncol = 1)

library(htmlwidgets)
saveWidget(leafsync::sync(m,n, ncol = 1), file=paste0(getwd(),"Taxi Pickup and Dropoff.html"))

taxi %>%
  mutate(pickup_datetime = date(pickup_datetime)) %>%
  plot_seasonal_diagnostics(pickup_datetime, (fare_amount))
# using log is better for visualization since it can remove outliers and value of 0
# the only noticeable median difference is found in year variable, the median fare increase from 7.7 at 2011 to 9.5 at 2013  

taxi %>% 
  ggplot(aes(x=as.factor(hour), y=fare_amount)) +
  geom_boxplot() + labs(x='Hour', y='Fare')
taxi %>% slice_min(order_by = fare_amount, n=5) 
# negative fare, what a data, we should remove it before modelling
# 0 value of fare will be preserved because I don't know whether it was a discounted fare or not

taxi %>% 
  ggplot(aes(x=as.factor(day), y=fare_amount)) +
  geom_boxplot() + labs(x='Day', y='Fare')

taxi %>% 
  select(fare_amount, wday, hour) %>%
  mutate(hour = as.factor(hour)) %>%
  group_by(wday, hour) %>%
  summarise('median_fare' = median(fare_amount)) %>%
  ggplot(aes(x=wday, y=hour, fill=median_fare, label=ifelse(median_fare>10, median_fare, ""))) +
  geom_tile() +
  geom_text() +
  scale_fill_viridis_c()
# so the median fare is higher than 10 in the morning, mostly between 3:00 to 5:00
# lets check the taxi trips which happen in 1:00, 3:00, and 4:00 on monday
taxi %>%
  filter(wday=='Mon') %>%
  filter(hour %in% c(1,3,4)) %>%
  summary()
# nothing wrong here

taxi %>% 
  select(distance, wday, hour) %>%
  mutate(hour = as.factor(hour)) %>%
  group_by(wday, hour) %>%
  summarise('median_distance' = median(distance/1000)) %>%
  ggplot(aes(x=wday, y=hour, fill=median_distance, 
             label=ifelse(median_distance>3, round(median_distance,3), ""))) +
  geom_tile() +
  geom_text() + 
  scale_fill_viridis_c() +
  theme(legend.position = 'top')
# again, the trip as far as 3 km or more only happened between 1:00 and 5:00

# let's merge these two information
high_dist_fare = taxi %>% 
  select(fare_amount, distance, wday, hour) %>%
  mutate(hour = as.factor(hour)) %>%
  group_by(wday, hour) %>%
  summarise('median_fare' = median(fare_amount),
            'median_distance' = median(distance/1000)) %>%
  filter(median_fare>10 & median_distance>3)
# so, in these days and time, the median fare and the median distance traveled are higher than usual
# is there some places to visit at time like this?

pickup_density = taxi %>% 
  mutate(hour = as.factor(hour)) %>%
  inner_join(high_dist_fare, by=c('wday', 'hour')) %>%
  ggplot(aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  xlim(-74.025, -73.9) +
  ylim(40.69, 40.84) +
  labs(x='Longitude', y='Latitude', title = 'Taxi Pickup Location')
# it is found that there are two centers of pickup location

dropoff_density = taxi %>% 
  mutate(hour = as.factor(hour)) %>%
  inner_join(high_dist_fare, by=c('wday', 'hour')) %>%
  ggplot(aes(x=dropoff_longitude, y=dropoff_latitude)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  xlim(-74.025, -73.9) +
  ylim(40.69, 40.84) +
  labs(x='Longitude', y='Latitude', title = 'Taxi Dropoff Location')
# only one center of drop-off location
patchwork::wrap_plots(pickup_density, dropoff_density, guides = 'collect')

# let's find the location with the highest density
pickup_kde2d = kde2d(taxi$pickup_longitude, taxi$pickup_latitude, n = 50, 
                     lims = c(-74.025, -73.9, 40.69, 40.84))
image(pickup_kde2d)
which(pickup_kde2d$z == max(pickup_kde2d$z), arr.ind = TRUE) # row 14 and col 21
pickup_kde2d$x[which(pickup_kde2d$z == max(pickup_kde2d$z), arr.ind = TRUE)[1]]
pickup_kde2d$y[which(pickup_kde2d$z == max(pickup_kde2d$z), arr.ind = TRUE)[2]]
# it is W 34th St & 7th Avenue, Manhattan, NY, a lot of pickup and drop-off happened in this area

dropoff_kde2d = kde2d(taxi$dropoff_longitude, taxi$dropoff_latitude, n = 50, 
                     lims = c(-74.025, -73.9, 40.69, 40.84))
image(dropoff_kde2d)
which(dropoff_kde2d$z == max(dropoff_kde2d$z), arr.ind = TRUE) # row 14 and col 21
dropoff_kde2d$x[which(dropoff_kde2d$z == max(dropoff_kde2d$z), arr.ind = TRUE)[1]]
dropoff_kde2d$y[which(dropoff_kde2d$z == max(dropoff_kde2d$z), arr.ind = TRUE)[2]]

taxi %>% 
  mutate(distance = distance/1000) %>%
  ggplot(aes(x = distance, y = fare_amount)) +
  geom_point()
# some outliers in distance
North_America = map_data("world") %>% filter(region=="USA" | region=='Canada' | region == 'Greenland')

taxi %>% slice_max(order_by = distance, n=5) %>%
  ggplot(aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_point() +
  geom_segment(aes(xend=dropoff_longitude, yend=dropoff_latitude), 
               arrow = arrow(length = unit(0.1,"cm"))) +
  geom_polygon(data = North_America, aes(x=long, y = lat, group = group), fill="grey", alpha=.3) +
  coord_map(xlim = c(-100,-25), ylim = c(40,70)) + 
  labs(x = 'Longitude', y='Latitude', title = 'Taxi Destination', subtitle = 'Arrow shows the destination')
# we got 2 nonsense informations: 
# 1. someone who ride a taxi from greenland to new york but the fare is only 14.5
# 2. someone who ride a taxi to minnesota while the fare is only 9.7

taxi %>% 
  mutate(distance = distance/1000) %>%
  ggplot(aes(x = distance, y = fare_amount)) +
  geom_point() + xlim(0, 300)
# no linear correlation found

### so far, no correlation found between independent variables and the dependent var

# re-import the data so that we can apply feature engineering at the recipe 
taxi = fread("train.csv") %>%
  sample_n(10^5)

taxi = taxi %>%
  filter(pickup_longitude<(0-10) & pickup_longitude>-360 & pickup_latitude>10 & pickup_latitude<360 &
           dropoff_longitude<(0-10) & dropoff_longitude>-360 & dropoff_latitude>10 & 
           dropoff_latitude<360 & fare_amount >= 0)

for (i in 1:NROW(taxi)) {
  taxi$distance[i] = distm(c(taxi$pickup_longitude[i], taxi$pickup_latitude[i]), 
                           c(taxi$dropoff_longitude[i], taxi$dropoff_latitude[i]),
                           fun=distHaversine)
}

taxi_split <- initial_split(taxi, prop = .8)
taxi_folds = training(taxi_split) %>% vfold_cv(v=10)

# create recipe
str(taxi)
my_recipe_1 = recipe(fare_amount~., data = training(taxi_split)) %>%
  step_rm(key, pickup_datetime, distance) %>%
  step_nzv(all_predictors()) %>%
  prep()
my_recipe_1 %>% bake(new_data=NULL)

my_recipe_2 = recipe(fare_amount~., data = training(taxi_split)) %>%
  step_timeseries_signature(key) %>%
  step_rm(key, pickup_datetime, key_index.num, contains('iso'), contains('xts'), contains('lbl'), 
          key_minute:key_am.pm,
          key_mday:key_yday, key_week2:key_mday7) %>%
  step_interact(terms = ~key_wday:key_hour) %>%
  prep()
my_recipe %>% bake(new_data = NULL)

# AutoML
h2o::h2o.init(nthreads = 14, max_mem_size = '12g')
taxi_auto <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 1800, seed = 11, nfolds=10, sort_metric = 'RMSE', 
             stopping_metric = "AUTO") %>%
  set_mode("regression")

taxi_wflow_1 <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_recipe(my_recipe_1)

set.seed(11)
taxi_fit_1 <- fit(taxi_wflow_1, data = training(taxi_split))
taxi_fit_1_bundle = bundle(taxi_fit_1)
#taxi_fit_1 = unbundle(taxi_fit_1_bundle)

taxi_fit_1_info = taxi_fit_1[["fit"]][["fit"]][["fit"]]@leader

taxi_fit_1_rank_results = rank_results(taxi_fit_1)

 taxi_fit_1_rank_results %>% 
  filter(.metric == "rmse") %>%
  arrange(rank)

taxi_fit_1_collect_metrics = collect_metrics(taxi_fit_1, summarize = FALSE)
collect_metrics(taxi_fit_1, summarize = T) %>% filter(.metric == 'rmse' & algorithm == 'stacking')

taxi_fit_1_autoplot_rmse = autoplot(taxi_fit_1, type = "rank", metric = 'rmse') +
  theme(legend.position = "none")

taxi_fit_1_autoplot_rank = autoplot(taxi_fit_1, type = "rank") +
  theme(legend.position = "none")

taxi_fit_1_autoplot_metric = autoplot(taxi_fit_1, type = "metric") +
  theme(legend.position = "none")

taxi_fit_1_member_weights = taxi_fit_1 %>%
  extract_fit_parsnip() %>%
  member_weights() %>%
  unnest(importance) 

taxi_fit_1_member_weights %>%
  filter(type == "scaled_importance") %>%
  ggplot() +
  geom_boxplot(aes(value, algorithm)) +
  geom_jitter(aes(value, algorithm), alpha=.5) +
  scale_x_sqrt() +
  labs(y = NULL, x = "scaled importance", title = "Member importance in stacked ensembles")

id_1 = taxi_fit_1[["fit"]][["fit"]][["fit"]]@leader@model_id

taxi_pred_train = augment(taxi_fit, training(taxi_split)) %>%
  mutate('res' = fare_amount-.pred)
median(abs(taxi_pred_train$.pred-taxi_pred_train$fare_amount))
taxi_pred_test = augment(taxi_fit, testing(taxi_split))
rmse(taxi_pred_test, truth = fare_amount, estimate = .pred)

taxi_wflow_2 <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_recipe(my_recipe_2)

set.seed(11)
taxi_fit_2 <- fit(taxi_wflow_2, data = training(taxi_split))
taxi_fit_2_bundle = bundle(taxi_fit_2)
#taxi_fit_2 = unbundle(taxi_fit_2_bundle)

taxi_fit_2_info = taxi_fit_2[["fit"]][["fit"]][["fit"]]@leader
improv = (taxi_fit_1_info@model$cross_validation_metrics@metrics$RMSE-taxi_fit_2_info@model$cross_validation_metrics@metrics$RMSE)/taxi_fit_1_info@model$cross_validation_metrics@metrics$RMSE 
scales::percent(improv)
taxi_fit_2_rank_results = rank_results(taxi_fit_2)

taxi_fit_2_rank_results %>% 
  filter(.metric == "rmse") %>%
  arrange(rank)

taxi_fit_2_collect_metrics = collect_metrics(taxi_fit_2, summarize = FALSE)
collect_metrics(taxi_fit_2, summarize = T) %>% filter(.metric == 'rmse' & algorithm == 'stacking')

taxi_fit_2_autoplot_rmse = autoplot(taxi_fit_2, type = "rank", metric = 'rmse') +
  theme(legend.position = "none")

taxi_fit_2_autoplot_rank = autoplot(taxi_fit_2, type = "rank") +
  theme(legend.position = "none")

taxi_fit_2_autoplot_metric = autoplot(taxi_fit_2, type = "metric") +
  theme(legend.position = "none")

taxi_fit_2_member_weights = taxi_fit_2 %>%
  extract_fit_parsnip() %>%
  member_weights() %>%
  unnest(importance) 

taxi_fit_2_member_weights %>%
  filter(type == "scaled_importance") %>%
  ggplot() +
  geom_boxplot(aes(value, algorithm)) +
  geom_jitter(aes(value, algorithm), alpha=.5) +
  scale_x_sqrt() +
  labs(y = NULL, x = "scaled importance", title = "Member importance in stacked ensembles")


# Testing phase
test <- fread("D:/My Scripts/Github/Taxi Fare Prediction/test.csv")
for (i in 1:NROW(test)) {
  test$distance[i] = distm(c(test$pickup_longitude[i], test$pickup_latitude[i]), 
                           c(test$dropoff_longitude[i], test$dropoff_latitude[i]),
                           fun=distHaversine)
}
test_submission = augment(taxi_fit_2, test)
openxlsx::write.xlsx(data.frame(key = test_submission$key, test_submission$.pred), 'automl sub.xlsx')

summary(predict(taxi_fit_2, training(taxi_split)) - y)


taxi_ft_train = my_recipe %>% bake(new_data = NULL)
taxi_ft_test = my_recipe %>% bake(new_data = testing(taxi_split))
taxi_ft_wflow <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_formula(fare_amount~.)
set.seed(11)
taxi_ft_fit <- fit(taxi_ft_wflow, data = taxi_ft_train)
predict(taxi_ft_fit, head(taxi_ft_test))

pred_func <- function(model, newdata) {
  predict(model, newdata) %>% unlist() %>% as.numeric()
}
pred_func(taxi_fit, head(testing(taxi_split)))

res_func = function(model, newdata, y, predict_function = pred_func) {
  y - predict_function(model, newdata)
}
res_func(taxi_fit, head(testing(taxi_split)), 
         testing(taxi_split) %>% select(fare_amount) %>% head() %>% unlist() %>% as.numeric())

explainer = DALEXtra::explain_h2o(
  model = taxi_fit_2,
  data = training(taxi_split) %>% select(-fare_amount) %>% slice_head(n=20000),
  y = training(taxi_split) %>% select(fare_amount) %>% slice_head(n=20000),
  predict_function = pred_func,
  residual_function = res_func,
  label = id)

modelStudio::modelStudio(explainer, parallel = TRUE, 
                         new_observation = head(taxi_ft_test),
                         new_observation_y = head(taxi_ft_test$fare_amount))
# https://htmlpreview.github.io/?https://github.com/ModelOriented/DALEX-docs/blob/master/vignettes/DALEX_h2o_automl.html
## dont use recipe to engineer new features
fi_automl <- ingredients::feature_importance(x = explainer, type = "difference")
plot(fi_automl) # distance, year, and geographical coordinate are the most importance feature
bd_automl = breakDown::break_down(explainer, training(taxi_split) %>% slice(12)) 
plot(bd_automl) # distance, coordinate,and year contribute the most
bdu_automl = iBreakDown::break_down_uncertainty(explainer, taxi_ft_train[12, ])
plot(bdu_automl)

h2o::h2o.shutdown()


