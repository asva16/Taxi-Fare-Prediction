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
library(maptools)
theme_set(theme_minimal())

taxi = fread("train.csv")

# calculate distance between two geographical coordinates
# result in km
haversine_dist = function(lat1, lon1, lat2, lon2) {
  p = pi/180
  a = 0.5 - cos((lat2-lat1)*p)/2 + cos(lat1*p) * cos(lat2*p) * (1-cos((lon2-lon1)*p))/2
  return(12742 * asin(sqrt(a))) #2*R*asin...
}
taxi$distance = haversine_dist(taxi$pickup_latitude, taxi$pickup_longitude, 
                               taxi$dropoff_latitude, taxi$dropoff_longitude)

summary(taxi$distance) # we have some NA's here
taxi = na.omit(taxi)
taxi = taxi %>% 
  filter(distance!=0)

origin_test = paste(taxi$pickup_latitude[1],taxi$pickup_longitude[1], sep = ',')
destination_test = paste(taxi$dropoff_latitude[1],taxi$dropoff_longitude[1], sep = ',')
driving_test = gmapsdistance::gmapsdistance(origin_test, 
                             destination_test, 
                             key = 'AIzaSyA1MgLuZuyqR_OGY3ob3M52N46TDBRI_9k')
driving_test$Distance
taxi$distance[1]

# remove coordinates outside NY
taxi = taxi %>%
  filter(pickup_longitude<(0-71.8) & pickup_longitude>-74.5 & pickup_latitude>40 & pickup_latitude<41.5 &
           dropoff_longitude<(0-71.8) & dropoff_longitude>-74.5 & dropoff_latitude>40 & dropoff_latitude<41.5)

# add driving distance in sample data
# result in meter, I think
taxi$driving_dist = NA
driving_id = sample(NROW(taxi), 2500)
for (i in driving_id) {
  origin_sample = paste(taxi$pickup_latitude[i],taxi$pickup_longitude[i], sep = ',')
  destination_sample = paste(taxi$dropoff_latitude[i],taxi$dropoff_longitude[i], sep = ',')
  driving_sample = gmapsdistance::gmapsdistance(origin_sample, 
                                                destination_sample, 
                                                key = 'AIzaSyA1MgLuZuyqR_OGY3ob3M52N46TDBRI_9k')
  taxi$driving_dist[i] = driving_sample$Distance
  }

# predict the rest of driving distance with automl since lm is so bad
taxi[driving_id,] %>%
  dplyr::select(distance, driving_dist) %>%
  ggplot(aes(driving_dist, distance)) +
  geom_point()
cor.test(taxi$driving_dist[driving_id], taxi$distance[driving_id])

h2o::h2o.init(nthreads = 14, max_mem_size = '8g')
taxi_auto <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 900, seed = 11, nfolds=10, sort_metric = 'RMSE', 
             stopping_metric = "AUTO") %>%
  set_mode("regression")

taxi_wflow <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_formula(driving_dist~distance+pickup_longitude+pickup_latitude+dropoff_longitude+dropoff_latitude)

driving_fit = fit(taxi_wflow, data = taxi[driving_id,])
driving_fit_bundle = bundle(driving_fit)
augment(driving_fit, taxi[driving_id,]) %>%
  metrics(.pred, driving_dist)

# split 'test' data to 5
NROW(taxi)/5
sample_test_1 = 1:10732892
sample_test_2 = (1:10732892)+(NROW(taxi)/5)
sample_test_3 = (1:10732892)+(NROW(taxi)/5)*2
sample_test_4 = (1:10732892)+(NROW(taxi)/5)*3
sample_test_5 = (1:10732892)+(NROW(taxi)/5)*4

taxi[-driving_id, c(4:7,9)] %>% head()
taxi$driving_dist[sample_test_5] = predict(driving_fit, taxi[sample_test_5, c(4:7,9)])
sum(is.na(taxi$driving_dist[sample_test_5]))
# remove after use to free memory
rm(list=c('sample_test_1','sample_test_2','sample_test_3','sample_test_4','sample_test_5'))

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

# Seasonal diagnostics
taxi %>%
  sample_n(10^5) %>%
  mutate(pickup_datetime = date(key)) %>%
  plot_seasonal_diagnostics(pickup_datetime, log(fare_amount))

# Remove extreme value of fare
taxi %>% 
  arrange(-fare_amount) %>%
  head(20) # ok but wtf is that amount (top 10 fares are more than 900)
# im gonna remove value of more than 1000
taxi %>%
  filter(fare_amount<1000)

# lets look at distance field
# filter distance_diff is distance_diff < 2*distance
# to make sure that driving_dist value is make sense
taxi = taxi_test
taxi = taxi %>%
  filter(pickup_longitude<(0-71.8) & pickup_longitude>-74.5 & pickup_latitude>40.5 & pickup_latitude<41.5 &
           dropoff_longitude<(0-71.8) & dropoff_longitude>-74.5 & dropoff_latitude>40.5 & dropoff_latitude<41.5) %>%
  arrange(-distance_diff)

North_America = map_data("world") %>% filter(region=="USA")

top_5_distance = taxi %>% head(20)

# I dont know how to filter these obs
top_5_distance %>%
  head(5) %>%
  ggplot(aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_point() +
  geom_segment(aes(xend=dropoff_longitude, yend=dropoff_latitude), 
               arrow = arrow(length = unit(0.1,"cm"))) +
  geom_polygon(data = North_America, aes(x=long, y = lat, group = group), fill="grey", alpha=.3) +
  coord_map(xlim = c(-74.5,-71.8), ylim = c(40,41.5)) + 
  labs(x = 'Longitude', y='Latitude', title = 'Top 5 Taxi Ride Distance', 
       subtitle = 'Arrow shows the destination')

# Given Lat and Lon identify if point over land or ocean
# but this is not perfect
data(wrld_simpl)
test_d = top_5_distance %>% dplyr::select(dropoff_longitude, dropoff_latitude)
test_p = top_5_distance %>% dplyr::select(pickup_longitude, pickup_latitude)
pts_d <- SpatialPoints(test_d, proj4string=CRS(proj4string(wrld_simpl)))
pts_p <- SpatialPoints(test_p, proj4string=CRS(proj4string(wrld_simpl)))
ii <- !is.na(over(pts_d, wrld_simpl)$FIPS)
iii = !is.na(over(pts_p, wrld_simpl)$FIPS)
data.frame(a=ii,b=iii) %>%
  mutate_if(is.logical, as.numeric) %>%
  filter(a==1 & b==1)
# ok the sample is done, time to apply to our data
# too much data, my 32 gb memory can't handle this
taxi = taxi %>% 
  sample_n(4*10^6)
test_d = taxi %>% dplyr::select(dropoff_longitude, dropoff_latitude)
test_p = taxi %>% dplyr::select(pickup_longitude, pickup_latitude)
pts_d <- SpatialPoints(test_d, proj4string=CRS(proj4string(wrld_simpl)))
pts_p <- SpatialPoints(test_p, proj4string=CRS(proj4string(wrld_simpl)))
ii <- !is.na(over(pts_d, wrld_simpl)$FIPS)
rm(list=c('test_d','pts_d'))
iii = !is.na(over(pts_p, wrld_simpl)$FIPS)
rm(list=c('test_p','pts_p'))
taxi = taxi %>%
  bind_cols(a=ii, b=iii) %>%
  mutate_if(is.logical, as.numeric) %>%
  filter(a==1 & b==1) # it reduced my data to more than half

#lets plot the distance map again
taxi %>%
  arrange(-distance_diff) %>%
  head(5) %>%
  ggplot(aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_point() +
  geom_segment(aes(xend=dropoff_longitude, yend=dropoff_latitude), 
               arrow = arrow(length = unit(0.1,"cm"))) +
  geom_polygon(data = North_America, aes(x=long, y = lat, group = group), fill="grey", alpha=.3) +
  coord_map(xlim = c(-74.5,-71.8), ylim = c(40,41.5)) + 
  labs(x = 'Longitude', y='Latitude', title = 'Top 5 Taxi Ride Distance', 
       subtitle = 'Arrow shows the destination') #this is great

# export taxi_test then remove it from environment
fwrite(taxi_test, 'taxi_halfway_preprocessing.csv') # 5.3 M obsr 11 vars exportec in 20 secs
# the csv file is stored in .rar
rm('taxi_test')
# now R uses 5 gb instead of 20 gb

# engineering time-based feature
taxi = recipe(fare_amount~., taxi) %>%
  step_timeseries_signature(key) %>%
  step_rm(key, pickup_datetime, key_index.num, contains('iso'), contains('xts'), contains('lbl'), 
          key_minute:key_am.pm,
          key_mday:key_yday, key_week2:key_mday7,
          a,b) %>%
  step_interact(terms = ~key_wday:key_hour) %>%
  prep() %>%
  juice()
rm('taxi_prep')  

str(taxi)
taxi = taxi %>%
  filter(fare_amount<1000 | fare_amount>=0)

# EDA time-based feature
taxi %>%
  ggplot(aes(as.factor(key_hour), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median")
## even though the median stays same, the 80% interval is different at 5 in the morning, 
## the 95% interval is higher for 4 to 7 and 13 to 17

taxi %>%
  ggplot(aes(as.factor(key_year), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median") +
  stat_halfeye(fill='grey80',
               adjust = .75,
               justification = -.1,
               .width = 0,
               point_color = NA,
               show.legend = F) +
  ylim(0,60)
## slight increase at 80% interval, steady increase at 95% from 2009 to 2012 
## followed by massive increase at 2013 and 2014. 
## the interval pattern between 2014 and 2015 are about the same

taxi %>%
  ggplot(aes(as.factor(key_quarter), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median") +
  stat_halfeye(fill='grey80',
               adjust = .75,
               justification = -.1,
               .width = 0,
               point_color = NA,
               show.legend = F) +
  ylim(0,60)
## 7.5% (95-80/2) observations are slightly higher at 3rd quarter

taxi %>%
  ggplot(aes(as.factor(key_month), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median")
## September made the 3rd quarter have the widest 95% interval

taxi %>%
  ggplot(aes(as.factor(key_wday), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median") +
  stat_halfeye(fill='grey80',
               adjust = .75,
               justification = -.1,
               .width = 0,
               point_color = NA,
               show.legend = F) +
  ylim(0,60)
## the 1st and 2nd day has wider 95% interval of fare
## I don't know whether the 1st day is sunday or monday, 
## I can check using the very initial data but it cost a lot of memory

taxi %>% 
  dplyr::select(fare_amount, key_wday, key_hour) %>%
  mutate(hour = as.factor(key_hour)) %>%
  group_by(key_wday, hour) %>%
  summarise('median_fare' = median(fare_amount)) %>%
  ggplot(aes(x=key_wday, y=hour, fill=median_fare, label=ifelse(median_fare>8.1, median_fare, ""))) +
  geom_tile() +
  geom_text() +
  scale_fill_viridis_c()
## the median fare is significantly higher at 4 am 2nd day, 5am 1st day, and 4-5am 7th day 

taxi %>% 
  dplyr::select(distance, key_wday, key_hour) %>%
  mutate(hour = as.factor(key_hour)) %>%
  group_by(key_wday, hour) %>%
  summarise('median_distance' = median(distance)) %>%
  ggplot(aes(x=key_wday, y=hour, fill=median_distance, 
             label=ifelse(median_distance>2.7, round(median_distance,3), ""))) +
  geom_tile() +
  geom_text() +
  scale_fill_viridis_c() +
  theme(legend.position = 'top')
## distant trip happened between 4-5 am in 1st, 2nd, 3rd, and 7th day

## add another fields based on fare from https://en.wikipedia.org/wiki/Taxis_of_New_York_City
taxi$base_fare = ifelse(taxi$key_hour<6 | taxi$key_hour>=20, 3,
                                 ifelse(taxi$key_hour<20 & taxi$key_hour>=16, 3.5, 2.5))

taxi %>%
  filter(taxi$pickup_latitude<=40.6580923 & taxi$pickup_latitude>=40.6380923 & 
           taxi$pickup_longitude>=-73.7970545 & taxi$pickup_longitude<=-73.7770545)
# 28,994 trips from JFA
JFA_pickup = ifelse(taxi$pickup_latitude<=40.6580923 & taxi$pickup_latitude>=40.6380923 & 
                      taxi$pickup_longitude>=-73.7970545 & taxi$pickup_longitude<=-73.7770545,1,0)

taxi %>%
  filter(taxi$dropoff_latitude<=40.6580923 & taxi$dropoff_latitude>=40.6380923 & 
           taxi$dropoff_longitude>=-73.7970545 & taxi$dropoff_longitude<=-73.7770545)
## 16886 trips to JFA
JFA_dropoff = ifelse(taxi$dropoff_latitude<=40.6580923 & taxi$dropoff_latitude>=40.6380923 & 
                       taxi$dropoff_longitude>=-73.7970545 & taxi$dropoff_longitude<=-73.7770545,1,0)
JFA_fare = ifelse(JFA_pickup + JFA_dropoff==2,1,JFA_pickup + JFA_dropoff)
JFA_fare = ifelse(JFA_fare==1,52,0)
taxi %>% 
  filter(taxi$dropoff_latitude<=40.7019076 & taxi$dropoff_latitude>=40.6819076 & 
           taxi$dropoff_longitude>=-74.1898102 & taxi$dropoff_longitude<=-74.1698102)
## 25 trips to Newark
Newark_fare = ifelse(taxi$dropoff_latitude<=40.7019076 & taxi$dropoff_latitude>=40.6819076 & 
                       taxi$dropoff_longitude>=-74.1898102 & taxi$dropoff_longitude<=-74.1698102,17.5,0)

taxi$additional_fare = ifelse(JFA_fare+Newark_fare>52, Newark_fare, JFA_fare+Newark_fare)
table(as.factor(taxi$additional_fare))
taxi %>%
  ggplot(aes(as.factor(additional_fare), fare_amount)) +
  stat_interval(alpha = 1, 
                .width = c(.5, .8, .95, .99),
                show.legend = NA) +
  stat_summary(
    geom = "point",
    fun = "median")

## I think I messed up the JFA fare, I'm not gonna included it
taxi$additional_fare = Newark_fare
total_fare_distance = taxi$driving_dist/1609.34*2.5
taxi$expected_fare = Newark_fare + total_fare_distance

taxi %>%
  dplyr::select(expected_fare, fare_amount) %>%
  ggplot(aes(expected_fare, fare_amount)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, col='red') +
  coord_obs_pred() # clearly it's not the best one to predict fare_amount
cor.test(taxi$expected_fare, taxi$fare_amount)

# Modelling time
taxi_split = taxi %>%
  initial_split()
taxi_train = training(taxi_split)
taxi_test = testing(taxi_split)
rm('taxi_split')

h2o::h2o.init(nthreads = -1, max_mem_size = '16g')
taxi_auto <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 7200, seed = 8, nfolds=10, sort_metric = 'RMSE', 
             stopping_metric = "AUTO") %>%
  set_mode("regression")

taxi_wflow <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_formula(fare_amount~.-distance_diff)

taxi_fit = fit(taxi_wflow, data = taxi_train)
taxi_fit_bundle = bundle(taxi_fit)

taxi_fit_info = taxi_fit[["fit"]][["fit"]][["fit"]]@leader

taxi_fit_rank_results = rank_results(taxi_fit)

taxi_fit_rank_results %>% 
  filter(.metric == "rmse") %>%
  arrange(rank)

taxi_fit_collect_metrics = collect_metrics(taxi_fit, summarize = FALSE)
taxi_fit_collect_metrics %>%
  group_by(id, algorithm, .metric) %>%
  summarise('mean'=mean(.estimate),
            'sd' = sd(.estimate)) %>%
  filter(.metric=='rmse') %>%
  arrange(mean)

taxi_fit_autoplot_rmse = autoplot(taxi_fit, type = "rank", metric = 'rmse') +
  theme(legend.position = "none")

taxi_fit_autoplot_rank = autoplot(taxi_fit, type = "rank") +
  theme(legend.position = "none")

taxi_fit_autoplot_metric = autoplot(taxi_fit, type = "metric") +
  theme(legend.position = "none")

taxi_fit_member_weights = taxi_fit %>%
  extract_fit_parsnip() %>%
  member_weights() %>%
  unnest(importance) 

taxi_fit_member_weights %>%
  filter(type == "scaled_importance") %>%
  ggplot() +
  geom_boxplot(aes(value, algorithm)) +
  geom_jitter(aes(value, algorithm), alpha=.5) +
  scale_x_sqrt() +
  labs(y = NULL, x = "scaled importance", title = "Member importance in stacked ensembles")

id = taxi_fit[["fit"]][["fit"]][["fit"]]@leader@model_id

taxi_pred_train = augment(taxi_fit, taxi_train) %>%
  mutate('res' = fare_amount-.pred) 
taxi_pred_train %>% metrics(fare_amount, .pred)
ggplot(taxi_pred_train, aes(.pred, fare_amount)) +
  geom_point() +
  geom_abline(intercept = 0, slope=1, col='red') +
  coord_obs_pred() # can't handle outliers

taxi_pred_test = augment(taxi_fit, taxi_test) %>%
  mutate('res' = fare_amount-.pred) 
taxi_pred_test %>% metrics(fare_amount, .pred)

h2o::h2o.init(nthreads = -1, max_mem_size = '16g')
no_ft_wflow <-
  workflow() %>%
  add_model(taxi_auto) %>%
  add_formula(fare_amount~pickup_longitude+pickup_latitude+dropoff_longitude+dropoff_latitude+passenger_count)

no_ft_fit = fit(no_ft_wflow, data = taxi_train)
no_ft_fit_bundle = bundle(no_ft_fit)
no_ft_fit_info = no_ft_fit[["fit"]][["fit"]][["fit"]]@leader
no_ft_fit_rank_results = rank_results(no_ft_fit)
no_ft_rmse = no_ft_fit_rank_results %>% 
  filter(.metric=='rmse') %>%
  slice(1) %>%
  dplyr::select(mean)
with_ft_rmse = taxi_fit_rank_results %>% 
  filter(.metric=='rmse') %>%
  slice(1) %>%
  dplyr::select(mean)
(no_ft_rmse-with_ft_rmse)/no_ft_rmse


h2o::h2o.shutdown()

