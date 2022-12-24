# Taxi-Fare-Prediction
Predicting the fare amount (inclusive of tolls) for a taxi ride in New York City with some feature engineering

# Explanatory Data Analysis
## Feature Engineering
•	Extract date and time-based variables from datetime variable (key or pickup_datetime)

•	Haversine distance and driving distance (the 2nd distance uses Googla API) between two coordinates. Since calculating driving distance is not free, I only calculate the distance for 2500 sample and predict the rest. I did using linear regression to predict the rest of the driving distance, but it took a lot of memory to save, instead, I used AutoML which is far superior and took less space.

•	Fare-based variable from https://en.wikipedia.org/wiki/Taxis_of_New_York_City. I created base fare/initial fare, fare based on distance travelled, and Newark Airport trip.

## Remove Unusual Observation
We know that the taxi ride happened in New York, so the geographical coordinate of pickup and drop-off should match, more or less. There is obviously a chance that someone might ride a taxi to or from another city. At least if the coordinate does make sense, we shouldn’t remove the observations.  Google maps said that the latitude and longitude of New York is around 40.7 and -73.9, so any coordinate that deviates a lot from New York must be treated carefully. There are some observations that has weird coordinate, such as 0oN and 0oE (this is the coordinate of Null Island), beyond 360o, observation that located in Greenland, and observations that showed over the sea. The latter was more than half of whole observations. It's calming down that I was no longer have to deal with large data, thus large model. 

Another variable that needs our attention is fare. I thought that there is no way fare can be negative, so I removed those observations. However, there are a few observations that have zero value of taxi fare. My guess was the passenger got a discount or something, so I decided not to remove it.

## Visualization
Okay there is a linear pattern between driving distance (in meter) and haversine distance (in km). 50km haversine distance is around 75 km of driving distance which is make sense since we couldn’t always build a straight road.
![image](https://user-images.githubusercontent.com/48485276/209452775-d470b821-99ec-4ca9-a951-136494518e5f.png)

There is no significant pattern between coordinate (pickup and drop-off) with fare. The map above only had 1000 samples since it would take a lot of time to render 5M of observations.

![image](https://user-images.githubusercontent.com/48485276/209452788-ac44234a-7ea6-4e27-8cb6-a6e188f3803c.png)

This is our data before I remove all observations that were pointed over sea. This problem can be overcome by intersecting the coordinate points with a polygon map (such as .shp files). After cleaned up this mess, we got this plot instead. This is such as beautiful plot.
![image](https://user-images.githubusercontent.com/48485276/209452825-f4c08cc0-7eb9-4ea5-8e77-6815edae8fc6.png)

Okay this graphics is too small to see the pattern. It’s better to create the seasonal diagnostics by myself.
![image](https://user-images.githubusercontent.com/48485276/209452831-82b353d6-5938-4b87-a50d-d1f0193327a9.png)

This is an interval plot which tells the probability of the value of a certain data. For instance, generally, 50% of fare paid by customer is around 2-12. We can see that the median stays same (the black dot), the 80% interval is different at 5 in the morning, the 95% interval is higher from 4 to 7 and 13 to 17.

![image](https://user-images.githubusercontent.com/48485276/209452837-3350784e-5738-40c8-b457-9c921d8d2674.png)

The 1st and 2nd day has wider 95% interval of fare. I don't know whether the 1st day is Sunday or Monday. I didn’t bother checking because loading new data costs a lot of memory.

![image](https://user-images.githubusercontent.com/48485276/209452842-cd7de0e2-0058-4cad-b7c3-c3ea3501c798.png)

It is September that made the 3rd quarter have the widest 95% interval.
![image](https://user-images.githubusercontent.com/48485276/209452848-6ce2fd09-ccec-4a14-8a15-bf2bccaf4916.png)

The 1st quarter has the narrowest 95% interval that differs significantly from the others. However, if we measure difference in median, they all are the same.

![image](https://user-images.githubusercontent.com/48485276/209452850-16830c22-26d4-4d3e-92f0-5bbb6bf3096a.png)

There is a slight increase at 80% interval, steady increase at 95% from 2009 to 2012 followed by massive increase at 2013 and 2014. The interval pattern between 2014 and 2015 are about the same.

![image](https://user-images.githubusercontent.com/48485276/209452853-5f450b07-5a5f-4ec0-ab54-74859e84c2ce.png)

The median fare is significantly higher at 4 am 2nd day, 5am 1st day, and 4-5am 7th day while hit the lowest point at 6 am on 3rd to 5th day.
![image](https://user-images.githubusercontent.com/48485276/209452859-8fd56162-f153-49f5-ac33-dcb31a2bd9b6.png)

Distant trip happened between 4-5 am in 1st, 2nd, 3rd, and 7th day even though it was only between 2.7 and 2.9 km.
![image](https://user-images.githubusercontent.com/48485276/209452860-5cf1e817-759b-4341-b91b-2640e0b1c3b2.png)

Clearly my expected_fare is not the best one to predict fare_amount. Expected_fare was calculated by adding Newark trip fare and total fare per distance.

![image](https://user-images.githubusercontent.com/48485276/209452865-543f9945-e17b-42bf-a35e-f7d5c88eb85e.png)

# Modelling
I used AutoML again since I could decide how long the models trained themselves. This is the modelling result with 10-folds CV on feature engineered-data.

![image](https://user-images.githubusercontent.com/48485276/209452870-9cc88479-3c42-4177-a881-78143ff2ee5b.png)

Stacking algorithm dominated the leader board as expected. It was surprising that two gradient boosting achieved top 10 spot with 0.01 and 0.02 difference in rmse.

![image](https://user-images.githubusercontent.com/48485276/209452876-dc7dc578-e2c4-4b43-b4b1-475ea71e3d6a.png)

This is how the other models (base learners) competed against the others. Stacking is clearly the best, followed by gradient boosting and random forest. It seemed that neural nets were as bad as generalized linear model (glm).
Feature engineering increased my model performance by 12%. The best model, StackedEnsemble_AllModels_4 predict the testing data with around the same error as predicting the training data.

![image](https://user-images.githubusercontent.com/48485276/209452897-ecdb4555-22f7-47e0-9414-835d4a49eaa8.png)






