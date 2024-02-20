```
# ===================================================
# STEP 1: INSTALL THE REQUIRED PACKAGES AND LIBRARIES
# ===================================================

# First package will be the tidyverse,as it will help to import,transform, explore and visualize it.

# lubridate -> date functions
# skimr and janitor -> data cleaning
# dplyr -> data manipulation
# ggplot2 -> visualization
```

```{r}
install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
```

```{r}
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(ggplot2)
library(dplyr)
```

```
# =====================
# STEP 2: COLLECT DATA
# =====================

# Next, the divvy tripdata datasets will be uploaded

Variables will be assigned to the datasets for easy storage and presentation

```
```{r}
q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
```

```{r}
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv") 
```



**# ===================================================**

**# STEP 3: WRANGLE DATA AND COMBINE INTO A SINGLE FILE**

**# ===================================================**



#### Compare column names each of the files


While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

```{r}
colnames(q1_2019)
```

```{r}
colnames(q1_2020)
```


While cleaning the raw data in Excel, I noticed the column names were not consistent.
Hence, to make them consistent I will use the rename function

```{r}
q1_2019 <- rename(q1_2019,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)
```


#### Inspect the dataframes and look for incongruencies using the str() function

```{r}
str(q1_2019)
str(q1_2020)

```


#### Convert ride_id and rideable_type to character so that they can stack correctly

```{r}
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))
                  
```


#### Combine both data frames by stacking individual quarter's data frames into one big data frame

```{r}
all_trips <- bind_rows(q1_2019,q1_2020)
```

#### Remove the lat, long, birthyear, and gender fields as these data were dropped in 2020


```{r}
all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,"tripduration"))
```



**# =====================================================**

**# STEP 4: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS**

**# =====================================================**



### Before analysis, I need to inspect the new table that has been created


List of column names

```{r}
colnames(all_trips)
```

How many rows are in the data frame?

```{r}
nrow(all_trips)
```

Dimensions of the data frames?

```{r}
dim(all_trips)
```

See the first 6 rows of data frame. Also, tail(all_trips)

```{r}
head(all_trips)
```

See list of columns and data types (numeric, character, etc)

```{r}
str(all_trips)
```

Statistical summary of data. Mainly for numerics

```{r}
summary(all_trips)
```





#### There are few problems I need to fix:

(1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual rider ("Customer" and "casual"). I will need to consolidate that from four to two labels.
(2) The data can only be aggregated at the ride-level, which is too granular. I will add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
(3) I will add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column.I will add "ride_length" to the entire dataframe for consistency.
(4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. I will delete these rides.

#### Let's Begin

In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"

Before 2020, Divvy used different labels for these two types of riders ... I will make the dataframe consistent with their current nomenclature.

*Note: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level.*

##### First, let's see how many observations fall under each usertype

```{r}
table(all_trips$member_casual)
```

Reassign to the desired values (let's go with the the current 2020 labels)

```{r}
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))
```

Check to make sure the proper number of observations were assigned 

```{r}
table(all_trips$member_casual)
```

##### Secondly, add columns that list the date, month, day, and year of each ride

This will allow me to aggregate ride data for each month, day, or year ... before completing these operations I could only aggregate at the ride level

```{r}
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
```

##### Thirdly, add a "ride_length" calculation to all_trips (in seconds)

```{r}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
```

Inspect the structure of the columns

```{r}
str(all_trips)
```

Convert "ride_length" calculation to all_trips (in seconds)

```{r}
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

##### Lastly, remove "bad" data

##### The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative

I will create a new version of the dataframe (v2) since data is being removed

```{r}
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```



**# ====================================**

**# STEP 5: CONDUCT DESCRIPTIVE ANALYSIS**

**# ====================================**



#### Descriptive analysis on ride_length (all figures in seconds)

straight average (total ride / rides)

```{r}
mean(all_trips_v2$ride_length)
```

midpoint number in the ascending array of ride lengths

```{r}
median(all_trips_v2$ride_length)
```

longest ride

```{r}
max(all_trips_v2$ride_length)
```

shortest ride

```{r}
min(all_trips_v2$ride_length)
```



##### I will condense the four lines above to one line using summary() on the specific attribute 

```{r}
summary(all_trips_v2$ride_length)
```

#### Compare members and casual users

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
```

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
```

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
```

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

I will find the average ride time by each day for member vs casual users

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

I noticed the days of the week are out of order, so I will fix it.

```{r}
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

Now, let's run the average ride time by eac day for members vs casual users

```{r}
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```

Analyse ridership data by type and weekday

```{r}
all_trips_v2 %>% 
  mutate(weekday =wday(started_at,label = TRUE)) %>%  
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)
```



**# ===========================**

**# STEP 6: DATA VISUALIZATIONS**

**# ===========================**


##### First, I will visualize the number of rides by rider type

```{r}
ggplot(data = all_trips_v2,mapping = aes(x = member_casual, fill= member_casual))+
  geom_bar() +
  labs(title = "User Count: Member vs Casual")
```

**Observations:**

(1) Member riders have more rides than casual riders.


##### Secondly, I will visualize the number of rides of each rider type by day of the week

```{r}
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday,y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "By Weekday and Rider Type")
```

**Observations:**

(1) Member riders have more rides than casual riders both weekdays and weekends.

(2) Casual riders have slightly higher number of rides during the weekends when compared to weekdays.

(3) Member riders have significantly higher number of rides during the weekdays when compared to weekends.


##### Thirdly, I will create a visualization for average duration

```{r}
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "Average Duration")
```

**Observations:**

(1) The average duration during the week is higher for casual riders in comparison to member riders.

(2) The average duration for member riders is slightly higher at weekends when compared to weekdays.

(3) The average duration for casual riders fluctuates through the day of the week with Thursday having the highest average duration for casual riders. 


##### Fourthly, I will visualize the number of rides by month

```{r}
all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month,y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "By month and Rider Type")
```

**Observations:**

(1) Top month for casual riders:March

(2) Top month for member riders: March

(3) For most months of the year, member riders have more total rides than casual riders

##### I would also visualize the top starting station booked by cyclistic members

```{r}
all_trips_v2 %>% 
  filter(!is.na(start_station_name)) %>% 
  filter(member_casual == "member") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()
            ,average_ride_length = mean(ride_length)
            ,average_ride_min = mean(ride_length) / 60) %>% 
  arrange(-number_of_rides) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle = "Top Station Booked by Cyclistic Members") +
  coord_flip()+
  theme_minimal()
```


**Observations:**

(1) The top starting station for member riders is Canal & Adams st.

(2) The least starting starting station for member riders is Clinton st. & Lake st.


##### Next, visualize the top ending station booked by cyclistic members

```{r}
all_trips_v2 %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(member_casual == "member") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()
            ,average_ride_length = mean(ride_length)
            ,average_ride_min = mean(ride_length) / 60) %>% 
  arrange(-number_of_rides) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(end_station_name, number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle = "Top Station Booked by Cyclistic Members") +
  coord_flip()+
  theme_minimal()
```

**Observations:**

(1) Member riders mostly end their trips at Canal & Adams st.

(2) Member riders seldom end their trips at LaSalle st &Jackson Blvd.


##### I would also do the same for Cyclistic casuals

```{r}
all_trips_v2 %>% 
  filter(!is.na(start_station_name)) %>% 
  filter(member_casual == "casual") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_rides = n()
            ,average_ride_length = mean(ride_length)
            ,average_ride_min = mean(ride_length) / 60) %>% 
  arrange(-number_of_rides) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle = "Top Station Booked by Cyclistic Members") +
  coord_flip()+
  theme_minimal()
```

**Observations:**

(1) The top starting station for casual riders is Streeter Dr & Grand Ave.

(2) The least starting starting station for member riders is Lake shore Dr & North Blvd.


##### Next, visualize the top ending station booked by cyclistic Casuals

```{r}
all_trips_v2 %>% 
  filter(!is.na(end_station_name)) %>% 
  filter(member_casual == "casual") %>% 
  group_by(end_station_name) %>% 
  summarise(number_of_rides = n()
            ,average_ride_length = mean(ride_length)
            ,average_ride_min = mean(ride_length) / 60) %>% 
  arrange(-number_of_rides) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(end_station_name, number_of_rides), y = number_of_rides)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle = "Top Station Booked by Cyclistic Members") +
  coord_flip()+
  theme_minimal()
```

**Observations:**

(1) Member riders mostly end their trips at Streeter Dr & Grand Ave.

(2) Member riders seldom end their trips at Adler Planetarium.



**# ================================================**

**# STEP 7: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS**

**# ================================================**


### Create a csv file to use on other presentation software

```{r}
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN = mean)
```

```{r}
write.csv(counts, file ='avg_ride_length.csv')
```
