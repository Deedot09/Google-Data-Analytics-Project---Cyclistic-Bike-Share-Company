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

```r
install.packages("tidyverse")
install.packages("lubridate")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
```

```r
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

# Variables will be assigned to the datasets for easy storage and presentation

```
```r
q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
```

```r
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv") 
```


```r
# ===================================================
# STEP 3: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
# ===================================================

# Compare column names each of the files

# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

colnames(q1_2019)

colnames(q1_2020)
```

```r
# While cleaning the raw data in Excel, I noticed the column names were not consistent.
# Hence, to make them consistent I will use the rename function

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

```r
# Inspect the dataframes and look for incongruencies using the str() function

str(q1_2019)
str(q1_2020)

```

```r
# Convert ride_id and rideable_type to character so that they can stack correctly

q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))
                  
```

```r
# Combine both data frames by stacking individual quarter's data frames into one big data frame

all_trips <- bind_rows(q1_2019,q1_2020)
```
```r
# Remove the lat, long, birthyear, and gender fields as these data were dropped in 2020

all_trips <- all_trips %>% 
  select(-c(start_lat,start_lng,end_lat,end_lng,birthyear,gender,"tripduration"))
```

```r
# =====================================================
# STEP 4: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# =====================================================

# Before analysis, I need to inspect the new table that has been created

# List of column names
colnames(all_trips)

# How many rows are in the data frame?
nrow(all_trips)

# Dimensions of the data frames?
dim(all_trips)

# See the first 6 rows of data frame. Also, tail(all_trips)
head(all_trips)

# See list of columns and data types (numeric, character, etc)
str(all_trips)

# Statistical summary of data. Mainly for numerics
summary(all_trips)
```

```
# There are a few problems I need to fix:

# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). I will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. I will add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) I will add a calculated field for the length of ride since the 2020Q1 data did not have the "tripduration" column. I will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. I will delete these rides.

# Let's Begin
# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... I will make the dataframe consistent with their current terminology.
# Note: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level.
```

```r
# First, let's see how many observations fall under each usertype

table(all_trips$member_casual)

# Reassign to the desired values (let's go with the current 2020 labels)

all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were assigned 

table(all_trips$member_casual)
```

```r
# Secondly, add columns that list the date, month, day, and year of each ride

# This will allow me to aggregate ride data for each month, day, or year ... before completing these operations I could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
```

```r
# Thirdly, add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)
 'data.frame':    791956 obs. of  15 variables:
  $ ride_id           : chr  "21742443" "21742444" "21742445" "21742446" ...
  $ started_at        : chr  "2019-01-01 0:04:37" "2019-01-01 0:08:13" "2019-01-01 0:13:23" "2019-01-01 0:13:45" ...
  $ ended_at          : chr  "2019-01-01 0:11:07" "2019-01-01 0:15:34" "2019-01-01 0:27:12" "2019-01-01 0:43:28" ...
  $ rideable_type     : chr  "2167" "4386" "1524" "252" ...
  $ start_station_id  : int  199 44 15 123 173 98 98 211 150 268 ...
  $ start_station_name: chr  "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
  $ end_station_id    : int  84 624 644 176 35 49 49 142 148 141 ...
  $ end_station_name  : chr  "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
  $ member_casual     : chr  "member" "member" "member" "member" ...
  $ date              : Date, format: "2019-01-01" "2019-01-01" ...
  $ month             : chr  "01" "01" "01" "01" ...
  $ day               : chr  "01" "01" "01" "01" ...
  $ year              : chr  "2019" "2019" "2019" "2019" ...
  $ day_of_week       : chr  "Tuesday" "Tuesday" "Tuesday" "Tuesday" ...
  $ ride_length       : 'difftime' num  390 441 829 1783 ...
   ..- attr(*, "units")= chr "secs"

# Convert "ride_length" calculation to all_trips (in seconds)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
```

```r
# Lastly, remove "bad" data

# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# I will create a new version of the dataframe (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
```

```r
# ====================================
# STEP 5: CONDUCT DESCRIPTIVE ANALYSIS
# ====================================

# Descriptive analysis on ride_length (all figures in seconds)

# straight average (total ride / rides)
mean(all_trips_v2$ride_length)

# midpoint number in the ascending array of ride lengths
median(all_trips_v2$ride_length)

# longest ride
max(all_trips_v2$ride_length)

# shortest ride
min(all_trips_v2$ride_length)

# I will condense the four lines above to one line using summary() on the specific attribute 
summary(all_trips_v2$ride_length)
```

```r
# Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```

```r
# I will find the average ride time by each day for member vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# I noticed the days of the week are out of order, so I will fix it.

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by eac day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyse ridership data by type and weekday

all_trips_v2 %>% 
  mutate(weekday =wday(started_at,label = TRUE)) %>%  
  group_by(member_casual,weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)
```

```r
# ===========================
# STEP 6: DATA VISUALIZATIONS
# ===========================

# First, I will visualize the number of rides by rider type

ggplot(data = all_trips_v2,mapping = aes(x = member_casual, fill= member_casual))+
  geom_bar() +
  labs(title = "User Count: Member vs Casual")


Observations

(1) Member riders have more rides than casual riders.
```

```r
# Secondly, I will visualize the number of rides of each rider type by day of the week

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x = weekday,y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge") +
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "By Weekday and Rider Type")


Observations

(1) Member riders have more rides than casual riders on both weekdays and weekends.

(2) Casual riders have a slightly higher number of rides during the weekends when compared to weekdays.

(3) Member riders have a significantly higher number of rides during the weekdays when compared to weekends.
```

```r
# Thirdly, I will create a visualization for the average duration

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "Average Duration")


Observations

(1) The average duration during the week is higher for casual riders in comparison to member riders.

(2) The average duration for member riders is slightly higher at weekends when compared to weekdays.

(3) The average duration for casual riders fluctuates through the day of the week with Thursday having the highest average duration for casual riders. 
```

```r
# Fourthly, I will visualize the number of rides by month

all_trips_v2 %>% 
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x = month,y = number_of_rides, fill = member_casual))+
  geom_col(position = "dodge")+
  labs(title = "Cyclistic -Total Rides: Jan - Mar 2019 & 2020", subtitle  = "By month and Rider Type")


Observations

(1) Top month for casual riders; March

(2) Top month for member riders; March

(3) For most months of the year, member riders have more total rides than casual riders
```

```r
# I would also visualize the top starting station booked by cyclistic members

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


Observations

(1) The top starting station for member riders is Canal & Adams St.

(2) The least starting starting station for member riders is Clinton St. & Lake St.
```

```r
# Next, visualize the top ending station booked by cyclistic members

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


Observations

(1) Member riders mostly end their trips at Canal & Adams st.

(2) Member riders seldom end their trips at LaSalle st &Jackson Blvd.
```

```r
# I would also do the same for Cyclistic casuals

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


Observations

(1) The top starting station for casual riders is Streeter Dr & Grand Ave.

(2) The least starting station for member riders is Lake Shore Dr & North Blvd.
```

```r
# Next, visualize the top ending station booked by cyclistic Casuals

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


Observations

(1) Member riders mostly end their trips at Streeter Dr & Grand Ave.

(2) Member riders seldom end their trips at Adler Planetarium.
```

```r
# ================================================
# STEP 7: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
# ================================================

# Create a CSV file to use on other presentation software


counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,FUN = mean)

write.csv(counts, file ='avg_ride_length.csv')
```
