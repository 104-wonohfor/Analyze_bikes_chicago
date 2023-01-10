q2_2019 <- read.csv('bike_chicago/Divvy_Trips_2019_Q2.csv')
q3_2019 <- read.csv('bike_chicago/Divvy_Trips_2019_Q3.csv')
q4_2019 <- read.csv('bike_chicago/Divvy_Trips_2019_Q4.csv')
q1_2020 <- read.csv('bike_chicago/Divvy_Trips_2020_Q1.csv')

View(q2_2019)
View(q3_2019)
View(q4_2019)
View(q1_2020)


(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID" 
                   ,rideable_type = "X01...Rental.Details.Bike.ID" 
                   ,started_at = "X01...Rental.Details.Local.Start.Time" 
                   ,ended_at = "X01...Rental.Details.Local.End.Time" 
                   ,start_station_name = "X03...Rental.Start.Station.Name" 
                   ,start_station_id = "X03...Rental.Start.Station.ID"  
                   ,end_station_name = "X02...Rental.End.Station.Name"  
                   ,end_station_id = "X02...Rental.End.Station.ID"
                   ,member_casual = "User.Type"))

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 



all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

View(all_trips)

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, 
            "X01...Rental.Details.Duration.In.Seconds.Uncapped", 
            "X05...Member.Details.Member.Birthday.Year", "Member.Gender", "tripduration"))

table(all_trips$member_casual)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))

is.numeric(all_trips$ride_length)

table(all_trips$start_station_name)
table(all_trips$ride_length)

bad_data <- all_trips %>% select(start_station_name,ride_length) %>% filter(start_station_name == 'HQ QR' | ride_length<0)

all_trips_v2 <- all_trips[!(all_trips$start_station_name =='HQ QR' | all_trips$ride_length <0),]
View(all_trips_v2)

mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length)

test_table <- all_trips_v2 %>% arrange(ride_length)
View(test_table)

summary(all_trips_v2$ride_length)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)		

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) + 
  labs(title = 'Member vs Casual: Number of bike rides', subtitle = 'Customers of Lyft Bikes in Chicago')

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = 'Member vs Casual: Average Duration', subtitle = 'Customers of Lyft Bikes in Chicago')



avg_duration <- aggregate(all_trips_v2$ride_length ~ 
                            all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
number_of_rides <- aggregate(all_trips_v2$ride_length ~ 
                               all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = length)

write.csv(avg_duration, 'avg_duration.csv',row.names = FALSE)
write.csv(number_of_rides, 'number_of_rides.csv',row.names = FALSE)
write.csv(all_trips_v2, 'data_to_analyze.csv',row.names = FALSE)
