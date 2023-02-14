#### Loading Data ####

install.packages(tidyverse)
install.packages("ggthemes")
library(tidyverse) ## load tidyverse ##
library(scales)
library(ggthemes)

##Loading .csv data into Rstudio##

nov21 <- read_csv("Desktop/cyclist_data/2021-11-divvy-tripdata.csv")

dec21 <- read_csv("Desktop/cyclist_data/2021-12-divvy-tripdata.csv")

jan22 <- read_csv("Desktop/cyclist_data/2022-01-divvy-tripdata.csv")

feb22 <- read_csv("Desktop/cyclist_data/2022-02-divvy-tripdata.csv")

mar22 <- read_csv("Desktop/cyclist_data/2022-03-divvy-tripdata.csv")

apr22 <- read_csv("Desktop/cyclist_data/2022-04-divvy-tripdata.csv")

may22 <- read_csv("Desktop/cyclist_data/2022-05-divvy-tripdata.csv")

jun22 <- read_csv("Desktop/cyclist_data/2022-06-divvy-tripdata.csv")

jul22 <- read_csv("Desktop/cyclist_data/2022-07-divvy-tripdata.csv")

aug22 <- read_csv("Desktop/cyclist_data/2022-08-divvy-tripdata.csv")

sep22 <- read_csv("Desktop/cyclist_data/2022-09-divvy-tripdata.csv")

oct22 <- read_csv("Desktop/cyclist_data/2022-10-divvy-tripdata.csv")


## Find the unique values in columns ##

unique(nov21[c("member_casual")])

unique(nov21[c("rideable_type")])


## Explore differences in Station ID Names ##

start_id <- unique(dec21[c("start_station_id")])
  View(start_id)

end_id <- unique(dec21[c("end_station_id")])
  View(end_id)

start_locations <- unique(dec21[c("start_station_id", "start_station_name")])
view(start_locations)


## Combine data into one large data frame ##

trip_data <- bind_rows(nov21,dec21, jan22, feb22, mar22, apr22, may22, jun22, jul22, aug22, sep22, oct22)

#### Preparing and Cleaning Data ####

## Clean the data by seperating the month, day, week, and time into different columns ##

trip_data$date <- as.Date(trip_data$started_at)

trip_data$month <- format(as.Date(trip_data$date), "%m")

trip_data$day <- format(as.Date(trip_data$date), "%d")

trip_data$year <- format(as.Date(trip_data$date), "%Y")

trip_data$day_of_week <- format(as.Date(trip_data$date), "%A")

colnames(trip_data)


## Create a column that calculates the ride duration from the time stamps ##

trip_data$ride_duration <- difftime(trip_data$ended_at, trip_data$started_at)

trip_data$ride_duration <- as.numeric(as.character(trip_data$ride_duration))
is.numeric(trip_data$ride_duration) #checking to see if it is a numeric


## Remove ride durations with a negative or zero duration. ##

trip_data_clean <- trip_data[trip_data$ride_duration > 0, ]

## Remove Docked Bikes that look like they are being tested for long durations ##

trip_data_clean <- subset(trip_data_clean, rideable_type != "docked_bike")
unique(trip_data_clean$rideable_type) ## checking for removal ##


## Eliminate rides without an ending Lat/Lng. Many outliers here and could be lost/broken bikes ##
## I would want to check with stake holders before taking out of my data.##

trip_data_clean <- trip_data_clean[!with(trip_data_clean, is.na(end_lat) & is.na(end_lng)), ]


## Create column called wkend_wkday, so we can see if riders use bike more on weekends or weekdays ##
## Monday through Friday will be the Week, and Saturday/Sunday will be the weekend ##

trip_data_clean$wkend_wkday <- ifelse(trip_data_clean$day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday")

## Order Days of the Week ##

trip_data_clean$day_of_week <- ordered(trip_data_clean$day_of_week,
                                       levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

##### Analysis #####
## Questions to look into ##

## Find Average ride length, mean and median. ##

summary(trip_data_clean$ride_duration) ## Median 604 secs, Mean 918 secs ##


## Average ride length - Member vs Casual

aggregate(trip_data_clean$ride_duration, list(trip_data_clean$member_casual), FUN = mean)

##   Group.1         x
## 1  casual 1189.5807
## 2  member  745.3383

## Average ride length - Weekend vs Weekday

aggregate(trip_data_clean$ride_duration, list(trip_data_clean$wkend_wkday), FUN = mean)

##   Group.1         x
## 1 weekday  851.9034
## 2 weekend 1075.6832

## Average ride length - Member/Casual vs Wkend/Wkday

trip_data_clean %>% 
  aggregate(ride_duration ~ member_casual + wkend_wkday, FUN = mean, na.rm = TRUE) %>% 
  ggplot(aes(x=wkend_wkday, y=ride_duration, fill=member_casual)) + 
  geom_col(position = position_dodge(width = .75)) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  ylim(0, 1500) +
  labs(title="Average Ride Length vs Weekend or Weekday", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  xlab("Time of Week") + ylab("Ride Duration (sec)")
  
  


##     member_casual wkend_wkday ride_duration
## 1        casual     weekday     1101.7311
## 2        member     weekday      718.1264
## 3        casual     weekend     1339.3324
## 4        member     weekend      827.0357



## Number of Rides Member/Casual vs Wkend/Wkday ##

trip_data_clean %>% 
  count(member_casual, wkend_wkday) %>% 
  ggplot(aes(x=wkend_wkday, y=n, fill=member_casual)) + geom_col(position = position_dodge(width=.75)) + 
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  scale_y_continuous(limits = c(0,3000000), labels = scales::comma) +
  labs(title="Rider Type vs Time of Week", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  xlab("Time of Week") + ylab("Number of Rides")



##  member_casual wkend_wkday       n
##   <chr>         <chr>         <int>
## 1 casual        weekday     1366402
## 2 casual        weekend      801579
## 3 member        weekday     2551747
## 4 member        weekend      849941


## Number of rides per day of the week vs Member/Casual ##

trip_data_clean %>% 
  count(member_casual, day_of_week) %>% 
  group_by(day_of_week) %>% 
  ggplot(aes(x=day_of_week, y=n, fill=member_casual)) + geom_col(position = position_dodge2(width = 0.5)) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  scale_y_continuous(limits = c(0,600000), labels = scales::comma) +
  labs(title="Total Rides Per Day", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  xlab("Day of Week") + ylab("Number of Rides")

##differenet attempt##

trip_data_clean %>% 
  count(member_casual, day_of_week) %>% 
  group_by(day_of_week) %>% 
  ggplot(aes(x=day_of_week, y=n, fill=member_casual)) + geom_path(aes(color = member_casual), group = 1) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  scale_y_continuous(limits = c(0,600000), labels = scales::comma) +
  labs(title="Total Rides Per Day", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  xlab("Day of Week") + ylab("Number of Rides")



## When do casual/member riders month of year ride most ##

## Organized by "member_casual" ##
trip_data_clean %>% 
  group_by(member_casual)%>%
  count(month, member_casual) %>%
  ggplot(aes(x=month, y=n, fill=member_casual)) + geom_col(position = position_dodge(width = 0.75)) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  scale_y_continuous(limits = c(0,500000), labels = scales::comma) +
  labs(title="Total Rides Per Month", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  xlab("Month") + ylab("Number of Rides")


## member_casual month      n
## <chr>         <chr>  <int>
## 1 casual        01     17517
## 2 casual        02     20015
## 3 casual        03     81415
## 4 casual        04    114122
## 5 casual        05    253617
## 6 casual        06    337849
## 7 casual        07    374551
## 8 casual        08    332242
## 9 casual        09    276556
## 10 casual        10    196188
## 11 casual        11     99181
## 12 casual        12     64728
## 13 member        01     85219
## 14 member        02     94168
## 15 member        03    194122
## 16 member        04    244799
## 17 member        05    354351
## 18 member        06    400023
## 19 member        07    417323
## 20 member        08    426882
## 21 member        09    404511
## 22 member        10    349561
## 23 member        11    252960
## 24 member        12    177769

## Organized by "month" ##

trip_data_clean %>% 
  count(month, member_casual) %>%
  print(n=24)

## month member_casual      n
## <chr> <chr>          <int>
## 1 01    casual         17517
## 2 01    member         85219
## 3 02    casual         20015
## 4 02    member         94168
## 5 03    casual         81415
## 6 03    member        194122
## 7 04    casual        114122
## 8 04    member        244799
## 9 05    casual        253617
## 10 05    member        354351
## 11 06    casual        337849
## 12 06    member        400023
## 13 07    casual        374551
## 14 07    member        417323
## 15 08    casual        332242
## 16 08    member        426882
## 17 09    casual        276556
## 18 09    member        404511
## 19 10    casual        196188
## 20 10    member        349561
## 21 11    casual         99181
## 22 11    member        252960
## 23 12    casual         64728
## 24 12    member        177769



## What types of bikes do casual/members use most ##

trip_data_clean %>% 
  count(rideable_type, member_casual) %>% 
  ggplot(aes(x=rideable_type, y=n, fill=member_casual)) + geom_col(position = position_dodge()) +
  theme_fivethirtyeight() +
  scale_fill_manual(values = fivethirtyeight_pal()(3)) +
  scale_color_manual(values = fivethirtyeight_pal()(3)) +
  scale_y_continuous(limits = c(0,2000000), labels = scales::comma) +
  labs(title="Types of Bikes Riden", 
       subtitle="Organized by Rider Type", 
       caption="Source: Divvy Bike Dataset") +
  labs(fill = "Rider Type") +
  ylab("Bike Type") + xlab("Number of Rides")


## rideable_type member_casual       n
## <chr>         <chr>           <int>
## 1 classic_bike  casual         894802
## 2 classic_bike  member        1739741
## 3 electric_bike casual        1273179
## 4 electric_bike member        1661947
