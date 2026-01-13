# Load core data manipulation packages
library(tidyverse)

# Load date/time manipulation package
library(lubridate)

# Merging 12 months of data into a single dataframe
trips_data <- list.files(
  path = "~/Projects/case_study_cyclist_using_R",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  map_df(~ read_csv(.))

saveRDS(trips_data, "all_trips.rds")

#we use rds file to improve efficiency and reduce knitting time. This file is load in R markdown setup

all_trips <- readRDS("all_trips.rds")


#from now we will use alltrips

#overview of data 
glimpse(all_trips)

# Standardize to lowercase
all_trips <- all_trips %>%
  rename_with(tolower) 

#date and time format conversion 
all_trips <- all_trips %>%
  mutate(
    started_at = as.POSIXct(started_at),
    ended_at   = as.POSIXct(ended_at)
  )

# Identify missing values
colSums(is.na(all_trips))

# Remove records with missing station names
all_trips <- all_trips %>%
  filter(
    !is.na(start_station_name),
    !is.na(end_station_name)
  )

#removing duplicates
all_trips <- all_trips %>%
  distinct()

#unique users types 
unique(all_trips$member_casual)

#dropping selective columns 
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

#calculating ride length 
all_trips <- all_trips %>% 
  mutate(ride_length=difftime(ended_at,started_at,unit="mins"))

#day,week,month
all_trips <- all_trips %>%
  mutate(
    date = as.Date(started_at),
    month = format(date, "%m"),
    day = format(date, "%d"),
    year = format(date, "%Y"),
    day_of_week = format(date, "%A")
  )

#data validation
all_trips <- all_trips %>% 
  filter(ride_length > 0)

# Overall summary of ride_length (in minutes)
summary(as.numeric(all_trips$ride_length))

# Comparing mean, median, max, and min ride length by member type
all_trips %>% 
  group_by(member_casual) %>% 
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length),
    median_duration = median(ride_length),
    max_duration = max(ride_length)
  )

# Fixing the order of days for better analysis
all_trips$day_of_week <- ordered(all_trips$day_of_week, 
                                 levels=c("Sunday", "Monday", "Tuesday", 
                                          "Wednesday", "Thursday", "Friday", "Saturday"))

# Calculating average duration and number of rides by day of week
all_trips %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

# Analyzing usage trends by month
all_trips%>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)

#viz 1
all_trips %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Rides by Day: Members vs. Casual Riders",
    subtitle = "Members dominate weekdays, while casual riders peak on weekends",
    x = "Day of the Week", y = "Number of Rides",
    fill = "Rider Type"
  )+ scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme_minimal()

#viz 2
all_trips %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_duration = mean(as.numeric(ride_length)), .groups = 'drop') %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Trip Duration by Day",
    subtitle = "Casual riders spend significantly more time per ride",
    x = "Day of the Week", y = "Average Duration (Minutes)",
    fill = "Rider Type"
  ) +
  theme_classic()

#viz 3
all_trips %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Seasonal Trends: Monthly Ride Count",
    subtitle = "Ridership peaks in Summer (June–August)",
    x = "Month", y = "Total Rides",
    color = "Rider Type"
  )+ scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K")) +
  theme_light()

#the end