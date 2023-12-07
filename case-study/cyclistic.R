# installed the 'tidyverse', 'ggplot2', and 'lubridate' packages
library(tidyverse)
library(ggplot2)
library(lubridate)
# Check the current working directory
getwd()

# Set and confirm the the working Directory
setwd("/cloud/project/case-study")
getwd()

# Step 1(COLLECT DATA)
# Get the datasets to work with
download.file(url = "https://raw.githubusercontent.com/dnmuasya/Case_Study_1/main/202004-divvy-tripdata.csv?token=GHSAT0AAAAAACLCPPZFTHV7RLRQNYX5NZKCZLO6YHQ",
              destfile = "data_raw/202004-divvy-tripdata.csv")

download.file(url = "https://raw.githubusercontent.com/dnmuasya/Case_Study_1/main/202012-divvy-tripdata.csv?token=GHSAT0AAAAAACLCPPZEYFDEK7JPBVJ5SR5KZLO63VA",
              destfile = "data_raw/202012-divvy-tripdata.csv")

download.file(url = "https://raw.githubusercontent.com/dnmuasya/Case_Study_1/main/202101-divvy-tripdata.csv?token=GHSAT0AAAAAACLCPPZEGOKOUUDE4SKEWE4KZLO66LA",
              destfile = "data_raw/202101-divvy-tripdata.csv")

# Download the data
data1 <- read_csv("data_raw/202004-divvy-tripdata.csv")
data2 <- read_csv("data_raw/202012-divvy-tripdata.csv")
data3 <- read_csv("data_raw/202101-divvy-tripdata.csv")

# Get a glimpse of the dataframes
head(data1)
head(data2)
head(data3)
# Inspect the dataframes
View(data1)
View(data2)
View(data3)
dim(data1)
dim(data2)
dim(data3)
str(data1)

# Misceleneous - interacting with the data frames
data1$member_casual <- factor(data1$member_casual)
summary(data1$member_casual)
data2$member_casual <- factor(data2$member_casual)
summary(data2$member_casual)
data3$member_casual <- factor(data3$member_casual)
summary(data3$member_casual)
nlevels(data3$member_casual)
# Plotting data3 using factors for members and casual users
plot(data3$member_casual)

member_casual <- data3$member_casual
levels(member_casual)
member_casual <- addNA(member_casual)
levels(member_casual)
head(member_casual)
levels(member_casual)[3] <- "undertermined"
levels(member_casual)
plot(member_casual)
str(data1, "column_id")
summary(data1$started_at)
summary(data2$started_at)
summary(data3$started_at)
data1 <- read_csv("data_raw/202004-divvy-tripdata.csv")
str(data1)
str(data2)
str(data3)
View(data1)
colnames(data1)
colnames(data2)
colnames(data3)

# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE DATA FRAME
# Make the column names to be consistent before merging into one dataframe
data1 <- mutate(data1, ride_id = as.character(ride_id),
                rideable_type = as.character(rideable_type),
                start_station_id = as.character(start_station_id),
                end_station_id = as.character(end_station_id))

data2 <- mutate(data2, ride_id = as.character(ride_id),
                rideable_type = as.character(rideable_type),
                start_station_id = as.character(start_station_id),
                end_station_id = as.character(end_station_id))

data3 <- mutate(data3, ride_id = as.character(ride_id),
                rideable_type = as.character(rideable_type),
                start_station_id = as.character(start_station_id),
                end_station_id = as.character(end_station_id))

# Merge into one big dataframe to manipulate
all_trips <- bind_rows(data1, data2, data3)
summary(all_trips)
str(all_trips)
# Remove the unnecessary columns
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
# Confirm the columns are removed
str(all_trips)
head(all_trips)

# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Inspect the new data frame
colnames(all_trips) # All columns
nrow(all_trips)  #All rows
dim(all_trips)  # data frame dimension
head(all_trips)
str(all_trips)
levels(all_trips$member_casual)
all_trips <- all_trips %>%
  mutate(member_casual = factor(member_casual))
nlevels(all_trips$member_casual)

# Each user type observation
table(all_trips$member_casual)
# Add columns that list date: month, day and year of each ride
# This will allow us to aggregate ride data for each month, day, 
# or year ... before completing these operations we 
# could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at) # the default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
# Confirm the columns have been added
str(all_trips)
colnames(all_trips)

# Add a 'ride_length' calculation to all trips (in seconds): end - start time
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
# Confirm the column has been added
str(all_trips)
colnames(all_trips)

# Convert 'ride_length from 'Factor' to 'Numeric' so we can run calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove 'bad' data - where 'ride_length' was negative and the entries when
# bikes were taken for quality check by DIVVY -indicated by 
# start_station_name == "HQ QR"
# We create a version 2 to do the analysis since some data is being removed
# NOTE: DONT FORGET THE COMMA AT THE END OF ARRAY
# We need clean data for correct analysis
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]
str(all_trips_v2) # Confirms that some data has been deleted
str(all_trips)  # Compare against the cleaned data - all_trips_v2

# STEP 4 : CONDUCT DESCRIPTIVE ANALYSIS
# Descriptve analysis on ride_length
mean(all_trips_v2$ride_length, na.rm = TRUE) # Remove the null values(na.rm=TRUE)
median(all_trips_v2$ride_length, na.rm = TRUE) # Midpoint number of ride_length
max(all_trips_v2$ride_length, na.rm = TRUE) # Longest ride in seconds
min(all_trips_v2$ride_length, na.rm = TRUE) # shortest ride in seconds

# Gives a descriptive summary of the ride_length column(condenses lines 152-155 above)
summary(all_trips_v2$ride_length)

# Compare statistics of casual users vs members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels = c("Sunday","Monday", "Tuesday",
                                               "Wednesday", "Thursday",
                                               "Friday", "Saturday"))
# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
            all_trips_v2$day_of_week, FUN = mean)
# The output indicates casual users have the highest ride_length everyday

# Ridership data by type and weekday analysis
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE))%>% #creates weekday field using wday() function
  group_by(member_casual,weekday)%>% #groups by user-type and weekday
  summarise(number_of_rides = n(), #calculates the number of rides
            average_duration = mean(ride_length))%>% # and average duration
  arrange(member_casual, weekday) #sorts by user-type
# The output indicates casual users have less rides with more duration- seasonal
# while the members have more rides with less duration - frequent riders

# Let us visualize the number of rides by rider-type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Rides by rider-type")
ggsave("fig/weekly_number_of_riders.png")
# Indicates saturday and sunday casual riders are many compared to other days
# Members ride frequently but probably for short distances

# Visualisation for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Average Duration for each rider type")
ggsave("fig/Average_duration.png")
# Casual users have the highest average weekly duration 
# Could be casual riders ride for longer distances and more time but less use


# STEP 5: Exporting Summary file for further analysis
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write_csv(counts, file = "data/average_ride_length.csv")