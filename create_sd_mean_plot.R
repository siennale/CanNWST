library(tidyverse)
library(snakecase)
library(lubridate)
library(zoo)

# create this file path with your data
file_name <- "data/2018/10/18/51516_1539820800_12178.csv"

game_start_time <- "00:14:44"
first_half_end  <- "01:00:00"
half_time_end   <- "01:15:42"
game_end        <- "02:04:43"

gps_data <- 
  read_csv(file_name, skip = 3, guess_max = 1000000)


# get what time of day the device was started at 
device_start_time <- 
  read_csv(file_name, n_max = 1, col_names = F) %>%
  mutate(date = parse_date_time(X1, orders = "mdy T"),
         time = paste(hour(date), minute(date), second(date), sep = ":")) %>%
    pull(time)


start_of_game_removed <- 
  gps_data %>%
  # add the offset of time that the device was started at to the timestamp
  mutate(clock_time = hms(TimeStamp) + hms(device_start_time)) %>%
  # based on the game start time we manually figured out 
  # filter out any pregame movement
  filter(clock_time > hms(game_start_time)) 

# get only data for the first half
first_half <-
  start_of_game_removed %>%
  filter(clock_time < hms(first_half_end)) 

# get only data for the second half
second_half <-
  start_of_game_removed %>%
  filter(clock_time > hms(half_time_end), clock_time < hms(game_end))

# lets take a look at the first half
first_half %>%
  ggplot(aes(x = TimeStamp, y = SmoothedVelocity)) +
  geom_point()
  
# split data if there's a bad gps signal
first_half_split <-
  first_half %>% 
  split_on_gps_hdop()

second_half_split <-
  second_half %>%
  split_on_gps_hdop()

# get only the smoothed velocity
first_half_split <- 
  first_half_split %>%
  mutate(data = map(data, ~ select(., SmoothedVelocity)),
         half = "first")

second_half_split <- 
  second_half_split %>%
  mutate(data = map(data, ~ select(., SmoothedVelocity)),
         half = "second")

# create our data set to model
smoothed_data <-
  bind_rows(first_half_split, second_half_split) %>%
  mutate(duration   = list(seq(100, 18000, by = 1000))) %>%
  unnest(duration)
          
# get the max mean velocity and sd
# WARNING THIS IS A LONG RUN 
# WE have data saved from this run in max_mean_vel_data.rds
max_mean_vel_data <-
  smoothed_data %>%
  mutate(max_mean_vel = 
           map2(data, duration, max_mean_vel)
         )

# Warning don't overwrite
#max_mean_vel_data %>%
#  select(-data) %>%
#  write_rds("max_mean_vel_data.rds")

max_mean_vel_data <- read_rds("max_mean_vel_data.rds")

# make the sd mean plot that we've been making
max_mean_vel_data %>%
  unnest(max_mean_vel) %>%
  mutate(duration = duration/100) %>%
  group_by(duration) %>%
  filter(mean_vel == max(mean_vel)) %>%
  ggplot() +
  geom_line(aes(x = duration , y = mean_vel)) +
  geom_line(aes(x = duration,  y = sd_vel))



  