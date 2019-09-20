library(tidyverse)
library(snakecase)
library(lubridate)
library(zoo)

file_name <- "51516_1539820800_12178.csv"

gps_data <- 
  read_csv(file_name, skip = 3, guess_max = 1000000)


read_csv(file_name, n_max = 5)


start_of_game_removed <- 
  gps_data %>%
  mutate(clock_time = hms(TimeStamp) + hms("00:00:00")) %>%
  filter(clock_time > hms("00:14:44")) 

first_half <-
  start_of_game_removed %>%
  filter(clock_time < hms("01:00:00")) 

second_half <-
  start_of_game_removed %>%
  filter(clock_time > hms("01:15:42"), clock_time < hms("02:04:43"))



first_half %>%
  ggplot(aes(x = TimeStamp, y = SmoothedVelocity)) +
  geom_point()
  
first_half_split <-
  first_half %>% 
  split_on_gps_hdop()

second_half_split <-
  second_half %>%
  split_on_gps_hdop()

first_half_split <- 
  first_half_split %>%
  mutate(data = map(data, ~ select(., SmoothedVelocity)),
         half = "first")

second_half_split <- 
  second_half_split %>%
  mutate(data = map(data, ~ select(., SmoothedVelocity)),
         half = "second")

smoothed_data <-
  bind_rows(first_half_split, second_half_split) %>%
  mutate(duration   = list(seq(100, 18000, by = 1000))) %>%
  unnest(duration)
          
max_mean_vel_data <-
  smoothed_data %>%
  mutate(max_mean_vel = 
           map2(data, duration, max_mean_vel)
         )

max_mean_vel_data %>%
  unnest(max_mean_vel) %>%
  mutate(duration = duration/100) %>%
  group_by(duration) %>%
  filter(mean_vel == max(mean_vel)) %>%
  ggplot() +
  geom_line(aes(x = duration , y = mean_vel)) +
  geom_line(aes(x = duration,  y = sd_vel))

write_rds(max_mean_vel_data, "max_mean_vel_data.rds")

  