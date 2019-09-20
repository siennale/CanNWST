split_on_gps_hdop <- function(data){
  first_half %>%
    mutate(hdop_count = GNSS.HDOP > 2,
           hdop_count = replace_na(hdop_count, 0),
           hdop_group = cumsum(hdop_count)) %>% 
    nest(-hdop_group) %>%
    mutate(length_group = map_dbl(data, nrow)) %>%
    filter(length_group > 500)
}



mean_no_na <- function(x){
  mean(x, na.rm = T)
}

sd_no_na <- function(x){
  sd(x, na.rm = T)
}

max_mean_vel <- function(gps_data, dur){
  mmv <-
    gps_data %>%
    mutate(sd_vel   = rollapply(SmoothedVelocity, FUN = sd_no_na, width = dur, fill = NA, align = "left"),
           mean_vel = rollapply(SmoothedVelocity, FUN = mean_no_na, width = dur, fill = NA, align = "left")) %>%
    filter(mean_vel == max(mean_vel, na.rm = T)) %>%
    select(sd_vel, mean_vel)
  
  return(mmv)
}
