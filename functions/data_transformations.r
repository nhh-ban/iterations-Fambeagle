# I got the fail on the test so i needed to add a few things to make it go thorugh
# One thing that was hard to figure out was that there was lat and lon deep inside the location list
# pretty fun

transform_metadata_to_df <- function(stations_metadata) {
  
  # took some inspiration from the lecture notes
  df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    bind_rows() %>% 
    mutate(
      latestData = map_chr(latestData, 1, .default = ""),
      latestData = as_datetime(latestData, tz = "Europe/Berlin"),
      location = map(location, unlist),
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location) %>% 
    mutate(latestData = with_tz(latestData, tzone = "UTC")) %>% 
    select(id, name, latestData, lat, lon)
  
  return(df)
}

# What i did not expect with API data was the amount of list it actually is since ive never used that type of data


transform_volumes <- function(result) {
  volume_data <- result$data$trafficData$volume$byHour$edges
  from_times <- map_chr(volume_data, ~ .x$node$from)
  to_times <- map_chr(volume_data, ~ .x$node$to)
  volumes <- map_dbl(volume_data, ~ .x$node$total$volumeNumbers$volume)
  df <- tibble(
    from = ymd_hms(from_times),
    to = ymd_hms(to_times),
    volume = volumes
  )
  
  return(df)
}


