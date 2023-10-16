
transform_metadata_to_df <- function(stations_metadata) {
  
  df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    bind_rows() %>% 
    mutate(
      latestData = map_chr(latestData, 1, .default = NA_character_),
      latestData = as_datetime(latestData, tz = "Europe/Berlin"),
      latestData = with_tz(latestData, tzone = "UTC")
    ) %>% 
    select(id, name, latestData, lat, lon) # I got the fail on the test so i needed to add this to the function to get it through 
  
  return(df)
}




