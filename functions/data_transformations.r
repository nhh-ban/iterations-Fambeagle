
transform_metadata_to_df <- function(stations_metadata) {
  
  df <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    bind_rows() %>% 
    mutate(
      latestData = map_chr(latestData, 1, .default = NA_character_),
      latestData = as_datetime(latestData, tz = "Europe/Berlin")
    ) %>% 
    mutate(
      latestData = with_tz(latestData, tzone = "UTC")
    )
  
  return(df)
}

