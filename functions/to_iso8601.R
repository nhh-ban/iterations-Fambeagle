
to_iso8601 <- function(date_time, offsetdays) {
 
  date <- date_time + days(offsetdays)
  
  iso_date <- iso8601(date, use_tz = TRUE)
  return(paste0(substr(iso_date, 1, 19), "Z"))
}
