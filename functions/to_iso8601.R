
to_iso8601 <- function(date_time, offsetdays) {
  date <- date_time + days(offsetdays)
  iso_date <- iso8601(date)
  
  return(paste0(substr(iso_date, 1, 19), "Z"))
}

