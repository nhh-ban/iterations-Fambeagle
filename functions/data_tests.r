# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

#Comments to the function
test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon") # here the function checks wheter the correct colnames
                                                                     # are in the dataframe names station_metadata using an if else statement
                                                                     # outputting pass if the df matches and fail if the coloumns do not match it 
  
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }
# Comments for this function
# This function sets a required min and max level for the expected amount of data in the station_metadata df
# it is using a if else function with an extra logical test checking if the data has either to many or to few row
# compared ot the desired amount, if its within the desired amount its a pass
test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }
# comments on the function 
# Agin we see an if logical test, however this time its looking for what type of coloumn types there is
# We can also se that its uses the map function to apply the typeof function to the coloumns
# if the coloumns match the type its outputs a pass and a fail if it does not.
test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
 # comments for the functions 
# this function looks trough the df to check if there are to many missing values present
# if there are to many NA we can have faulty data, so it will not pass us if the 
# amount of missing values exceed 200
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }
# comments to functions
# Checks if the df has the correct timezone
# like the other functions its a pass or fail if it does not find the required timezone

test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }

# comments on the functions
# this last function calls on all the other functions and does all the test simutainasly
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }





