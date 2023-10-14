# Function transforming meta data to a data frame
transform_metadata_to_df <- function(data){
  data[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    mutate(location = map(location, unlist)) %>% 
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}

# Function returning a data time variable in ISO8601 format, with added
# offset.
to_iso8601 <- function(datetime, offset){
  datetime <- as_datetime(datetime, tz = "UTC")
  offset_datetime <- datetime + days(offset) 
  time <- paste0(iso8601(anytime(offset_datetime)), "Z")
  return(time)
}


