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
  # Ensure that datetime is correctly parsed as a datetime object
  datetime <- as_datetime(datetime, tz = "UTC")
  
  # Check if the conversion was successful before proceeding
  if(is.na(datetime)) stop("Datetime conversion failed for input: ", datetime)
  
  offset_datetime <- datetime + lubridate::days(offset) 
  
  # Ensure the offset does not create an NA
  if(is.na(offset_datetime)) stop("Offset results in an invalid datetime for input: ", datetime)
  
  # Return the datetime in ISO8601 format with "Z" denoting UTC time
  time <- paste0(anytime::iso8601(offset_datetime), "Z")
  return(time)
}

# Function transforming query return to a data frame suitable for plotting 
transform_volumes <- function(query_output) {
  query_output %>% 
    pluck("trafficData", "volume", "byHour", "edges") %>% 
    map_dfr(~{
      tibble(
        from = anytime::anytime(.x$node$from),
        to = anytime::anytime(.x$node$to),
        volume = .x$node$total$volumeNumbers$volume
      )
    })
}


