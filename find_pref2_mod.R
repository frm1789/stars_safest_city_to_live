#
# find_pref2: A function to find prefecture names for the long/lat combinations using Google Maps API
# created by @kei51e
# find_pref2_mod: A function to find country names for the long/lat combinations using Google Maps API
# modified by @frm1789
find_pref2_mod <- function(long, lat, apiKey = NULL) {
  
  # Request URL parameters
  parameters <- ""
  
  # Add API Key in the parameters if available.
  if (!is.null(apiKey)) {
    parameters <- str_c("&key=", apiKey)
  }
  
  # Construct Google Maps APIs request URL.
  apiRequests <- iconv(str_c("https://maps.googleapis.com/maps/api/geocode/json?latlng=", lat, ",", long, parameters), "", "UTF-8")
  
  # Prefecture names will be stored to this.
  result <- c()
  
  # Iterate longitude/latitude combinations.
  for(i in 1:length(lat)) {
    
    # Avoid calling API too often.
    Sys.sleep(0.1)
    
    # Call Google Maps API.
    conn <- httr::GET(URLencode(apiRequests[i]))
    
    # Parse the JSON response. 
    apiResponse <- jsonlite::fromJSON(httr::content(conn, "text"))
    
    # Look at the address_components of the 1st address.
    ac <- apiResponse$results$address_components[[1]]
    
    # Prefecture name
    country <- ""
    
    # If address_components is available, look for 
    # the country where the meteorite fall. 
    if (!is.null(ac)) {
      
      # Iterate the types of the current address_components.
      for (j in 1:length(ac$types)) {
        if (ac$types[[j]][[1]] == "country") {
           country <- ac$long_name[[j]]
        }
      }
    }    
    result[i] <- country
  }
  # Return the result vector.
  return(result)
}