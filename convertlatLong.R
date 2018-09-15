# Idea: Gabriel Tellez
# modify by: @FRM1789

convert_latlong <- function(v_lat, v_long) {

      df <- data.frame(v_lat, v_long)
      
      # find all South latitudes and make new column that distinguishes North and South
      df$lat_South <- grepl("S", df$lat)
      # change to -1 for South and 1 for North
      df$lat_South <- ifelse(df$lat_South == TRUE, -1, 1)
      # remove N and S from Latitude
      df$lat <- gsub("N", "", df$lat)
      df$lat <- gsub("S", "", df$lat)
      # convert Latitude to numeric
      df$lat <- as.numeric(df$lat)
      # make new Latitudes column to show negative sign for South
      df$lats <- df$lat * df$lat_South
      
      #do the same process from above for West longitudes
      df$long_West <- grepl("W", df$long)
      df$long_West <- ifelse(df$long_West == TRUE, -1, 1)
      df$long <- gsub("E", "", df$long)
      df$long <- gsub("W", "", df$long)
      df$long <- as.numeric(df$long)
      df$longs <- df$long * df$long_West
      
      # delete auxiliary columns 
      df <- df[ , -which(names(df) %in% c("long_West","lat_South","longs","lats"))]
      
      ## 1.4 Delete NA values
      df <- df[complete.cases(df[ ,]),]
      
      return(df)
      }