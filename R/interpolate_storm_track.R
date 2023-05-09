#' Interpolate Storm Track
#'
#' This function interpolates a storm track to 30 minute increments.
#'
#' @param df Hurricane Dataset containing of a single storm track
#' @return Updated Hurricane Dataset with 30 minute increments interpolated
#' @export
interpolate_storm_track <- function(df){
    
    # If the dataframe is empty, return the dataframe
    if(nrow(df) == 0){
        return(df)
    }

    # If the dataframe only has one row, return the dataframe
    if(nrow(df) == 1){
        return(df)
    }

    # Create a new dataframe to store the interpolated data
    df_interpolated <- data.frame()

    # Convert latitude and longitude to numeric
    df$Latitude <- as.numeric(df$Latitude)
    df$Longitude <- as.numeric(df$Longitude)

    # Loop through each row in the dataframe
    for(i in 1:(nrow(df)-1)){

        # Calculate the time difference between the current row and the next row
        time_prev <- as.POSIXct(paste(toString(df$Date[i]), toString(df$Time[i]), sep = ""), format = "%Y%m%d%H%M")
        time_next <- as.POSIXct(paste(toString(df$Date[i+1]), toString(df$Time[i+1]), sep = ""), format = "%Y%m%d%H%M")
        time_diff <- as.numeric(difftime(time_next, time_prev, units = "mins"))

        # If the time difference is greater than 30 minutes, interpolate the data
        if(time_diff > 30){

            # If i = 1, then we need to add the first row to the dataframe
            if(i == 1){
                df_interpolated <- rbind(df_interpolated, data.frame(df[i,]))
            }

            # Calculate the number of 30 minute increments between the two rows
            num_increments <- time_diff / 30

            # Calculate the latitude and longitude difference between the two rows
            lat_diff <- df$Latitude[i+1] - df$Latitude[i]
            lon_diff <- df$Longitude[i+1] - df$Longitude[i]

            # Calculate the latitude and longitude increment between the two rows
            lat_increment <- lat_diff / num_increments
            lon_increment <- lon_diff / num_increments

            # Loop through each 30 minute increment and interpolate the data
            for(j in 1:(num_increments-1)){

                # Calculate the new latitude and longitude
                new_lat <- df$Latitude[i] + (lat_increment * j)
                new_lon <- df$Longitude[i] + (lon_increment * j)

                # Calculate the new time and date as POSIXct
                new_time <- as.POSIXct(paste(toString(df$Date[i]), toString(df$Time[i]), sep = ""), format = "%Y%m%d%H%M") + (30 * j * 60)
                new_date <- as.Date(new_time)

                # Convert the new time and date to the correct format
                new_time <- format(new_time, "%H%M")
                new_date <- format(new_date, "%Y%m%d")

                # Add these columns to the new row
                new_row <- data.frame(
                                        StormID = df$StormID[i],
                                        StormName = df$StormName[i],
                                        Date = new_date,
                                        Time = new_time,
                                        Latitude = new_lat,
                                        Longitude = new_lon,
                                        RecordIdentifier = df$RecordIdentifier[i],
                                        SystemStatus = df$SystemStatus[i],
                                        MaximumWind = df$MaximumWind[i],
                                        MinimumPressure = df$MinimumPressure[i],
                                        "34NE" = df$`34NE`[i],
                                        "34SE" = df$`34SE`[i],
                                        "34SW" = df$`34SW`[i],
                                        "34NW" = df$`34NW`[i],
                                        "50NE" = df$`50NE`[i],
                                        "50SE" = df$`50SE`[i],
                                        "50SW" = df$`50SW`[i],
                                        "50NW" = df$`50NW`[i],
                                        "64NE" = df$`64NE`[i],
                                        "64SE" = df$`64SE`[i],
                                        "64SW" = df$`64SW`[i],
                                        "64NW" = df$`64NW`[i],
                                        RadiusMaxWind = df$RadiusMaxWind[i])

                # Add the new row to the dataframe
                df_interpolated <- rbind(df_interpolated, new_row)
            }

            # Add the last row to the dataframe
            df_interpolated <- rbind(df_interpolated, data.frame(df[i+1,]))

        } else {
            # If the time difference is less than 30 minutes, add the row to the dataframe
            df_interpolated <- rbind(df_interpolated, data.frame(df[i,]))
        }

    }

    return(df_interpolated)
}