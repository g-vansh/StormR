#' Interpolate Storm Track
#'
#' This function interpolates a storm track to 30 minute increments.
#'
#' @param df Hurricane Dataset containing of a single storm track
#' @return Updated Hurricane Dataset with 30 minute increments interpolated
#' @export
interpolate_storm_track <- function(df){
    # Create a new dataframe to store the interpolated data
    df_interpolated <- data.frame()

    # Loop through each row in the dataframe
    for(i in 1:(nrow(df)-1)){

        # Calculate the time difference between the current row and the next row
        time_diff <- difftime(df$time[i+1], df$time[i], units = "mins")

        # If the time difference is greater than 30 minutes, interpolate the data
        if(time_diff > 30){

            # Calculate the number of 30 minute increments between the two rows
            num_increments <- time_diff / 30

            # Calculate the latitude and longitude difference between the two rows
            lat_diff <- df$latitude[i+1] - df$latitude[i]
            lon_diff <- df$longitude[i+1] - df$longitude[i]

            # Calculate the latitude and longitude increment between the two rows
            lat_increment <- lat_diff / num_increments
            lon_increment <- lon_diff / num_increments

            # Loop through each 30 minute increment and interpolate the data
            for(j in 1:(num_increments-1)){

                # Calculate the new latitude and longitude
                new_lat <- df$latitude[i] + (lat_increment * j)
                new_lon <- df$longitude[i] + (lon_increment * j)

                # Calculate the new time
                new_time <- df$time[i] + (30 * j)

                # Create a new row with the interpolated data
                new_row <- data.frame(STORM_ID = df$STORM_ID[i],
                                      NAME = df$NAME[i],
                                      ISO_TIME = new_time,
                                      LAT = new_lat,
                                      LON = new_lon,
                                      WIND = df$WIND[i],
                                      PRESSURE = df$PRESSURE[i],
                                      STORM_TYPE = df$STORM_TYPE[i],
                                      STORM_SPEED = df$STORM_SPEED[i],
                                      STORM_DIR = df$STORM_DIR[i],
                                      STORM_DIST = df$STORM_DIST[i],
                                      STORM_BASIN = df$STORM_BASIN[i],
                                      STORM_SUBBASIN = df$STORM_SUBBASIN[i],
                                      STORM_SEASON = df$STORM_SEASON[i],
                                      STORM_YEAR = df$STORM_YEAR[i],
                                      STORM_MONTH = df$STORM_MONTH[i],
                                      STORM_DAY = df$STORM_DAY[i],
                                      STORM_HOUR = df$STORM_HOUR
                                        )

                # Add the new row to the dataframe
                df_interpolated <- rbind(df_interpolated, new_row)
            }
        }
    }

    return(df_interpolated)
}
