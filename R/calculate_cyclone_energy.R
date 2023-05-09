#' Calculate the Accumulated Cyclone Energy of a Storm
#'
#' This function computes the accumulated cyclone energy of a given storm.
#'
#' @param df A dataframe containing the storm data for a single storm.
#' @return A numeric value representing the accumulated cyclone energy of the storm.
#' @export
calculate_cyclone_energy <- function(df){

    # Create a new dataframe to store the collapsed data
    df_collapsed <- data.frame()

    # Get the date and time of the first observation
    first_time <- as.POSIXct(paste(toString(df$Date[1]), toString(df$Time[1]), sep = ""), format = "%Y%m%d%H%M")

    # Create a list of maximum sustained wind every 6 hours (Use MaximumWind column)
    max_winds <- list()

    # Create a temporary 6 hour wind list
    temp_winds <- list()

    # Add the first wind speed to the temporary list
    temp_winds <- c(temp_winds, df$MaximumWind[1])

    # If df only has one row, max_winds is temp_winds
    if(nrow(df) == 1){

        max_winds <- temp_winds

    } else {

        # Loop through each row, and keep only the times that are 6 hours apart
        for(i in 2:nrow(df)){

            # Append information to temp_winds
            temp_winds <- c(temp_winds, df$MaximumWind[i])

            # Get the date and time of the current observation
            current_time <- as.POSIXct(paste(toString(df$Date[i]), toString(df$Time[i]), sep = ""), format = "%Y%m%d%H%M")

            # Calculate the time difference between the current observation and the first observation
            time_diff <- as.numeric(difftime(current_time, first_time, units = "mins"))

            # If time_diff is NA, set it to 0
            if(is.na(time_diff)){
                time_diff <- 0
            }

            # If the time difference is a multiple of 6, keep the row
            if(time_diff >= 360){
                max_winds <- c(max_winds, max(unlist(temp_winds)))
                temp_winds <- list()
                first_time <- current_time
            } else if(i == nrow(df)){
                max_winds <- c(max_winds, max(unlist(temp_winds)))
            }
        }
    }

    # Keep only the max_winds that are greater than 34 knots
    # Loop through each wind speed
    for(i in 1:length(max_winds)){

        # If the wind speed is less than 34 knots, set it to 0
        if(max_winds[i] < 35){
            max_winds[i] <- 0
        }

    }

    # Initialise energy sum
    energy_sum <- 0

    # Loop through each wind speed
    for(i in 1:length(max_winds)){

        energy <- max_winds[[i]] ^ 2

        # Add the energy to the energy sum
        energy_sum <- energy_sum + energy

    }

    # Calculate the cyclone energy
    cyclone_energy <- energy_sum / 10000

    # Return the cyclone energy
    return(cyclone_energy)

}
