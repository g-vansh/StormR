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

    # Add the first row to the dataframe
    df_collapsed <- rbind(df_collapsed, data.frame(df[1,]))

    # Loop through each row, and keep only the times that are 6 hours apart
    for(i in 2:nrow(df)){

        # Get the date and time of the current observation
        current_time <- as.POSIXct(paste(toString(df$Date[i]), toString(df$Time[i]), sep = ""), format = "%Y%m%d%H%M")

        # Calculate the time difference between the current observation and the first observation
        time_diff <- as.numeric(difftime(current_time, first_time, units = "mins"))

        # If the time difference is a multiple of 6, keep the row
        if(time_diff >= 360){
            df_collapsed <- rbind(df_collapsed, data.frame(df[i,]))
            first_time <- current_time
        }
    }

    # Initialise energy sum
    energy_sum <- 0

    # Loop through the 50 kt wind radii columns and sum their squares
    direction_list <- c("NE", "SE", "SW", "NW")
    for(i in 1:4){
        energy_sum <- energy_sum + sum(df[ ,paste("50", direction_list[i], sep = "")]^2)
        energy_sum <- energy_sum + sum(df[ ,paste("64", direction_list[i], sep = "")]^2)
    }

    # Calculate the cyclone energy
    cyclone_energy <- energy_sum / 10000

    # Return the cyclone energy
    return(cyclone_energy)

}

# Load the dataset
load("data/hurdat.rda")

# Subset hurdat to StormID AL012021
hurdat <- hurdat[hurdat$StormID == "AL142016", ]

# Calculate the cyclone energy
cyclone_energy <- calculate_cyclone_energy(hurdat)
