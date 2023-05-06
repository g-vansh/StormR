#' Calculate the Accumulated Cyclone Energy of a Storm
#'
#' This function computes the accumulated cyclone energy of a given storm.
#'
#' @param df A dataframe containing the storm data for a single storm.
#' @return A numeric value representing the accumulated cyclone energy of the storm.
#' @export
calculate_cyclone_energy <- function(df){

    # Calculate the cyclone energy
    cyclone_energy <- sum(df$wind.speed.max^2)
    
    # Return the cyclone energy
    return(cyclone_energy)
    
}