#' Plot Storm Tracks
#'
#' This function plots a map of storm tracks for a selection of storms.
#' The map includes country and US state boundaries.
#'
#' @param df_list A list of data frames containing storm track data. Each data frame tracks one storm.
#' @return A map of storm tracks.
#' @export
plot_storm_track <- function(df_list){

    # Row bind the data frames in the list
    df <- dplyr::bind_rows(df_list)

    # Plot the storm tracks
    plot <- plot_storms(df)

    # Plot the map
    return(plot)

}

plot_storms <- function(df){
    # Use the maps package to get country and state boundaries
    library(maps)
    library(ggplot2)

    # Convert Latitude and Longitude to numeric
    df$Latitude <- as.numeric(df$Latitude)
    df$Longitude <- as.numeric(df$Longitude)

    # Get country boundaries
    world <- ggplot2::map_data("world")

    # Get state boundaries
    states <- ggplot2::map_data("state")

    # Plot the map
    plot <- ggplot() +
    # Add country boundaries
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
    # Add state boundaries
    geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
    # Add storm tracks
    geom_path(data = df, aes(x = Longitude, y = Latitude, group = StormID, color = StormID), linewidth = 1) +
    # Add a title
    ggtitle("Storm Tracks") +
    # # Add a caption
    labs(caption = "Source: Atlantic Oceanographic and Meteorological Laboratory") +
    # # Set the x-axis label
    xlab("Longitude") +
    # # Set the y-axis label
    ylab("Latitude") +
    # # Set the theme
    theme_bw() +
    # Set the x-axis limits to everything below 0
    scale_x_continuous(limits = c(-150, -25)) +
    # Set the y-axis limits to everything above 0
    scale_y_continuous(limits = c(10, 80)) +
    # Set the legend title
    labs(color = "Storm ID")

    return(plot)
}
