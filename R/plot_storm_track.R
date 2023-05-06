#' Plot Storm Tracks
#'
#' This function plots a map of storm tracks for a selection of storms.
#' The map includes country and US state boundaries.
#'
#' @param df_list A list of data frames containing storm track data. Each data frame tracks one storm.
#' @return A map of storm tracks.
#' @export
plot_storm_track <- function(df_list){
    # Use the maps package to get country and state boundaries
    library(maps)

    # Get country boundaries
    world <- map_data("world")

    # Get state boundaries
    states <- map_data("state")

    # Plot the map
    ggplot() +
        # Add country boundaries
        geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
        # Add state boundariess
        geom_polygon(data = states, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
        # Add storm tracks
        geom_path(data = df_list[[1]], aes(x = long, y = lat, group = storm_id), color = "red") +
        geom_path(data = df_list[[2]], aes(x = long, y = lat, group = storm_id), color = "blue") +
        geom_path(data = df_list[[3]], aes(x = long, y = lat, group = storm_id), color = "green") +
        geom_path(data = df_list[[4]], aes(x = long, y = lat, group = storm_id), color = "orange") +
        geom_path(data = df_list[[5]], aes(x = long, y = lat, group = storm_id), color = "purple") +
        # Add a title
        ggtitle("Storm Tracks") +
        # Add a caption
        labs(caption = "Source: National Hurricane Center") +
        # Set the x-axis label
        xlab("Longitude") +
        # Set the y-axis label
        ylab("Latitude") +
        # Set the x-axis limits
        xlim(-100, -10) +
        # Set the y-axis limits
        ylim(0, 60) +
        # Set the theme
        theme_bw()
    
    # Return the plot
    return(plot)
}