#' Plot Storm Position and Size
#'
#' A function to plot a map of the position and size of a given storm.
#' The 34, 50, and 64 knot extent variables.
#'
#' @param df
#' @return ggplot
#' @export
plot_storm_pos_and_size <- function(df){

  longitude <- df$Longitude
  latitude <- df$Latitude
  max_34 <- max(df$`34NE`, df$`34SE`, df$`34SW`, df$`34NW`)
  max_50 <- max(df$`50NE`, df$`50SE`, df$`50SW`, df$`50NW`)
  max_64 <- max(df$`64NE`, df$`64SE`, df$`64SW`, df$`64NW`)

  radius <-

  print(plot_storm(df, longitude, latitude, radius))

}

plot_storm <- function(df, storm_longitude, storm_latitude, storm_radius){
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

    # Add storm position
    geom_point(aes(x = storm_longitude, y = storm_latitude), color = "red", size = 3) +
    #Add storm size
    geom_circle(aes(x0 = storm_longitude, y0 = storm_latitude, r = storm_radius), fill = NA, color = "blue") +

    # Add a title
    ggtitle("Storm Position and Size") +
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
