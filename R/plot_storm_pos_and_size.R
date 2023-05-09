#' Plot Storm Position and Size
#'
#' A function to plot a map of the position and size of a given storm.
#' We use the 34, 50, and 64 knot extent variables to do so.
#'
#' @param df A dataframe containing information about storms
#' @return ggplot object of the storm position and size mapped onto a world map.
#' @export
plot_storm_pos_and_size <- function(df){

  longitude <- df$Longitude
  latitude <- df$Latitude
  #Convert to kilometers by *1.852 and convert coordinate degrees to km by /111
  knot_34 <- data.frame("NE34" = df$`34NE`, "SE34" = df$`34SE`,
                        "SW34" = df$`34SW`, "NW34" = df$`34NW`) * 1.852/111
  knot_50 <- data.frame("NE50" = df$`50NE`, "SE50" = df$`50SE`,
                        "SW50" = df$`50SW`, "NW50" = df$`50NW`) * 1.852/111
  knot_64 <- data.frame("NE64" = df$`64NE`, "SE64" = df$`64SE`,
                        "SW64" = df$`64SW`, "NW64" = df$`64NW`) * 1.852/111

  maximum_extent <- list(knot_34 = knot_34, knot_50 = knot_50, knot_64 = knot_64)


  print(plot_storm(df, longitude, latitude, maximum_extent))

}

plot_storm <- function(df, longitude, latitude, maximum_extent){
  # Use the maps package to get country and state boundaries
  library(maps)
  library(ggplot2)
  library(ggforce)

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
    geom_point(aes(x = longitude, y = latitude), color = "red", size = 1) +
    #Add storm sizes
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_34"]]$NE34, start= 0, end= pi/2), color = "blue") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_34"]]$SE34, start= pi/2, end= pi), color = "blue") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_34"]]$SW34, start= pi, end= 3*pi/2), color = "blue") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_34"]]$NW34, start= -pi/2, end= 0), color = "blue") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_50"]]$NE50, start= 0, end= pi/2), color = "green") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_50"]]$SE50, start= pi/2, end= pi), color = "green") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_50"]]$SW50, start= pi, end= 3*pi/2), color = "green") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_50"]]$NW50, start= -pi/2, end= 0), color = "green") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_64"]]$NE64, start= 0, end= pi/2), color = "yellow") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_64"]]$SE64, start= pi/2, end= pi), color = "yellow") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_64"]]$SW64, start= pi, end= 3*pi/2), color = "yellow") +
    geom_arc(aes(x0= longitude, y0= latitude, r = maximum_extent[["knot_64"]]$NW64, start= -pi/2, end= 0), color = "yellow") +

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
    scale_x_continuous(limits = c(-200, 50)) +
    # Set the y-axis limits to everything above 0
    scale_y_continuous(limits = c(-30, 100)) +
    # Set the legend title
    labs(color = "Storm ID")

  return(plot)
}
