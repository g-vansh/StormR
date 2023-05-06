#' Determine if the Storm Made Landfall in the United States
#'
#' A function for determining whether the storm made landfall in the 
#' continental United States based on its latitude and longitude values.
#'
#' @param df A dataframe containing the storm data for a single storm
#' @return TRUE if the storm made landfall in the continental United States, FALSE otherwise
#' @export
made_landfall_US <- function(df){
    
    library(maps)
    library(sp)

    # Get the US map data
    us <- map("world", "usa", plot = FALSE, fill = TRUE)

    # Create a polygon object from the map data
    us_poly <- map2SpatialPolygons(us, IDs = us$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

    # Create a dataframe with the storm's latitude and longitude
    storm_df <- data.frame(x = df$longitude, y = df$latitude)

    # Create a SpatialPoints object from the storm's latitude and longitude
    storm_sp <- SpatialPoints(storm_df, proj4string = CRS("+proj=longlat +datum=WGS84"))

    # Determine if the storm made landfall in the US
    storm_in_us <- point.in.polygon(storm_sp, us_poly)

    # Return TRUE if the storm made landfall in the US, FALSE otherwise
    return(storm_in_us)
}