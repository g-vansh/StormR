#' Determine if the Storm Made Landfall in the United States
#'
#' A function for determining whether the storm made landfall in the
#' continental United States based on its latitude and longitude values.
#'
#' @param df A dataframe containing the storm data for a single storm
#' @return TRUE if the storm made landfall in the continental United States, FALSE otherwise
#' @export
made_landfall_US <- function(df){
    library(maptools)
    library(maps)
    library(sp)

    # Get the US map data from the maps package
    us_map <- maps::map("state", plot = FALSE, fill = TRUE)

    # Create a polygon object from the map data
    us_poly <- maptools::map2SpatialPolygons(us_map, IDs = us_map$names, proj4string = CRS("+proj=longlat +datum=WGS84"))

    # Create a SpatialPoints object from the storm data
    storm_points <- SpatialPoints(df[, c("Longitude", "Latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

    # Determine if the storm made landfall in the US
    landfall <- sp::over(storm_points, us_poly)

    # Replace all NA values with 0
    landfall[is.na(landfall)] <- 0

    if (sum(landfall) > 0){
        landfall <- TRUE
    } else {
        landfall <- FALSE
    }

    # Return TRUE if the storm made landfall in the US, FALSE otherwise
    return(landfall)
}