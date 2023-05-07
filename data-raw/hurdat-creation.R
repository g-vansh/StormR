library(usethis)

dat <- read.csv("data-raw/hurdat-raw.txt", header= FALSE)

# Convert the second column to string
dat[,2] <- as.character(dat[,2])

#We will initialize the processed data
hurdat <- data.frame(matrix(NA, 0, ncol(dat)+2))
colnames(hurdat) <- c("StormID", "StormName", "Date", "Time",
                      "RecordIdentifier", "SystemStatus", "Latitude",
                      "Longitude", "MaximumWind", "MinimumPressure", "34NE",
                      "34SE", "34SW", "34NW", "50NE", "50SE", "50SW",
                      "50NW", "64NE", "64SE", "64SW", "64NW", "RadiusMaxWind")

# counter for the row of hurdat
k <- 0

# loop over rows of raw dataset and extract the necessary data
for(i in 1:nrow(dat)){

  # extract the current row of raw data
  current_row <- dat[i,]

  # determine if this row is a StormID
  if( grepl("[a-zA-Z]", current_row[1,1])){
    # if so, update the storm_id and storm_name
    storm_id <- current_row[1,1]
    storm_name <- gsub("\\s+", "", current_row[1,2])

  } else {

    # otherwise update the counter because we are adding data to
    # our processed hurdat data.frame. Then, write to the next row
    k <- k + 1
    hurdat[k,] <- cbind(storm_id, storm_name, current_row )
  }
}

#We have now formatted the dataset into a data frame. We need to finish
#cleaning up the data.

#Replace missing value codes with NAs
for(row in 1:nrow(hurdat)){
  for(column in 1:ncol(hurdat)){
    if (is.na(hurdat[row, column]) |
        nchar(gsub("\\s", "", hurdat[row, column]))==0){
      hurdat[row, column] <- NA
    }
  }
}

#Convert latitude and longitude columns to degrees north and
#degrees east respectively.
for(i in 1:nrow(hurdat)){
  #Convert latitude to degrees north
  current_latitude <- hurdat$Latitude[i]
  if(grepl("S", current_latitude)){
    new_latitude <- -1 * as.numeric(gsub("[A-Za-z]", "", current_latitude))
  } else {
    new_latitude <- as.numeric(gsub("[A-Za-z]", "", current_latitude))
  }
  hurdat$Latitude[i] <- new_latitude

  #Convert latitude to degrees east
  current_longitude <- hurdat$Longitude[i]
  if(grepl("W", current_longitude)){
    new_longitude <- -1 * as.numeric(gsub("[A-Za-z]", "", current_longitude))
  } else {
    new_longitude <- as.numeric(gsub("[A-Za-z]", "", current_longitude))
  }
  hurdat$Longitude[i] <- new_longitude
}

# Convert Latitude and Longitude to numeric
hurdat$Latitude <- as.numeric(hurdat$Latitude)
hurdat$Longitude <- as.numeric(hurdat$Longitude)

# Convert Date and Time to string
hurdat$Date <- as.character(hurdat$Date)
hurdat$Time <- as.character(hurdat$Time)

# Rename this file to the name of the dataset
write.csv(hurdat, "data-raw/hurdat.csv")
usethis::use_data(hurdat, overwrite = TRUE)
