## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 
## MANAGING ACOUSTIC TELEMETRY DATA IN R
## 02 - Handling acoustic telemetry data
##
## COST action ETN Training School - Olsztyn (Polska)
##
## Author: Eneko Aspillaga (IMEDEA, CSIC-UIB, Spain)
## Contact: aspillaga@imedea.uib-csic.es
## Date: 20 May - 1 June 2023
## 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Remove all the objects in the environment.
rm(list = ls())

# Set working directory (directory of the current script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required libraries:
# install.packages(c("data.table", "lubridate", "sp", "sf))

# Load libraries
library(data.table) # Extension of data.frame for faster management
library(lubridate) # Dealing with dates and times
library(sp)
library(sf)

# Load all the data files prepared in the previous script

# From csv files:
deploy <- fread("./exports/deployment_database.csv")
deploy

fish_db <- fread("./exports/fish_database.csv")
fish_db

data <- fread("./exports/detection_data_20230601.csv.gz")


# PART 1. Plotting the receiver network ========================================

# Select last deployments. First we will order them by decreasing deployment
# date and then remove the duplicated station IDs (will remove the oldest 
# deployments)
stations <- deploy[order(deploy$date_in, decreasing = TRUE), ]
nrow(stations) # 26 deployments

stations <- stations[!duplicated(stations$station_id), ]
nrow(stations) # We keep the unique 13 stations witht their last positions

# Coordinates are in decimal latitude and longitude (geographic coordenates)
stations[, c("lat", "long")]

# We can define this projection using the proj4 system (library for performing 
# conversions between cartographic projections)
proj_longlat <- CRS("+proj=longlat +datum=WGS84")
# Reference for the projection: https://epsg.io/4326


# For plotting and analysis, we will use a UTM system (projected coordinates)
proj_utm <- CRS("+proj=utm +datum=WGS84 +zone=31")
# Reference: https://epsg.io/32631


# To transform from one coordinate system to another, we have first to create
# a "SpatialPoints object
sp <- SpatialPoints(coords = stations[, c("long", "lat")], 
                    proj4string = proj_longlat)
coordinates(sp)
plot(sp, axes = T)

# We can transform them using the "spTransform" function and providing the
# desired coordinate system
sp_t <- spTransform(sp, CRSobj = proj_utm)
coordinates(sp_t)
plot(sp_t, axes = T)

# Add the new coordinates to the "stations" data.table
stations$x <- coordinates(sp_t)[, 1]
stations$y <- coordinates(sp_t)[, 2]

# Another advantage of projected systems is that we can calculate distances
# between coordinates directly (Euclidean distance)

# Distance between the first and the second receiver
dist <- sqrt((stations$x[1] - stations$x[2])^2 + 
               (stations$y[1] - stations$y[2])^2)
dist # Distance in meters

# Or using the "dist" function to create a distance matrix
dist_mat <- dist(stations[, c("x", "y")])
dist_mat

# Load coastline shapefile using the "sf" package
coast <- sf::read_sf("./data/gis/medes_islands.shp")
# Tranfrom to sp object (easier to plot)
coast <- as_Spatial(coast)
plot(coast, col = "gray80", border = "gray50", xlim = c( 517770, 519070), 
     ylim = c(4654100, 4655800), axes = TRUE, main = "Receiver array")
points(stations$x, stations$y, pch = 21, bg = "gold", cex = 1)



# PART 2. Adding coordinares to detections =====================================

# Create a match vector with the "match" function. It extracts the indices of
# the elements of the second argument that match the elements in the first
# element
match_indx <- match(data$station, stations$station_id)
data$station[1:10]
stations$station_id[match_indx][1:10]

# We can use this index now to transfer the variables in the "stations" table
# to the "data" table
data$x <- stations$x[match_indx]
data$y <- stations$y[match_indx]

data


# PART 3. Removing false detections ============================================

# We will separate all the detections from unknown IDs. We will use the %in% 
# operator, which allows us to check if the values of the first vector are 
# present in the second argument.

# EXAMPLE
data_vector <- c("a", "b", "c", "d", "a", "a", "f", "b")

# Check which element coincides with the value "a"
data_vector == "a"
which(data_vector == "a")
data_vector[data_vector == "a"] # Get the elements that are equat to "a"
data_vector[data_vector != "a"] # Return the elements that are not "a"

# But if we want to check more than one value, we have to use the "%in% operator
data_vector %in% c("a", "b")
data_vector[data_vector %in% c("a", "b")] # Return elements "a" and "b"
data_vector[!data_vector %in% c("a", "b")] # Return elements != a and != b


# Use this to separate detections from known IDs and unkown IDs
fish_db$tag_id # Those are the known IDs

data$tag_id # Those are the values we want to check

data$tag_id %in% fish_db$tag_id # Returns a logical vector


# Use the %in% operator to extract the detections of not known tags (ghost
# detections)
ghost_det <- data[!data$tag_id %in% fish_db$tag_id, ]

# Calculate the occurrence of each ghost ID
tab <- table(ghost_det$tag_id)
tab
length(tab)

# Order the table in decreasing order
tab[order(tab, decreasing = TRUE)]

# Check number of ghost detections per protocol
table(ghost_det$protocol)
table(ghost_det$tag_id, ghost_det$protocol)


# Use the %in% operator to extract all the detections correponding to known 
# individuals (using the "fish_id" individual codes to only get the detections
# assigned to the individuals)
data <- data[data$fish_id %in% fish_db$fish_id, ]

# Check number of detections per ID
table(data$fish_id)



# PART 4. Subsetting detections ================================================

# By individual ----------------------------------------------------------------

# Vector with all the fish IDs
fish_db$fish_id

# Pick one individual:
ind_id <- "SPHVIR-01"

# Subset the data
data_ind <- data[data$fish_id == ind_id, ]
data_ind



# By species -------------------------------------------------------------------

# Vector of tagged species
unique(fish_db$species)

# Codes of dusky groupers
grouper_codes <- fish_db$fish_id[fish_db$species == "Epinephelus marginatus"]

# Extract all detections of these individuals
data_sp <- data[data$fish_id %in% grouper_codes, ]
data_sp
table(data_sp$fish_id)



# By date range ----------------------------------------------------------------

# Time range of the entire dataset
range(data$date_time) 
our_range <- ymd("2022-01-01", "2022-02-28")

data_sub <- data[data$date_time >= our_range[1] & date_time <= our_range[2], ]
range(data_sub$date_time)
data_sub



# PART 5. Calculate residence indices of all the individuals -------------------

all_ids <- unique(data$fish_id) # Extract unique IDs
all_ids

# Loop across all the IDs to estimate the number of days with detections and
# the tracking period
ri_df <- lapply(all_ids, function(id) {
  
  data_tmp <- data[data$fish_id == id]
  detection_dates <- as.Date(data_tmp$date_time)
  capt_date <- as.Date(fish_db$tag_date[fish_db$fish_id == id])
  last_date <- max(detection_dates)
  
  # Tracking period: Days elapsed between the tagging and the last detection
  tp <- last_date - capt_date + 1
  tp <- as.numeric(tp)
  
  # Number of days with detections
  dd <- length(unique(detection_dates))
  
  # Residence index
  ri <- dd / tp
  
  # Also, we will calculate the number of detections
  n_detect <- nrow(data_tmp)
  
  # Prepare the data.table
  result <- data.table(fish_id = id, capture_date = capt_date, 
                       last_date = last_date, n_detect = n_detect,
                       track_period = tp, days_detect = dd, ri = ri)
  return(result)
  
})

# Merge all the rows
ri_df <- rbindlist(ri_df)
ri_df


# PART 6. Plot sensor values ===================================================

data_ind

plot(-sensor_val ~ date_time, data = data_ind[data_ind$sensor_type == "P", ],
     type = "l", xlab = "Date", ylab = "Depth (m)")

plot(sensor_val ~ date_time, data = data_ind[data_ind$sensor_type == "A", ],
     type = "l", xlab = "Date", ylab = "Acceleration (m/s2)")

# Calculate daily means using data.table magic
# First, generate a vector of dates inside the "data_ind" data.table
data_ind[, date := as.Date(date_time)]

# The usual way of doing this using base R would be this one. The result is
# exactly the same, but the previous is a bit faster
# data_ind$date <- as.Date(data_ind$date_time)

# Calculate daily means using this special data.table sintaxis 
data_days <- data_ind[, .(mean_depth = mean(sensor_val[sensor_type == "P"]),
                          mean_accel = mean(sensor_val[sensor_type == "A"])),
                      by = date]
data_days

# Plot daily average values
plot(-mean_depth ~ date, data = data_days,
     type = "l", xlab = "Date", ylab = "Depth (m)")
plot(mean_accel ~ date, data = data_days,
     type = "l", xlab = "Date", ylab = "Depth (m)")


# In case we want to calculate the average values for different time intervals,
# we can use the "floor_date" function in lubridate to round down the 
# date_time to the desired interval


# 2-Hour intervals
data_ind[, hours := floor_date(date_time, "2hour")]
data_hours <- data_ind[, .(mean_depth = mean(sensor_val[sensor_type == "P"]),
                          mean_accel = mean(sensor_val[sensor_type == "A"])),
                      by = hours]
plot(-mean_depth ~ hours, data = data_hours,
     type = "l", xlab = "Date", ylab = "Depth (m)")
plot(mean_accel ~ hours, data = data_hours,
     type = "l", xlab = "Date", ylab = "Depth (m)")


# Weeks 
data_ind[, weeks := floor_date(date_time, "week")]
data_weeks <- data_ind[, .(mean_depth = mean(sensor_val[sensor_type == "P"]),
                           mean_accel = mean(sensor_val[sensor_type == "A"])),
                       by = weeks]
plot(-mean_depth ~ weeks, data = data_weeks,
     type = "l", xlab = "Date", ylab = "Depth (m)")
plot(mean_accel ~ weeks, data = data_weeks,
     type = "l", xlab = "Date", ylab = "Depth (m)")


# PART 7. Presence/absence plot ================================================

data_sp
data_sp[, date := as.Date(date_time)]

daily_det <- data_sp[, .(n_det = length(date_time)), by = list(fish_id, date)]
daily_det

# Convert IDs to factor, so it can be represented as a numeric vector
daily_det$fish_id <- factor(daily_det$fish_id)
daily_det$fish_id 
as.numeric(daily_det$fish_id)
levels(daily_det$fish_id)

plot(as.numeric(fish_id) ~ date, data = daily_det, pch = "|", 
     xlab = "Date", ylab = "Fish index")

# Make the plot better
par(mar = c(3.1, 8, 1, 1))
plot(as.numeric(fish_id) ~ date, data = daily_det, pch = "|", 
     xlab = "Date", ylab = "", axes = FALSE)
axis.Date(1, daily_det$date)
axis(2, at = 1:length(levels(daily_det$fish_id)), 
     labels = levels(daily_det$fish_id), las = 1)
box()


# PART 8. Map of detections per receiver =======================================

data_ind

# Count detections per receiver
det_tab <- table(data_ind$station)
det_tab

# Add these numbers to the stations data
stations
stations$n_det <- det_tab[match(stations$station_id, names(det_tab))]
stations

# Function to rescale variables to the desired range
rescale <- function(values, original.range, new.range) {
  rescaled <- (new.range[1] + (values - original.range[1]) * 
                 (new.range[2] - new.range[1]) / 
                 (original.range[2] - original.range[1]))
  return(rescaled)
}


# Rescale detection data to specific range (cex size of points)
stations$cex <- rescale(stations$n_det, 
                        original.range = c(0, max(stations$n_det)),
                        new.range = c(0, 5))
par(mar = c(3, 3, 1, 1))
plot(coast, col = "gray80", border = "gray50", xlim = c( 517770, 519070), 
     ylim = c(4654100, 4655800), axes = TRUE)
points(stations$x, stations$y, pch = 21, bg = "gold", cex = stations$cex)

# Add legend
legend_values <- pretty(stations$n_det, 3)
legend_cex <- rescale(legend_values,
                      original.range = c(0, max(stations$n_det)),
                      new.range = c(0, 5))
legend("bottomleft", legend = legend_values, pch = 21, pt.bg = "gold",
       pt.cex = legend_cex, y.intersp = 2, x.intersp = 2, 
       title = "No. of detections")


