## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 
## MANAGING ACOUSTIC TELEMETRY DATA IN R
## 01 - Loading and merging acoustic telemetry data
##
## COST action ETN Training School - Olsztyn (Polska)
##
## Author: Eneko Aspillaga (IMEDEA, CSIC-UIB, Spain)
## Contact: aspillaga@imedea.uib-csic.es
## Date: 20 May - 1 June 2023
## 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Set working directory (directory of the current script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Install required libraries:
# install.packages(c("data.table", "lubridate", "sp"))

# Load libraries
library(data.table) # Extension of data.frame for faster management
library(lubridate) # Dealing with dates and times


# PART 1. Loading data to R ====================================================

# 1.1. Loading single files ----------------------------------------------------

# The basic function to load data from text files to R is "read.table".
# In this line, we will load the data corresponding to the receiver deployments
# into the "deploy" object.

deploy <- read.table("./data/deployments.csv", sep = ",", header = TRUE)

# For CSV files, we can also use the "read.csv" function. This is the same as
# "read.table", but sep = "," and "header = TRUE" are already set by default
deploy <- read.csv("./data/deployments.csv")

# The result is a data.frame with 6 columns
deploy
class(deploy)

# We can also load data from text using the "fread" (fast read) function of
# the data.table package, which is much faster than "read.table" (we will see
# it later)
deploy <- fread("./data/deployments.csv")

# If we use "fread", the result is a "data.table" object, which in general 
# behaves as a "data.frame"
class(deploy)

# Load the fish metadata
fish_db <- fread("./data/fish_metadata.csv")
fish_db

# We can also load data from excel files using the "read.xlsx" function in the
# "xlsx" package.


# 1.2. Loading multiple detection files ----------------------------------------

# Receiver log files are inside the "receiver_logs" directory. There is a
# subfolder for each different station ("R01" to "R13").

# We can load one file to see how it looks
detect <- fread("./data/receiver_logs/R01/TBR_5469_20220313.csv")
head(detect)

# We could do this one by one, but it would take ages and make the code 
# unnecessarily long. Instead, we can generate a vector with the path of all
# the files and load them at once using a "lapply" loop.

# Create file vector using the "list.files" function
files <- list.files("./data/receiver_logs/", pattern = ".csv", full.names = TRUE,
                    recursive = TRUE)
files

# Load data and generate a list of data.frames (each element of the list
# contains the data of one file)

# Load files using the base R:
t <- Sys.time() # Register initial time to later calculate the time it takes
data_list <- lapply(files, function(f) read.csv(f))
Sys.time() - t # Calculate the elapsed time

head(data_list[[1]])
head(data_list[[2]])


# Load files using "data.table":
t <- Sys.time()
data_list <- lapply(files, function(f) fread(f))
Sys.time() - t

data_list[[1]]
data_list[[2]]

# Merge all the elements to a single one using the function "rbindlist" from
# package "data.table"
data <- rbindlist(data_list)
data

# Change column names to make them handy
colnames(data)
colnames(data) <- c("date", "time_utc", "receiver", "transmitter", "sensor_val")
data

# Check number of detections
nrow(data)

# Remove the unnecesary objects
rm(data_list, files, detect, t)


# 2. Dealing with dates and times ==============================================

# 2.1. Introduction to date and time formats -----------------------------------

# Our three data.tables have different date formats, expressed as characters. 
# We need to convert them to the POSIXct format (number of seconds since 1970) 
# or to the "Date" format (only for dates).

dates <- c("2023-05-31", "2023-06-01")
class(dates)
# dates[2] - dates[1] # Error

dates <- as.Date(dates)
class(dates)
dates[2] - dates[1] # Now we can use them to calculate time differences
dates[2] < dates[1] # Or to establish logical relations

# Converting date and times using "strptime"
date_times_char <- c("2023-06-01 12:00", "2023-06-01 12:05")
class(date_times_char)

# The "format" argument has to match the format of the date
help(strptime)
strptime(date_times_char, format = "%Y-%m-%d %H:%M:%S") # Wrong format
strptime(date_times_char, format = "%Y-%m-%d %H:%M")

# WARNING: note that the timezone of the system is automatically assigned,
# to assign a different one, we have to specify it
strptime(date_times_char, format = "%Y-%m-%d %H:%M", tz = "UTC")

# It is easier using the lubridate package
ymd_hm(date_times_char, tz = "UTC")


# 2.2. Set date and time formats of telemetry files ----------------------------

# In fish metadata, we only know the tagging date (no time information)
head(fish_db$tag_date)
fish_db$tag_date <- as.Date(fish_db$tag_date)
head(fish_db$tag_date)
class(fish_db$tag_date)


# In deployment metadata, deployment and retrieval times have hours and minutes,
# and they are at the local time zone (CET)
head(deploy$date_in)
head(deploy$date_out)
deploy$date_in <- ymd_hm(deploy$date_in, tz = "CET")
deploy$date_out <- ymd_hm(deploy$date_out, tz = "CET")

# Note that the "CET" timezone considers the Daylight Saving Time
deploy$date_in

# To avoid confusion, we will transform them to "UTC"
deploy$date_in <- with_tz(deploy$date_in, "UTC")
deploy$date_in

deploy$date_out <- with_tz(deploy$date_out, "UTC")


# In the detection data, date and time are separated in two columns, we need
# to merge them before converting them to the time format. 

# We can use paste to concatenate strings like this:
paste(c("2023-05-31", "2023-06-01"), c("10:00", "12:00")) # Example

# Concatenate date and time:
data$date_time <- paste(data$date, data$time_utc)

# Convert to time format. Note that, in this case, the timezone must be "UTC"
data$date_time <- ymd_hms(data$date_time, tz = "UTC")
data$date_time[1:10]

# We can remove unnecessary columns
data$date <- NULL
data$time_utc <- NULL

# Finally, we will order detection data by date
data <- data[order(data$date_time), ]
data


# 3. Assign deployment and fish IDs to detections ==============================

# 3.1. Match the format of IDs in data and metadata ----------------------------

# Now we want to assign the ID of the station (or the deployment) and the ID of 
# the fishes to each detection. To do so, we need to match the different
# data frames.

# RECEIVER ID
# We will match the detection data ("data") with the deployment metadata 
# ("deploy") using the receiver ID. However, they have a different format in
# each data frame.
deploy$receiver_id # Numeric vector
class(deploy$receiver_id) 

unique(data$receiver) # Character, with "TBR-" preceding the ID code
class(data$receiver)

# We need to extract the ID code from data$receiver and transform it to 
# numeric. In this case, since all the IDs have the same format, we can extract
# the last four numbers of the code using "substr".

# EXAMPLE:
example_ids <- c("TBR-9999", "TBR-1000")
example_ids <- substr(example_ids, start = 5, stop = 9)
example_ids # Still character format
class(example_ids) 
# Convert to numeric
example_ids <- as.numeric(example_ids)
class(example_ids)
example_ids

# Apply this to the receiver column
data$receiver <- as.numeric(substr(data$receiver, start = 5, stop = 9))
unique(data$receiver) # Now data is numeric

# Now we can check if all the receivers in "deploy" are in the detection data
deploy$receiver_id %in% unique(data$receiver) # All should be TRUE
# With this, we ask if there is any "FALSE" (better for long vectors)
any(!deploy$receiver_id %in% unique(data$receiver))

# We can also check if any of the receivers in "data" is missing in "deploy"
any(!unique(data$receiver) %in% deploy$receiver_id)


# FISH ID
# We will match the detection data with the fish metadata using the transmitter
# ID. But again, they have different formats:
fish_db$tag_id
class(fish_db$tag_id) # Numeric

unique(data$transmitter) # Character
class(data$transmitter)

# In this case, fish ID in the data is preceded by the name of the protocol,
# separated by a hyphen ("-"). However, both the protocol names and the IDs have
# different lengths, so we cannot use the "substr" approach as with the
# receivers. Also, we are intested in keeping the procol of the tags in our
# data.frame.

# In this case, we can use "regular expressions". They are sequences of 
# characters that describe match patterns in text.

# For instance, have a look at this regular expression:
"(.*)-(.*)"

# The dot stands for any character and the asterisk means that can be repeated
# as many times and needed. That is, ".*" represents any text string. In this
# expression, we are describing the general pattern of the transmitter column
# in "data", which is: two things separated by a hyphen.

# We can use the "grepl" function to see which elements of a vector match a 
# pattern (this will be helpful in the future):
grepl(pattern = "(.*)-(.*)", x = data$transmitter)
any(!grepl(pattern = "(.*)-(.*)", x = data$transmitter))

# We can be also more specific, indicating that the second part of the code is
# only numeric. In this case [0-9] indicates any number, and the asterisk that
# it can be repeated many times
grepl(pattern = "(.*)-([0-9]*)" , x = data$transmitter)
any(!grepl(pattern = "(.*)-([0-9]*)", x = data$transmitter))


# We can use the "gsub" function to extract the different elements contained
# by the parenthesis, using "\\1" for the first element and "\\2" for the 
# second element.
gsub(pattern = "(.*)-(.*)", replacement = "\\1", x = data$transmitter[1:10])
gsub(pattern = "(.*)-(.*)", replacement = "\\2", x = data$transmitter[1:10])

# So lets add a new column to the data with the protocol
data$protocol <- gsub(pattern = "(.*)-(.*)", replacement = "\\1", 
                      x = data$transmitter)

# And a new column with the numeric ID version of the tag
data$tag_id <- as.numeric(gsub(pattern = "(.*)-(.*)", replacement = "\\2",
                               x = data$transmitter))
data

# Lets check if all the IDs in the fish metadata are in the detection data
fish_db$tag_id %in% unique(data$tag_id)
sum(!fish_db$tag_id %in% unique(data$tag_id)) # There is one fish without data

# Lets check which fish is:
fish_db[!fish_db$tag_id %in% unique(data$tag_id), ] # One E. marginatus

# We can also check how many detections correspond to IDs that are not included
# in our fish database, which correspond to either fish not included in the 
# database or, more probably, to false detections of non-existing tags.
sum(!data$tag_id %in% fish_db$tag_id) # Total number of detections
# Number of unique IDs not included in the data base
sum(!unique(data$tag_id) %in% fish_db$id)


# 3.2. Assigning station IDs to detections -------------------------------------

# We will assign the station ID to each detection, based on the receiver
# IDs and the time they were in the water (time between "date_in" and 
# "date_out")
head(deploy)

# We will use a "for" loop to iterate over each deployment (each row) and look
# for all the detections that match the same receiver ID and occurred between
# the deployment and retrieval times.

# First, we will create an empty variable in data where we will store the 
# station ID
data$station <- NA
data

# For loop. In each iteration, the "i" variable will be replaced with 
# consecutive elements of the provided vector. In this case, we will provide
# a vector going from 1 to the number of rows in the "deploy" dataframe. The
# loop will start with i = 1, and after finishing it, it will repeat the code
# between the brackets but using i = 2, and so on until arriving to the last
# element.
for (i in 1:nrow(deploy)) {
  
  # To control the progress, this line will print the "i" value in the console
  # and add a new line
  cat(i, "\n") 
  
  # Extract the receiver ID, station, and start and end times of this deployment
  rec <- deploy$receiver_id[i]
  st <- deploy$station_id[i]
  start <- deploy$date_in[i]
  end <- deploy$date_out[i]
  
  # Find the detections that match using logical operators
  indx <- data$receiver == rec & data$date_time > start & data$date_time < end
  
  # Finally, assign the station
  data$station[indx] <- st
  
}

data


# 3.3. Assigning fish ids to detections ----------------------------------------

# We will use a similar approach to assign the Fish id to each detetection, 
# based on the tag ID, the capture date, and, when needed, the recaptured date.
# Moreover, when the fish has a sensor tag, we will also calibrate the data
# using the "intercept" and "slope" values of tag specifications in this
# formula: calibrated_val = slope + raw_val * slope

# First, we will create a new variable to store the fish id
data$fish_id <- NA

# And another to store the type od sensor data (for tags with sensors)
data$sensor_type <- NA


# For loop, iterating over every row in the fish metadata
for (i in 1:nrow(fish_db)) {
  
  # To control the progress, this line will print the "i" value in the console
  # and the total number of rows
  cat(i, "/", nrow(fish_db), "\n", sep = "") 
  
  # Extract the relevant information
  tag <- fish_db$tag_id[i]
  id <- fish_db$fish_id[i]
  capt <- fish_db$tag_date[i]
  recapt <- fish_db$recapture_date[i]
  sensor <- fish_db$sensor[i]
  
  # Find the data that match the individual and the capture date
  indx <- data$tag_id == tag & as.Date(data$date_time) >= capt
  
  # If the fish was recaptured, set indx to FALSE in dates after the recapture
  if (!is.na(recapt)) {
    indx <- indx & as.Date(data$date_time) <= recapt
  }
  
  # Assign ID to the detections
  data$fish_id[indx] <- id
  
  # If there is sensor data, calibrate it and add sensor value information
  if (!is.na(sensor)) {
    inter <-  fish_db$sensor_intercept[i]
    slope <- fish_db$sensor_slope[i]
    data$sensor_val[indx] <- inter + data$sensor_val[indx] * slope
    data$sensor_type[indx] <- sensor
  }
  
}

# Now we have our data ready to be analyzed!
data

# This is something personal, I like to order the columns
data <- data[, c("date_time", "station", "receiver", "fish_id", "tag_id",
                 "protocol", "sensor_val", "sensor_type")]


# We can export this data to a file to use it in the future.

# Create new directory
dir.create("./exports/", showWarnings = FALSE)

# As when loading the files, we can export them using the "write.table" or
# "write.csv" functions from base R, or the "fwrite" function from the
# data.table package.

# Compare the speeds

# t <- Sys.time()
# write.csv(data, "./exports/detection_data_20230601.csv")
# Sys.time() - t

# t <- Sys.time()
# fwrite(data, "./exports/detection_data_20230601.csv")
# Sys.time() - t

# Moreover, fread also allows to export the data in a compressed format to save
# disk space (takes a bit longer to write and read, but compare the file sizes!)
t <- Sys.time()
fwrite(data, "./exports/detection_data_20230601.csv.gz")
Sys.time() - t

# Another nice thing of fwrite is that it keeps the time format, so when data
# is loaded using "fread", the correct format will be automatically assigned
# (by default set to UTC, but we can specify it with an attribute)
data_new <- fread("./exports/detection_data_20230601.csv.gz")
class(data_new$date_time)

# Export fish database
fwrite(fish_db, file = "./exports/fish_database.csv")

# Export deployments
fwrite(deploy, "./exports/deployment_database.csv")

# Another export option is to use the base "save" or "saveRDS" functions in R.
# They will store the objects as R binary objects that will keep all the
# formats, but they can only be read using R

# Save allows to save several objects in one file, but then they will be loaded
# with the same names. This function automatically compresses the data
save(data, deploy, fish_db, file = "./exports/all_data_together.rda")
load("./exports/all_data_together.rda")

# "saveRDS" is also used to save single objects. It also allows different levels 
# of compression, being "xz" the most compressed one. ".rds" file are loaded
# using the "readRDS" function, and assigned to an object.
saveRDS(data, file = "./exports/detection_data_20230601.rds", compress = "xz")
data_new <- readRDS("./exports/detection_data_20230601.rds")

