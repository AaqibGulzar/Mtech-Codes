# Load the necessary libraries
library(utils)
library(tidyverse)
library(foreach)
# Register a parallel backend, e.g., doParallel, to utilize multiple cores
library(doParallel)

getwd()
setwd("d:/Text daa/")


#=================       Not Needed   ============== |> Just for Cross checking <|====================================
domain = "https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv/2017/001/ptv17001.00m"
"https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv/2017/365/ptv17365.23m"
#<><><><><><><><><><><>><><><><<><><><><>><><<><><><><<><><><><>>>><><><><><><><><>>><>><><><><><><><><>>>>><><><><<>><.



data_down <- function(year, day) {
  if (day < 10) {
    day <- paste0("00", day)
  } else if (day >= 10 & day <= 99) {
    day <- paste0("0", day)
  } else {
    day <- as.character(day)
  }
  
  base_url <- "https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv"  # base URL
  
  url1 <- paste0(base_url,
                 "/",
                 year,
                 "/",
                 day,
                 "/ptv17",
                 day) # compile the URL
  
  numbers <- seq(0,0.23,0.01) # hourly data,0 to 23
  
  formatted_numbers <-
    sprintf("%.2f", seq(0,0.23,0.01))  # Most important, eliminate the trailing zeros
  # otherwise wrong URL formation takes place
  
  urls <- paste0(url1, sub("^0", "", formatted_numbers))  # combine the two
  
  url_down <- paste0(urls, "m")
  
  tmean <- numeric()
  psum <- numeric()
  for (i in 1:length(url_down)) {
    temp_file <- tempfile()   # create a temporary file
    
    tryCatch({
      #overwrite the file, loop over all URLS
      download.file(url_down[i], temp_file)
      
      data <- read.table(temp_file, header = FALSE, sep = ",")   # read the text
      tmean[i] <- mean(data$V5)
      psum[i] <- sum(data$V8)
    }, error = function(e) {
      # Handle errors (e.g., missing URL) here
      # Print a message or take other actions
      cat("Error for day", day, ":", conditionMessage(e), "\n")
      # Set tmean and psum to NA
      tmean[i] <- NA
      psum[i] <- NA
    })
  }
  
  df1 <- data.frame(year, day, tmean, psum)
  df <- data.frame(year, day = as.numeric(day), tmean = mean(df1$tmean, na.rm = TRUE), psum = sum(df1$psum, na.rm = TRUE))
  
  return(df)
}


#<><><><><><><><><><><><><><><><> Parallel Computing <><><><><><><><><><><><><><>

# Set the number of cores to use (adjust as needed)
 num_cores <- 8
# You can change this number based on your system
 
# Register parallel backend
 cl <- makeCluster(num_cores)
 registerDoParallel(cl)
 
# Create an empty dataframe to store the results
 all_data <- data.frame(year = numeric(),
                        day = numeric(),
                        tmean = numeric(),
                        psum = numeric())

all_data <- foreach(day = 1:365, .combine = 'rbind') %dopar% {
  # Call the data_down function for each day
  result <- data_down(year = 2017, day = day)
  result
}

write.csv(all_data,"year Potter Valley.csv")

stopCluster(cl)


# If you want to use a simple  for loop

# Create an empty list to store the results for all days, e,g 2 here
results_list <- list()

for (day in 1:2) {
  result <- data_down(year = 2017, day = day)
  results_list[[day]] <- result
}

# Now you have the results for all days stored in the 'results_list' list.



