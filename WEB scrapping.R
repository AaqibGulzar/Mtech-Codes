# Load the necessary libraries
library(utils)
library(tidyverse)
getwd()
setwd("d:/Text daa/")


#================= Not Needed============== |> Just for Cross checking <|====================================
domain = "https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv/2017/001/ptv17001.00m"
         "https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv/2017/365/ptv17365.23m"



data_down = function(year, day) {
  if (day < 10) {
    day = paste0("00", day)
  } else if (day >= 10 & day <= 99) {
    day = paste0("0", day)
  } else {
    day = day
  }
  
  
  base_url = "https://downloads.psl.noaa.gov/psd2/data/realtime/CsiDatalogger/SurfaceMet/ptv"  # base URL
  
  #formatted_day = sprintf("%.3s",day)
  
  url1 = paste0(base_url,
                "/",
                2017,
                "/",
                day,
                "/ptv17",
                day) # compile the URL
  
  numbers <- seq(0,0.23,0.01) # hourly data,0 to 23
  
  formatted_numbers <-
    sprintf("%.2f", seq(0,0.23,0.01))  # Most important, eliminate the trailing zeros
  # otherwise wrong URL formation takes place
  
  urls <- paste0(url1, sub("^0", "", formatted_numbers))  # combine the two
  
  url_down = paste0(urls, "m")
  
  tmean = numeric()
  psum = numeric()
  for (i in 1:length(url_down)) {
    temp_file <- tempfile()   # create a temporary file
    
    #overwrite the file, loop over all URLS
    download.file(url_down[i], temp_file)
    
    data = read.table(temp_file, header = FALSE, sep = ",")   # read the text
    tmean[i] = mean(data$V5)
    psum[i] = sum(data$V8)
    
  }
    df1 = data.frame(year,day,hour = 0:23,tmean,psum)
    #write.csv(data, paste0(year, day, i, ".csv") , row.names = FALSE)  # write as csv and save as unique files
    df = data.frame(year,day = as.numeric(day), tmean = mean(df1$tmean),psum =  sum(df1$psum))
    
    return(df)
}


data_down(year = 2017,day = 1)


library(foreach)

# Register a parallel backend, e.g., doParallel, to utilize multiple cores

library(doParallel)

# Set the number of cores to use (adjust as needed)
num_cores <- 12  # You can change this number based on your system

# Register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create an empty dataframe to store the results
all_data <- data.frame(year = numeric(),
                       day = numeric(),
                       tmean = numeric(),
                       psum = numeric())

# Loop through all 365 days in parallel
all_data <- foreach(day = 1:365, .combine = 'rbind') %dopar% {
  # Call the data_down function for each day
  result <- data_down(year = 2017, day = day)
  result
}

# Stop the parallel backend
stopCluster(cl)

# Reset row names
rownames(all_data) <- NULL



