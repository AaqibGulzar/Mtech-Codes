# Load the required library
library(readr)
file1 = "d:/usa/txt files/asos-stations.txt" 

# Specify the column widths
column_widths1 <- c(9, 6, 7, 5, 31, 31, 21, 3, 31, 10, 11, 7, 6, 51, 9, 12, 7, 2)

# Read the fixed-width formatted data
asos_stations <- read.fwf(file = file1, widths = column_widths1,fill = T,skip = 2) 

# Define column names based on your provided data
col_names <- c("NCDCID", "WBAN", "COOPID", "CALL", "NAME", "ALT_NAME", "COUNTRY", "ST", "COUNTY",
               "LAT", "LON", "ELEV", "UTC", "STNTYPE", "BEGDT", "GHCND", "ELEV_P", "ELEV_A")

# Set the column names
colnames(asos_stations) <- col_names

# Save as CSV
getwd()
setwd("d:/usa/txt files/")
write.csv(asos_stations,file = "asos.csv")


file2 = "d:/usa/txt files/coop-stations (1).txt"
column_widths2 = c(9, 7, 9, 16, 31, 21, 3, 51, 3, 41, 16, 16, 11, 9, 7, 4, 11, 4, 2, 1)
coop = read.fwf(file = file2,
                widths = column_widths2,
                fill = T)
write.csv(coop, file = "coop.csv")

file3 = "d:/usa/txt files/lcd-stations.txt"
column_widths3 = c(6, 11, 21, 11, 4, 6, 9, 7, 12, 51, 61, 26, 3, 3, 11, 10, 11, 7, 7, 3)
lcd = read.fwf(file = file3,
               widths = column_widths3,
               fill = T)
write.csv(lcd, file = "lcd.csv")


