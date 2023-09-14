# Load the required library
library(readr)
file = "d:/usa/txt files/asos-stations.txt" 

# Specify the column widths, you do this by manually calculating their respective character widths
column_widths <- c(9, 6, 7, 5, 31, 31, 21, 3, 31, 10, 11, 7, 6, 51, 9, 12, 7, 2)

# Read the fixed-width formatted data
asos_stations <- read.fwf(file = file, widths = column_widths,fill = T,skip = 2) 

# Define column names based on your provided data
col_names <- c("NCDCID", "WBAN", "COOPID", "CALL", "NAME", "ALT_NAME", "COUNTRY", "ST", "COUNTY",
               "LAT", "LON", "ELEV", "UTC", "STNTYPE", "BEGDT", "GHCND", "ELEV_P", "ELEV_A")

# Set the column names
colnames(asos_stations) <- col_names

# Save as CSV
write.csv(asos_stations,file = "asos.csv")

# Repeat the same for other two files as per their column widths!!!!!!!!!!!!!!!!

