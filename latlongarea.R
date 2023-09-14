library(tidyverse)
library(dataRetrieval)

sites = read.csv("c:/Users/ASUS/OneDrive/Documents/meh maam/site_name.csv")

print(sites)

# Check the number of digits in the site_no column
sites$digit_count <- nchar(as.character(sites$site_no))


# Create a new column based on the digit count
sites <-
  sites %>% mutate(new_site_no = if_else(
    digit_count == 7, 
    as.character(paste0("0", site_no)),
    as.character(site_no)))


# Display the updated dataset
print(sites)

new_sites = sites$new_site_no

site_info = readNWISsite(new_sites)


area_lat_long =  data.frame(site_info$dec_lat_va,
                            site_info$dec_long_va,
                            site_info$drain_area_va)

area_lat_long = cbind(new_sites,area_lat_long)

colnames(area_lat_long) = c("station_id","latitude","longitude","drainage_area")

area_lat_long |> head()


# write.csv(area_lat_long, "Area Lat Long.csv")






