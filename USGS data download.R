# Load the url package to access the download.file function
library(url)

# Set the URL you want to download
url2 <- "https://nwis.waterdata.usgs.gov/al/nwis/peak?site_no=02412000&agency_cd=USGS&set_logscale_y=1&date_format=YYYY-MM-DD&rdb_compression=file&format=hn2&hn2_compression=value&submitted_form=brief_list"

# Download the contents of the URL and save them as a text file
download.file(url2, destfile = "output.txt", quiet = FALSE, mode = "wb")


# Create a vector of site numbers
sites <- c("02412000", "02413475")

# Loop through each site and download the data
for (site in sites) {
  url <- paste0("https://nwis.waterdata.usgs.gov/al/nwis/peak?site_no=", site, "&agency_cd=USGS&set_logscale_y=1&date_format=YYYY-MM-DD&rdb_compression=file&format=hn2&hn2_compression=value&submitted_form=brief_list")
  destfile <- paste0(site, ".txt")
  download.file(url, destfile = destfile, quiet = FALSE, mode = "wb")
}
