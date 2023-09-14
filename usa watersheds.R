# read shpfile:
library(rgdal)
library(ggplot2)
library(maps)
library(sf)
library(terra)
library(ggspatial)
us_shape <- map_data("state")

finalshapes<-readOGR("d:/usa/selectedshps.shp") ##read watersheds shapefile
filtered_stations=read.csv('D:/usa/filtered_stations.csv', header=T)

filtered_stations=data.frame(filtered_stations)


# Reproject the SpatialPolygonsDataFrame to WGS84 (EPSG:4326)
finalshapes_wgs84 <- spTransform(finalshapes, CRS("+proj=longlat +datum=WGS84"))

# Convert the SpatialPolygonsDataFrame to a data frame
finalshapes_df <- fortify(finalshapes_wgs84)


ggplot() +
  geom_polygon(
    data = us_shape,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = "black"
  ) +
  geom_point(
    data = filtered_stations,
    aes(x = Long,
        y = Lat),
    color = "darkred",
    alpha = 0.5,
    size = 0.8
  ) +
  geom_polygon(data = finalshapes_df,
               aes(x = long, y = lat, group = group),color = "skyblue",alpha = 0.7) +
                 coord_fixed() + theme_minimal()  # Use equal aspect ratio (aspect ratio = 1)
               
