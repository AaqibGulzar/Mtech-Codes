library(tidyverse)
library(elevatr)
library(raster)
library(sf)
library(mapview)
sf_ARW<- st_read(
  "d:/R Programming/Shape file/american water/American_River_Watershed.shp"
)

elevation_ARW <- elevatr::get_elev_raster(locations = sf_ARW, z = 7, clip = "locations")


Elevate <-  as.data.frame(elevation_ARW, xy = TRUE)
colnames(Elevate)[3] <- "elevation_value"
Elevate <- Elevate[complete.cases(Elevate), ]       


Station =c("SCN", "HYS" ,"BL2" ,"GKS" ,"ALP", "SGP", "ADR","FDL","PIH")

lat=
  c(38.747002,
    39.281422,
    39.283001,
    39.07468,
    38.804192,
    39.123974,
    38.882,
    38.533001,38.831699)

long=c(
  -120.068001,
  -120.529633,
  -120.699997,
  -120.5616,
  -120.215652,
  -120.7612,
  -121.044998,
  -120.699997,-121.009201)


precip_stations=data.frame(Station,lat,long)


 gg_AWR=ggplot() + geom_sf(data = sf_ARW) +
  geom_raster(data = Elevate, aes(x = x, y = y, fill = elevation_value))  +
  labs(title = "elevation",
       x = "long",
       y = "lat",
       fill = "Elevation(m)")
 
gg_colr <-   scale_fill_gradientn(colours = met.brewer("Benedictus"),breaks=seq(0,3200,400))


gg <- gg_AWR + geom_point(
  data = precip_stations,aes(x = long,y = lat),color = "red",size=3)

gg + gg_colr


