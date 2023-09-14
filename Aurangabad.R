library(sf)
library(rgdal)
library(raster)
library(marmap)
library(MetBrewer)
library(rayshader)

sf_ramb= readOGR("~/usman reg_files/Aurangabad/Aurangabad.shp")


elevation_ramb <- elevatr::get_elev_raster(locations =sf_ramb, z = 11, clip = "locations")


elevate <-  as.data.frame(elevation_ramb, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ] 



gg_rambi = ggplot() +
  geom_polygon(data = sf_ramb,
               aes(x = long, y = lat),
               color = "white",
               alpha = 0.5) +
  theme_classic() +
  theme(legend.key.size = unit(0.6, "cm")) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value))  +
  labs(title = "Elevation Map of Aurangabad",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") 

gg2 = gg_rambi + 
  scale_fill_gradientn(colors=c("gold","red","green","dodgerblue4","snow"),breaks = seq(0,1000,200))



plot_gg(
  gg2,
  zoom = 0.4,
  pointcontract = 1,
  scale = 100,
  width = 6,
  height = 4,
  phi = 30,
  windowsize = c(1920, 1080)
)



render_movie("Aurangabad.mp4",frames=720)


