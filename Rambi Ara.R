library(sf)
library(rgdal)
ramb= readOGR(
  "c:/Users/ASUS/OneDrive/Documents/Rambiara[65]/Rambiara_watershed.shp")

sf_ramb = spTransform(ramb,
                      CRS(
                        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                      ))

library(tidyverse)
ggplot(ramb) + geom_sf()
library(elevatr)
elevation_ramb <- elevatr::get_elev_raster(locations =sf_ramb, z = 12, clip = "locations")
library(raster)
library(rgdal)
spatialref <-
  "+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

pr3 <- projectExtent(elevation_ramb, spatialref)
# Adjust the cell size 
res(pr3) <- 20
# now project
rep_elev <- projectRaster(elevation_ramb,pr3)
raster_to_matrix(rep_elev)
# we can crop the elevation data as per the boundaries of the shape files,
#e,g crop(elevation_USA,sf_State) will cut the USA elevation data as per the state mentioned

elevate <-  as.data.frame(elevation_ramb, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ] 



library(marmap)
library(MetBrewer)
gg_rambi= ggplot() +
  geom_polygon(data = sf_ramb,
               aes(x = long, y = lat),
               color = "white",
               alpha = 0.5) +
  theme_classic() +
  theme(legend.key.size = unit(0.6, "cm")) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value))  +
  labs(title = "Elevation Map of Rambi-Ara Watershed",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") 
gg2 = gg_rambi + 
  scale_fill_gradientn(colors=c("lightgreen","red","skyblue","white","orange"))


library(rayshader)

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

setwd("c:/Users/ASUS/OneDrive/Desktop/")


render_movie("Rambiara.mp4",frames=720)

slope <- terrain(elevation_ramb, opt='slope')
aspect <- terrain(elevation_ramb, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=F)
plot(elevation_ramb, col=rainbow(25, alpha=0.35), add=TRUE)


rambi_mat = raster_to_matrix(elevation_ramb)
rambi_mat %>%
  sphere_shade(texture="imhof2") %>%
  plot_3d(rambi_mat, zscale=50, water = TRUE, watercolor="imhof2", 
          waterlinecolor="white", waterlinealpha=0.5)



#------------------------------------------------------------------------------    
pal="Homer1"
Colors=met.brewer(pal)

ramb_gg=rambi_mat %>% 
  height_shade(texture = grDevices::colorRampPalette(Colors)(500)) %>%
  add_shadow(ray_shade(rambi_mat), 0.5) %>%
  add_shadow(ambient_shade(rambi_mat), 0) %>%
  plot_3d(
    rambi_mat,
    zscale = 50,
    fov = 0,
    theta = 135,
    zoom = 0.3,
    background = "lightblue"
  )               

