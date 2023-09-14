library(rayshader)
library(tidyverse)
library(raster)
library(sf)
library(elevatr)
library(raster)
library(rgdal)
jhelum= readOGR(
  "c:/Users/ASUS/OneDrive/Documents/Shakir/jhelum s f/Jhelum.shp")

sf_jhelum = spTransform(jhelum,
                      CRS(
                        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                      ))

elevation_jhe <- elevatr::get_elev_raster(locations =sf_jhelum, z = 10, clip = "locations")

elevate <-  as.data.frame(elevation_jhe, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ] 



my_colors <- RColorBrewer::brewer.pal(8, "RdYlBu")



library(readr)
X3_jhelum_discharge_stations <- read_csv("~/Shakir/3  jhelum discharge stations.csv")

X6_stations <- read_csv("~/Shakir/6 stations.csv")
panel.grid = element_line(colour = "grey", linetype = "dashed", linewidth = 0.1)

ggplot() + 
  geom_polygon(data = sf_jhelum,
               aes(x = long, y = lat, group = group),
               color = "white",
               alpha = 0.5) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value)) +
  labs(title = "Elevation Map of Jhelum Basin", 
       x = "Longitude",
       y = "Latitude",
       fill = "Elevation(m)") +
  scale_fill_gradientn(
    colours =c("#D73027","#F46D43","#FDAE61","#FEE090","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"))+
  theme(panel.background = element_rect("white"),
        panel.grid.major = element_line("lightgrey"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 15),
        panel.grid = element_line(colour = "grey", linetype = "dashed", linewidth = 0.1)) +
  geom_point(data = X3_jhelum_discharge_stations, aes(x = Longitude, y = Latitude, shape = "Stream Gauging Stations"), size = 4) +
  geom_point(data = X6_stations, aes(x = Longitude, y = Latitude, shape = "Meteorological Stations"), size = 4) +
  geom_text(data = X6_stations, aes(x = Longitude, y = Latitude, label = Station), hjust = -0.2, vjust = 0.2, size = 3, color = "black") +
  geom_text(data = X3_jhelum_discharge_stations, aes(x = Longitude, y = Latitude, label = Station), hjust = -0.2, vjust = 0.2, size = 3, color = "black") 

#ggsave("Shakir.png",dpi=700,width=35,height=25,units="cm") 

plot_gg(
  gg2,
  zoom = 0.5,
  pointcontract = 1,
  scale = 100,
  width = 5,
  height = 5,
  phi = 30,
  windowsize = c(1920, 1080))


rayshader::render_highquality(
  filename = "take_1.png",
  lightintensity = c(750, 500),
  lightdirection = c(135, 115),
  lightaltitude = c(30, 45),
  preview = T,
  parallel = T,
  interactive = F,
  obj_material = rayrender::diffuse(color="steelblue")
)
