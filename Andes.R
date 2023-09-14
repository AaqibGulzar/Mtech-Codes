library(rayshader)
library(tidyverse)
library(raster)
library(sf)
library(elevatr)
library(raster)
library(rgdal)
andes= readOGR(
  "d:/R Programming/Shape file/RegionAndina/Region Andina.shp")
elevation_andes <- elevatr::get_elev_raster(locations =andes, z = 5, clip = "locations")

elevate <-  as.data.frame(elevation_andes, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ] 



my_colors <- RColorBrewer::brewer.pal(8, "RdYlBu")
gg_andes= ggplot() +
  geom_polygon(data = andes,
               aes(x = long, y = lat,group = group),
               color = "white",
               alpha = 0.5) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value),show.legend=F)  +
  labs(title = "Elevation Map of Andes Range",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") + theme_classic()


gg_andes =  ggplot() +
  geom_polygon(data = andes,
               aes(x = long, y = lat,group = group),
               color = "white",
               alpha = 0.5) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value),show.legend=T)+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 7, color = "grey10"),
    legend.text = element_text(size = 5, color = "grey10"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white", size = 0),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
  ) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "",
    y = ""
  )

gg2 = gg_andes +
  scale_fill_gradientn(
  colours =c("#D73027","#F46D43","#FDAE61","#FEE090","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"))
gg2  
  # marmap::scale_fill_etopo() 
  # scale_fill_gradientn(colors=c("lightgreen","red","skyblue","white","orange"))


# cool =  gg_andes + marmap::scale_fill_etopo() 

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






#setwd("c:/Users/ASUS/OneDrive/Desktop/")

#render_movie("Andes.mp4",frames=1080)

elmat = raster_to_matrix(elevation_andes)
elmat %>%
  sphere_shade(texture="unicorn") %>%
  plot_3d(
    elmat,
    zscale = 20,
    windowsize = c(1200, 1200),
    zoom = 0.75,
    phi = 89,
    theta = 0,
    fov = 0,
    background = "dodgerblue4"
  )











render_highquality(lightdirection = 45, lightaltitude=60,
                  ground_material = rayrender::diffuse(checkerperiod = 30, checkercolor="grey50"))

#ggsave("Andes.png",dpi = 800,width=35,height=20,units="cm")


render_highquality(lightdirection = c(240), lightaltitude=30, 
                   lightcolor=c("#5555ff"),
                   scene_elements = rayrender::sphere(z=0,y=15, x=-18, radius=5,
                                                      material=rayrender::light(color="red",intensity=10)))

volcano %>%
  sphere_shade() %>%
  plot_3d(volcano,zscale = 2)

render_highquality() 

render_highquality(filename=".png",width=600,height=600)

