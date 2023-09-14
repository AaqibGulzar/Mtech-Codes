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


Archambault, Austria, Benedictus, Cassatt1, Cassatt2, Cross, Degas, Demuth, Derain,
Egypt, Gauguin, Greek, Hiroshige, Hokusai1, Hokusai2, Hokusai3, Homer1, Homer2, Ingres, 
Isfahan1, Isfahan2, Java, Johnson,Juarez, Kandinsky, Klimt, Lakota, Manet, Monet, Moreau,
Morgenstern, Nattier, Navajo, NewKingdom, Nizami, OKeeffe1, OKeeffe2, Paquin, Peru1, Peru2,
Pillement, Pissaro, Redon, Renoir, Signac, Tam, Tara, Thomas, Tiepolo, Troy, Tsimshian, 
VanGogh1, VanGogh2, VanGogh3, Veronese, and Wissing

my_colors <- RColorBrewer::brewer.pal(8, "RdYlBu")

c2 = RColorBrewer::brewer.pal(10,"RdYlGn")

gg = ggplot() + 
  geom_polygon(data = sf_jhelum,
               aes(x = long, y = lat, group = group),
               color = "white",
               alpha = 0.5) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value)) +
  labs(title = "",
       x = "",
       y = "",
       fill = "Elevation(m)") +
  scale_fill_gradientn(colors= rev(c2)) + theme(legend.position="none",
                                                 panel.background=element_blank(),
                                                 axis.text=element_blank(),
                                                 axis.ticks=element_blank())

gg

plot_gg(
  gg,
  zoom = 0.5,
  pointcontract = 1,
  scale = 50,
  width = 6,
  height = 5,
  phi = 30,
  windowsize = c(1920, 1080),
  raytrace=T)


rayshader::render_highquality(
  filename = "KSH.png",
  lightintensity = c(750, 500),
  lightdirection = c(135, 115),
  lightaltitude = c(30, 45),
  preview = T,
  parallel = T,
  interactive = F,
  obj_material = rayrender::diffuse(color="steelblue")
)
