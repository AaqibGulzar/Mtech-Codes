library(rayshader)
library(ambient)
library(tidyverse)



montereybay %>% 
  sphere_shade(texture = "desert") %>%
  height_shade(texture = grDevices::colorRampPalette(Colors)) %>% 
  add_water(detect_water(montereybay),color = "imhof3") %>% 
  add_shadow(ray_shade(montereybay),0.5) %>% 
  add_shadow(ambient_shade(montereybay),0) %>%
  plot_3d(montereybay,zscale = 50,fov = 0,theta = 135,
         zoom = 0.75,phi = 45,windowsize = c(1000,8000),background = "lightblue")



render_clouds(
  montereybay,
  zscale = 10,
  start_altitude = 800,
  end_altitude = 1000,
  attenuation_coef = 2,
  clear_clouds = T
)

render_camera(
  fov = 0,
  theta = 60,
  zoom = 0.75,
  phi = 45
)
render_scalebar(
  limits = c(0, 5, 10),
  label_unit = "km",
  position = "W",
  y = 50,
  scale_length = c(0.33, 1)
)
render_compass(position = "E")

render_snapshot(clear = T)

#------------------------------------------------------------------------------    
pal="Homer1"
Colors=met.brewer(pal)

montee=montereybay %>% 
  height_shade(texture = grDevices::colorRampPalette(Colors)(500)) %>%
  add_shadow(ray_shade(montereybay), 0.5) %>%
  add_shadow(ambient_shade(montereybay), 0) %>%
  plot_3d(
    montereybay,
    zscale = 50,
    fov = 0,
    theta = 135,
    zoom = 0.3,
    background = "lightblue"
  )               
 

#plot_gg tutorial.
library(sf)
fname <- system.file("shape/nc.shp", package = "sf")
nc <- st_read(fname)
library(MetBrewer)
nc_gg = ggplot(data = nc) + geom_sf(aes(fill = AREA), color = "black") + scale_fill_viridis_c()

plot_gg(
  nc_gg,
  multicore = T,
  width = 6,
  height = 2.7,
  fov = 70,
  zoom = 0.4
)
render_depth(focallength = 100, focus = 0.4)
render_movie()


flightsfeb= flights %>% filter(month == 2)
view(flightsfeb)

















