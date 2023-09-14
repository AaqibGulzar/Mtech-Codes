library(reshape2)
library(rayshader)
library(dplyr)
library(ggplot2)
library(rgdal)
#Contours and other lines will automatically be ignored. Here is the volcano dataset:

bigvo = resize_matrix(volcano, scale=5, method="cubic")

vol_df=bigvo %>%
  melt()

ggvolcano = 
  ggplot(vol_df) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_contour(aes(x = Var1, y = Var2, z = value), color = "black") +
  scale_x_continuous("X", expand = c(0, 0)) +
  scale_y_continuous("Y", expand = c(0, 0)) +
  scale_fill_gradientn("Z", colours = terrain.colors(10)) +
  coord_fixed()

plot_gg(ggvolcano,zoom=0.3)


par(mfrow = c(1, 2))
library(rgl)
rgl.open()
plot_gg(ggvolcano, width = 7, height = 4, raytrace = FALSE, preview = TRUE)
plot_gg(ggvolcano, multicore = TRUE, raytrace = TRUE, width = 7, height = 4,
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
Sys.sleep(0.2)

render_snapshot(clear = TRUE)
