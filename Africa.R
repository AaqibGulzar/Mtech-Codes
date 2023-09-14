library(rnaturalearth)
library(tidyverse)
africa = ne_countries(continent="africa", returnclass="sf")


dem <- elevatr::get_elev_raster(locations = africa, z = 1, clip = "locations")

elevate <-  as.data.frame(dem, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ] 


# 
# my_colors <- RColorBrewer::brewer.pal(8, "RdYlBu")
# gg = ggplot() +
#   geom_sf(data = africa,
#                alpha = 0.5) +
#   geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value),show.legend=F)  +
#   labs(title = "Elevation Map of Andes Range",
#        x = "long",
#        y = "lat",
#        fill = "Elevation(m)") + theme_classic()
# 
# 
# gg=  ggplot() +
#   geom_sf(data = africa,
#                alpha = 0.5) +
#   geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value),show.legend=T)+
#   theme_minimal() +
#   theme(
#     axis.line = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     legend.position = "top",
#     legend.key.height=unit(0.2,"cm"),
#     legend.key.width=unit(1,"cm"),
#     legend.title = element_text(size = 7, color = "grey10"),
#     legend.text = element_text(size = 5, color = "grey10"),
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(color = "white", size = 0),
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     legend.background = element_rect(fill = "white", color = NA),
#     plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines")
#   ) +
#   labs(
#     title = " Elevation Value Of Africa",
#     subtitle = "",
#     caption = "",
#     x = "",
#     y = ""
#   )
# 
# gg +
#   scale_fill_gradientn(
#     colours =c("#D73027","#F46D43","#FDAE61","#FEE090","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"))


gg = ggplot() +
  geom_sf(data = africa,
          alpha = 0.5) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value),show.legend=T)+
guides(
  fill = guide_legend(
    direction = "horizontal",
    keyheight = unit(1.5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = "top",
    label.position = "bottom",
    title.hjust = .5,
    label.hjust = .5,
    nrow = 1,
    byrow = T
  )
) +
  theme(
    axis.line = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.title = element_text(
      size = 11, color = "grey10"
    ),
    legend.text = element_text(
      size = 10, color = "grey10"
    ),
    plot.title = element_text(
      size = 20, color = "grey10",
      hjust = 0.5, vjust = -1
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(
      c(t = 1, r = 0, l = 0, b = 0), "lines"
    )
  ) +
  labs(
    x = "",
    y = "",
    title = "Elevaion Map of Africa",
    fill= NULL) 



gg + scale_fill_gradientn(colors = cols(10) ,breaks = seq(-1500,3000,500)) 
gg + scale_fill_gradientn(colors = etopo.colors(10) ,breaks = seq(-1500,3000,500)) 
gg + scale_fill_gradient(low="blue",high="orange",breaks = seq(-1500,3000,500)) 
gg + scale_fill_gradientn(colors = grDevices::terrain.colors(10) ,breaks = seq(-1500,3000,500)) 


#ggsave("Africa.jpg",dpi=1500,width=30,height=30,units="cm")

gg1 = gg + marmap::scale_fill_etopo() 

# marmap::scale_fill_etopo() 
# scale_fill_gradientn(colors=c("lightgreen","red","skyblue","white","orange"))


# cool =  gg_andes + marmap::scale_fill_etopo() 

rayshader::plot_gg(
  gg1,
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
