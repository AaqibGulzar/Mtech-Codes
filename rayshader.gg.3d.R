# first 3d plot
library(rayshader)
library(ggplot2)
library(tidyverse)

gg = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)
render_camera()

#second plot

mtcars_gg = ggplot(mtcars) + 
  geom_point(aes(x=mpg,color=cyl,y=disp),size=2) +
  scale_color_continuous(limits=c(0,8)) +
  ggtitle("mtcars: Displacement vs mpg vs # of cylinders") +
  theme(title = element_text(size=8),
        text = element_text(size=12)) 

plot_gg(mtcars_gg, height=3, width=3.5, multicore=TRUE, pointcontract = 0.7, soliddepth=-200)

#third plot
a = data.frame(x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b = data.frame(x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c = data.frame(x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data = rbind(a,b,c)

#Lines
pp = ggplot(data, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")
plot_gg(pp, width = 4, height = 4, scale = 300, multicore = TRUE)

#No lines
pp_nolines = ggplot(data, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0) +
  scale_fill_viridis_c(option = "C")
plot_gg(pp_nolines, width = 4, height = 4, scale = 300, multicore = TRUE)



#4th plot
library(MetBrewer)
d = ggplot(dia2) + geom_point(aes(clarity, color, color = carat),
                              size = 6,
                              shape = 11) + scale_color_gradientn(colours = met.brewer("Signac"))
plot_gg(
  d,
  width = 5,
  height = 5,
  scale = 250,
  zoom = 0.3
)











