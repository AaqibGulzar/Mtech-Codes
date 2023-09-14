world_map = ggplot() + 
  borders("world",fill="white",colour = "gray80")
world_map + 
  geom_point(data=world.cities,
             aes(x=long,y = lat),
             colour = "blue",
             size = 0.003)

library(tidyverse)
library(maps)
wc2 = world.cities[1:10, ]
ggp = ggplot(data = wc2, mapping = aes(x = long, y = lat, color = pop))  +
  geom_point() +
  scale_color_viridis_c() +
  geom_text(
    label = wc2$name,
    nudge_x = 0.75,
    nudge_y = 0.75,
    check_overlap = T,
    color = "black"
  )


plot_gg(ggp)
