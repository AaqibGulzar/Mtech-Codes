library(tidyverse)
library(ggside)
library(tidyquant)
mpg %>%
  ggplot(aes(hwy, cty, color = class)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(aes(color = NULL), se = TRUE) +
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = class),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(x = after_stat(density), fill = class),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  scale_color_tq() + scale_fill_tq() + theme_tq() +
  labs(
    title = "Fuel Economy by Vehicle Type",
    subtitle = "ggside density",
    x = "Highway MPG",
    y = "City MPG"
  ) + theme(ggside.panel.scale.x = 0.4,
            ggside.panel.scale.y = 0.4) + theme_ggside_minimal()


p <-
  ggplot(iris,
         aes(Sepal.Width, Sepal.Length, color = Species, fill = Species)) +
  geom_point()
#sidebar - uses StatCount
p +
  geom_xsidebar() +
  geom_ysidebar()
#sidecol - uses Global mapping
p +
  geom_xsidecol() +
  geom_ysidecol()
df <- expand.grid(UpperCase = LETTERS, LowerCase = letters)
df$Combo_Index <- as.integer(df$UpperCase) * as.integer(df$LowerCase)
p1 <- ggplot(df, aes(UpperCase, LowerCase)) +
  geom_tile(aes(fill = Combo_Index))
#sideboxplots
p1 + geom_xsideboxplot(aes(y = Combo_Index)) +
  geom_ysideboxplot(aes(x = Combo_Index)) +
  #when mixing continuous/discrete scales
  #use the following helper functions
  scale_xsidey_continuous() +
  scale_ysidex_continuous() + scale_fill_viridis_c()

ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(size = 2) +
  geom_xsidedensity() +
  geom_ysidedensity() +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(size = 2) +
  geom_xsidedensity(aes(y = after_stat(count)), position = "stack") +
  geom_ysidedensity(aes(x = after_stat(scaled))) +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4) + theme_ggside_minimal()

ggplot(diamonds, aes(price, carat, colour = cut)) +
  geom_point() +
  geom_xsidefreqpoly(aes(y=after_stat(count)),binwidth = 500) +
  geom_ysidefreqpoly(aes(x=after_stat(count)),binwidth = .2)



ggplot(mpg) +   
  geom_density(aes(x = hwy,y = after_stat(density),
        fill = class),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) + theme_classic()

x<- rweibull(100, 2.6, 3)
y<- rweibull(100, 1.8, 3)
xy.df<- data.frame(cbind(x,y))
p <- ggplot(xy.df, aes(x, y)) +
  geom_point(colour = "blue", size = 0.25) +
  geom_density2d() +
  geom_xsidedensity(fill = "blue", alpha = .3) +
  geom_ysidedensity(fill = "blue", alpha = .3) +
  stat_xsidefunction(fun = dweibull, args = list(shape = 1.8, scale = 3), colour = "red") +
  stat_ysidefunction(fun = dweibull, args = list(shape = 2.6, scale = 3), colour = "red") +
  theme_classic()
library(palmerpenguins)
penguins %>%
  ggplot(aes(flipper_length_mm,body_mass_g, color = species)) +
  geom_point() + theme_classic() + 
  geom_xsidedensity(
    aes(y = after_stat(density),
        fill = species),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  geom_ysidedensity(
    aes(x = after_stat(density), fill = species),
    alpha = 0.5,
    size = 1,
    position = "stack"
  ) +
  scale_color_tq() + scale_fill_tq() + theme_tq() +
  labs(
    title = "Relation between Body mass and flipper length of Penguins",
    subtitle = "ggside density",
    x ="Flipper Length(mm)",
    y = "Body Mass(g)"
  ) + theme(ggside.panel.scale.x = 0.4,
            ggside.panel.scale.y = 0.4) + theme_ggside_minimal()






