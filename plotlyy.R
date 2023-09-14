library(dplyr)
library(Hmisc)
regression_data.2$ries.skew = skew_compare$Reis_skew


pvalues =rcorr(as.matrix(regression_data.2))$P %>% round(digits=3)

getwd()
setwd("d:/Project/Excel Files/")

write.csv(pvalues,"p.csv")
correlation.matrix.new = cor(regression_data.2)
write.csv(correlation.matrix.new,"cm.csv")

library(plotly)
p1 = plot_ly(data=regression_data.2,
        y = ~ Aaqib_skew,
        type= "scatter",
        mode = "lines+markers",
        name = "Aaqib skew")
p1 %>% add_trace(y = ~ ries.skew,
                 type = "scatter",
                 mode = "lines+markers",
                 color = "red",
                 name = "Ries skew")
library(palmerpenguins)
g = ggplot(penguins) + geom_bar(aes(x = species,fill = island)) 
ggplotly(g,
         dynmaicTicks = F,
         )

ggpenguins <- ggplot(data=penguins, aes(x =bill_length_mm,y =body_mass_g,color = species)) + geom_point()
ggplotly(ggpenguins)

data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
ggplotly(viz, tooltip = c("text", "size"))



# more brushing (i.e. highlighting) examples
demo("crosstalk-highlight-ggplotly", package = "plotly")

# client-side linked brushing in a scatterplot matrix
highlight_key(palmerpenguins::penguins) %>%
  GGally::ggpairs(aes(colour = Species), columns = 1:4) %>%
  ggplotly(tooltip = c("x", "y", "colour")) %>%
  highlight("plotly_selected")








library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))

fig


data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
data_2007$size <- data_2007$pop
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

fig <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors,
               marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
               text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                             '<br>Pop.:', pop))
fig <- fig %>% layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
                      scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(2.003297660701705, 5.191505530708712),
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwidth = 2),
                                   yaxis = list(title = 'Life Expectancy (years)',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                range = c(36.12621671352166, 91.72921793264332),
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2),
                                   zaxis = list(title = 'Population',
                                                gridcolor = 'rgb(255, 255, 255)',
                                                type = 'log',
                                                zerolinewidth = 1,
                                                ticklen = 5,
                                                gridwith = 2)),
                      paper_bgcolor = 'rgb(243, 243, 243)',
                      plot_bgcolor = 'rgb(243, 243, 243)')

fig






