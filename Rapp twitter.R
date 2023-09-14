library(tidyverse)

dat <- gapminder::gapminder

####### For loop approach
slopes <- numeric(5)
names(slopes) <- unique(dat$continent)

for (selected_continent in unique(dat$continent)) {
  filtered_data <- dat |> 
    filter(continent == selected_continent) 
  
  lin_mod <- lm(
    data = filtered_data,
    lifeExp ~ year
  )
  
  slopes[selected_continent] <- coefficients(lin_mod)[2]
}

slopes


##### Using maps instead

nested_data <- dat |> 
  group_by(continent) |> 
  nest() |> 
  ungroup() 

nested_data |> 
  mutate(
    lin_mod = map(
      data,
      function(x) lm(data = x, lifeExp ~ year)
    ),
    coefficients = map(lin_mod, coefficients),
    slope = map_dbl(coefficients, \(x) x[2]),
    slope_short = map_dbl(coefficients, 2)
  )


map_dbl(1:5, \(x) x + 5)


extract_continent <- function(x) filter(dat, continent == x) 
run_lifeExp_model <- function(x) lm(data = x, lifeExp ~ year)
extract_slopes <- function(x) coefficients(x)[2]

unique(dat$continent) |> 
  map(extract_continent) |> 
  map(run_lifeExp_model) |> 
  map_dbl(extract_slopes)


tibble(
  x = 1:5, 
  y = 2:6,
  sum = map2_dbl(x, y, \(x, y) x + y)
)


tibble(
  x = c(1, 1, 1),
  y = c(10, 20, 30),
  z = c(100, 200, 300),
  sum = pmap_dbl(list(x, y, z), sum)
)

