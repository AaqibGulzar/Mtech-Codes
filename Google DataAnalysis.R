# Data Analysis With R --- GOOGLE

library(palmerpenguins)
library(tidyverse)
library(lubridate)

#Working with dates
ymd("20210220")  # converting the strings into Y-M-D format
mdy("jan 20 2021") # this is actually very cool.
mdy("mar twenty 2021")
mdy("feb 15 1997")
mdy_hms("feb 15 1997 06:30:30") # adding time too.
now() %>% as_date() # will convert date-time to date
file.create("aaqib.csv") # to create an empty csv file,inyour current wd.
getwd()

#using Pipe Operator
new_df <- ToothGrowth %>%  filter(dose == 0.5) %>% 
  group_by(supp) %>% # here we first filter it,then group it and
  arrange(len)  %>%     # then arrange in the decreasing order 
  summarise(mean_len= mean(len,na.rm=T),.group="drop") #then summarise.

ggplot(data = penguins) + geom_point(aes(
  x = flipper_length_mm,
  y = body_mass_g,
  color = species,
  shape = species
)) + facet_wrap(~species)

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )





