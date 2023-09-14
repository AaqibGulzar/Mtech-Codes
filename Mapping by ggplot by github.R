#making us map ,remiller1450 @github
#load us map data
Mainstates <- map_data("state")

#read the statepop data
library(tidyverse)
StatePopulation <-
  read.csv(
    "https://raw.githubusercontent.com/ds4stats/r-tutorials/master/intro-maps/data/StatePopulation.csv",
    as.is = TRUE
  )
#plot all the states simply
usa_base <-
  ggplot(data = Mainstates) + geom_polygon(aes(x = long, y = lat, group =
                                                 group),color = "black",fill="lightblue")

#use the dplyr to merge mainstaes and statepop files
Mergedstates <- inner_join(Mainstates,StatePopulation,by="region")

#create a choropleth map of usa
chor_usa <- ggplot(data = Mergedstates) + 
  geom_polygon(aes(x=long,y=lat,group=group,fill=population/1000000),color="white")


#try this to make it more colorful
chor_usa + scale_fill_gradientn(
  colours = rev(rainbow(7)),
  breaks = c(2, 4, 10, 100, 1000, 10000),
  trans = "log10"
)



chor_usa <- chor_usa + scale_fill_continuous(
  name = "Pop(millions)",
  low = "lightgreen",
  high = "darkgreen",
  limits = c(0, 40),
  breaks = c(5, 10, 15, 20, 25, 30, 35),
  na.value = "grey50"
) + labs(title = "State Population in the Mainland USA ")

#modification of legend
chor_usa2 <- chor_usa +
  guides(fill = guide_colorbar(
    barwidth = 0.5,
    barheight = 10,
    label.theme = element_text(
      color = "black",
      size = 10,
      angle = 45
    )
  ))

#creating a dense looking county map
Allcounty <- map_data("county")
ggplot(data = Allcounty) +
  geom_polygon(
    aes(x = long, y = lat, group = group),
    color = "darkblue",
    fill = "lightblue",
    size = 0.1
  ) +              #this layer adds state borders as well
  geom_polygon(
    data = Mainstates,
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = "lightblue",
    alpha = 0.3
  )

#Adding points to your choropleth
#here we will add cities to the map,size will be as per pop.
chor_usa2 + geom_point(data = us.cities, aes(
  x = long,
  y = lat,
  size = pop
))

#as you will see there are some cities outside usa,alaska and hawaai,so remove them.

Maincities <- filter(us.cities, long >= -130)
g <- ggplot() +
  geom_polygon(data = Mergedstates,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = population / 1000000
    ),
    color = "black",
    size = 0.2
  ) +
  scale_fill_continuous(
    name = "State Population",
    low = "lightblue",
    high = "darkblue",
    limits = c(0,40),
    breaks = c(5, 10, 15, 20, 25, 30, 35),
    na.value = "grey50"
  ) +
  labs(title = "Pop(millions) in the Mainland US")

#add cities now
g_city <-
  g + geom_point(
    data = Maincities,
    aes(
      x = long,
      y = lat,
      size = pop / 1000000
    ),
    color = "gold",
    alpha = 0.7
  ) + scale_size(name = "City Population")
  
#zooming in onto some portion
g_city + coord_cartesian(xlim=c(-75,-65),ylim =c(38,46) )


#///////////////////////////////////////////////////////////////////////////////
newstates <-
  filter(
    Mergedstates,
      region == "new york" |
      region == "vermont" |
      region == "new hampshire" |
      region == "rhode island" | 
      region == "connecticut"
  )

ggplot(data = newstates) + geom_polygon(aes(
  x = long,
  y = lat,
  group = group,
  fill = elect_votes
))
#/////////////////////////////////////////////////////////////////////////////                                       

library(elevatr)
library(kableExtra)
library(remotes)
library(rgeoboundaries)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)

library(elevatr)
library(kableExtra)
library(remotes)
library(rgeoboundaries)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)


#plotting switzerland DEM
swiss_bound <- rgeoboundaries::geoboundaries("Switzerland")
elevation_sw <- elevatr::get_elev_raster(locations = swiss_bound, z = 9, clip = "locations")


elevation_sw <- as.data.frame(elevation_sw, xy = TRUE)
colnames(elevation_sw)[3] <- "Elevation"
# remove rows of data frame with one or more NA's,using complete.cases
elevation_sw <- elevation_sw[complete.cases(elevation_sw), ]

ggplot() +
  geom_raster(data = elevation_sw, aes(x = x, y = y, fill = Elevation)) +
  geom_sf(data = swiss_bound, color = "white", fill = NA) +
  coord_sf() +
  scale_fill_viridis_c() +
  labs(title = "Elevation in Switzerland", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")

#We can also get the different administrative areas of the country Switzerland,
# by using the ne_states function from the rnaturalearth package. 
# And the get_elev_raster function, you can get the elevation data. The following code does the same:
# NOTE: increasing or decreasing the z argument will make the map zoom in and out
# on the country by setting the appropriate z-axis value.

library(rnaturalearth)
library(rnaturalearthhires)

sf_swiss <- ne_states(country = "switzerland", returnclass = "sf")

elevation_swa <- elevatr::get_elev_raster(locations = sf_swiss, z = 7, clip = "locations")
cropped_elev <- crop(elevation_swa, sf_swiss)
elevate <- as.data.frame(cropped_elev, xy = TRUE)

colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ]

ggplot() +
  # geom_sf(data = st_as_sfc(st_bbox(elevation_1)),color = "grey", fill = "grey",alpha = 0.05) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value)) +
  geom_sf(data = sf_swiss, color = "white", fill = NA) +
  coord_sf(xlim = c(5.3, 10.8), ylim = c(45.5, 47.8)) +
  scale_fill_viridis_c() +
  labs(title = "Elevation in Switzerland", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")



















