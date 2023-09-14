library(sf)
library(devtools)
library(stringr)
library(lubridate)
library(reshape)
library(historydata)
library(ggplot2)
library(raster)
library(USAboundariesData)
library(USAboundaries)
library(dplyr)
library(broom)
library(maptools)
library(mapdata)
library(spData)
library(rgeos)
library(ggmap)
library(tmap) # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny) # for web applications


set.seed(123)
data=matrix(rnorm(100,0.10),nrow = 10,ncol=10)
colnames(data) = paste0("col",1:10)
rownames(data)= paste0("row",1:10)
heatmap(data)
my_colors = colorRampPalette(c("cyan","deeppink3"))
heatmap(data,Rowv = NA,Colv = NA,col=my_colors(100))

data_melt = melt(data)
ggp = ggplot(data_melt,aes(X1,X2)) + geom_tile(aes(fill=value))
ggp
ggp + scale_fill_gradient(low = "green",high = "black")
library(plotly)
plot_ly(z=data,colorscale="greys",type = "heatmap")
#.......................................................................


#Making maps with R(Geocomputations with R)
#creating maps from shape files.
south_carolina = st_read(
  "c:/Users/Hp/Desktop/R Programming/Shape file/tl_2016_45_cousub/tl_2016_45_cousub.shp"
)
#library(rgdal)
my_spdf = readOGR(
  dsn = paste0(
    "c:/Users/Hp/Desktop/R Programming/Shape file/tl_2016_45_cousub/tl_2016_45_cousub.shp"
  )
)

plot(
  my_spdf,
  col = "#f2f2f2",
  bg = "skyblue",              #simple blue figure
  lwd = 0.25,
  border = 0
)

#using tmap
tm_shape(my_spdf) + tm_fill()
tm_shape(nz) + tm_fill(col="orange",alpha = .5) + tm_borders(col="red",lwd=..)

#using ggplot,but first tidy the data
spdf = tidy(my_spdf)
ggplot(spdf) + geom_polygon(aes(x = long, y = lat, group = group), fill =
                              "orange") + theme_classic()


#using map_data function and filter()
States <- map_data("state") 
sou_car = filter(States, region == "south carolina")
sou_car_map=ggplot(data = sou_car) + 
            geom_polygon(aes(x = long, 
                             y = lat), 
                             fill = "brown") +
                             theme_bw()


# tmap for new zealand,different styles
ma1 = tm_shape(nz) + tm_fill(col = "red")
ma2 = tm_shape(nz) + tm_fill(col ="red", alpha = 0.3)
ma3 = tm_shape(nz) + tm_borders(col = "blue")
ma4 = tm_shape(nz) + tm_borders(lwd = 3)
ma5 = tm_shape(nz) + tm_borders(lty = 2)
ma6 = tm_shape(nz) + tm_fill(col = "red", alpha = 0.3) +
  tm_borders(col = "blue", lwd = 3, lty = 2)
tmap_arrange(ma1, ma2, ma3, ma4, ma5, ma6)

# base plot fuction mapping in R
plot(st_geometry(nz),col=nz$Land_area)
tm_shape(nz) + 
  tm_fill(col = nz$Land_area) 
# fails,as tmap aesthetic doesnt take numeric arguments...

tm_shape(nz) + tm_fill(col = "Land_area") #this works

#to change the legend etc.
legend_title = expression("Area (km"^2*")")
tm_shape(nz) + 
              tm_fill(col = "Land_area",title = legend_title) +
              tm_borders(lwd = 1,col="yellow")

tm_shape(nz) + tm_polygons(col = "Median_income")

breaks = c(0, 3, 4, 5) * 10000 #we can set the breaks to 1,2,3,4,5.
-------------------------------#or use style ="cat","cont","order".

tm_shape(nz) + tm_polygons(col = "Median_income", breaks = breaks)
tm_shape(nz) + tm_polygons(col = "Median_income", n = 8)
tm_shape(nz) +
  tm_polygons(col = "Median_income", palette="BuGn") 
--------------------------------------------------------------------------------                             
tmaptools::palette_explorer()
#other palettes are viridis,Blues,plasma,YlOrBr,inferno,magma,librarry(RcolorBrewer)
#library(ggsci),library(wesanderson),
--------------------------------------------------------------------------------
#adding labels and other layout items.
  
  new_zealand <- tm_shape(nz) +
  tm_polygons(col = "Median_income", palette = "viridis",style="cont") +
  tm_compass(type = "8star", position = c("right", "top"),size = 1) +
  tm_scale_bar(breaks = c(0, 100, 200), size = 1)

#other layout arguments
 my_spdf$
map_nz <- tm_shape(nz) +
  tm_polygons()
map_nz + tm_layout(title = "New Zealand")
map_nz + tm_layout(scale = 5)
map_nz + tm_layout(bg.color = "lightblue")
map_nz + tm_layout(frame = FALSE)




urb_1970_2030 = urban_agglomerations %>%
  filter(year %in% c(1970, 1990, 2010, 2030))
tm_shape(world) + tm_polygons() +
  tm_shape(urb_1970_2030) + tm_symbols(col = "black", border.col = "white",
                                       size = "population_millions") +
  tm_facets(by = "year", nrow = 2, free.coords = FALSE)

#------------------------------------------------------------------------------
#making maps with R,from github,reproducible research,eric anderson.

usa <- map_data("usa") #creating usa 
w2hr <- map_data("world2Hires") #hihgres map centered on the pacific

# create a simple usa black map
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) + coord_fixed(1.5)
# coord_fixed is important to fix the size and dimensions

gg1 <- ggplot(data = usa) + 
  geom_polygon(aes(x=long,y=lat,group = group),fill="brown",color="lightblue") + 
  theme_light() +coord_fixed(1.5)  #group=group is important,
                                   #otherwise ,u get lines
   
# adding points to the map,here location of two labs.
labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC", "NWFSC"),
  stringsAsFactors = F
)
gg1 + geom_point(data = labs,aes(x=long,y=lat),color="orange",size=5)


#State Maps.
States=map_data("state")
color_state <- ggplot(data = States) +
  geom_polygon(aes(
    x = long,
    y = lat,
    fill = region,
    group = group), color = "white") + coord_fixed(1.5) +
    guides(fill = F) # do this to leave off the colour legend

# plotting just a subset of states by using the subset command.
# it provides another way of subsetting a dataframe with a logical vector.e,g

west_coast <-
  subset(States, region %in% c("california", "oregon", "washington"))

ggplot(data = west_coast) + 
   geom_polygon(aes(x = long, y = lat), fill = "palegreen", color ="black")
#see by not grouping,we get a wierd looking figure.so we fix it

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat,group=group), fill = "palegreen", color ="black") +
  coord_fixed()
  
#Zooming in on California.
cal_df <- subset(States,region == "california")

#now lets get the counties in as well.
counties <- map_data("county")
cal_county <- subset(counties,region == "california")

cal_base <- ggplot(data = cal_df,mapping = aes(x=long,y=lat,group=group)) + 
  coord_fixed(1.5) + geom_polygon(color="black",fill="gray")
cal_base + theme_nothing()

cal_base + geom_polygon(data=cal_county,fill=NA,color="white") + theme_nothing() +
         geom_polygon(color="black",fill=NA) #last layer for black borders
#///////////////////////////////////////////////////////////////////////////////  
#library(readxl)
cal_pop<- read_excel("C:/Users/Hp/Desktop/Book (1).xlsx")
cal_pop <- data_frame(cal_pop)
#///////////////////////////////////////////////////////////////////////////////





library(raster)
sc_raster=raster("c:/Users/Hp/Desktop/820568.tif")
sc_raster_pts <- rasterToPoints(sc_raster, spatial = TRUE)
# Then to a 'conventional' dataframe
DSM_HARV_df  <- data.frame(DSM_HARV_pts)
rm(DSM_HARV_pts)

ggplot() +
  geom_raster(data = DSM_HARV_df , aes(x = x, y = y, fill = HARV_dsmCrop)) + 
  ggtitle("Continuous Elevation Map - NEON Harvard Forest Field Site") + 
  coord_equal()









