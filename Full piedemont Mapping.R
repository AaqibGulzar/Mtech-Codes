#Creating the elevation map for piedemont region along with 
#locations of 92 gauging stations.
Reis_et_al <- read.csv("d:/R Programming/excel files/Reis_Et_Al_2020_ExplanatoryVariables.csv")
view(Reis_et_al)


library(ggmap)
library(tidyverse)
library(dplyr)
library(dataRetrieval)
site_numbs <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970","02192300"
               ,"02193500","02204500","02208450","02212600","02213050","02217500","02217900"
               ,"02219000","02219500","02220550","02220900","02221000","02221525","02335700"
               ,"02337000","02337400","02338660","02340500","02344500","02344700","02345000"
               ,"02346500","02347500","02394400","02411800","02413000","02413200","02077200",
               "02081500","02081747","02082770","02082950","02085000","0208521324","02085500"
               ,"02086000","02087500","02093800","02094000","02095000","02096500"
               ,"02098500","02099500","02100500","02101800","02102000","02114450","02120780"
               ,"02121500","02123500","02125000","02126000","02127000","02128000","02142900"
               ,"02143500","02144000","02146900","02147500","02157500","02159000"
               ,"02160000","02160500","02162010","02165000","02165200","02186000"
               ,"02192500","02196000","02044000","02044200","02044500","02046000","02051000"
               ,"02051500","02051600","02052500","02058400","02064000"
               ,"02065500","02066500","02075500","02076500","02079640")

#class(df_lat_lon)
site_detail <- readNWISsite(site_numbs)
site_detail$dec_lat_va  #gives decimal lat values 
site_detail$dec_long_va #gives decimal long values
library(rnaturalearth)
sf_usa <- ne_states(country = "united states of america", returnclass = "sf")
#now the filtering is done as i only need few states,name_sv is inside the shape file itself.
sf_piedemont <-
  filter(sf_usa,
         name_sv == "South Carolina" |
           name_sv == "North Carolina" | name_sv == "Georgia" | name_sv=="Virginia" |
           name_sv == "Alabama")

#following line of code requires fast internet!,i have downloaded for whole usa
#but we can use location=sf_usa2 only.
library(sf)
library(elevatr)
library(rgdal)
elevation_piedemont <- elevatr::get_elev_raster(locations = sf_piedemont, z = 7, clip = "locations")
library(raster)

# we can crop the elevation data as per the boundaries of the shape files,
#e,g crop(elevation_USA,sf_State) will cut the USA elevation data as per the state mentioned

elevate <-  as.data.frame(elevation_piedemont, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ]       

#import the watershed shapefile created using QGIS.
pied_water = readOGR("d:/QGIS/Piedemont/Piedemont.shp")

#finally we plot!!!!!!!!!!

ggplot()+geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value))  +
  labs(title = "elevation",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") +
  scale_fill_distiller(
    palette = "Spectral",
    limits = c(0, 1800),
    breaks = c(300, 600, 900, 1200, 1500)
  ) +
  geom_sf(data = sf_piedemont, color = "white",alpha = 0.1) +
  theme_classic() +
  geom_point(
    data = site_detail,
    aes(x = dec_long_va,
        y = dec_lat_va),
    size = 1,
    #regression data is reis variables.
    color = "black",
    alpha = 0.4
  ) + 
  theme(legend.key.size=unit(1,"cm")) + 
  geom_polygon(data=pied_water,aes(x = long,y = lat),color = "grey",alpha = 0.3)


#scale_fill_gradientn(colors=rainbow(n))
#scale_fill_viridis_c(option="magma")
#trying various color palletes
gg <- ggplot() + geom_sf(data = sf_piedemont) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value))  +
  labs(title = "Piedemont,USA with 92 Stream-gauging Stations",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") +
  geom_point(shape=2,
             data = site_detail,
             aes(x = dec_long_va,
                 y = dec_lat_va),
             color = "red")  + scale_size(name = "Area(sq.km)")

#library(gridExtra),to grid multiple plots.
grid.arrange(
  
  gg + scale_fill_viridis_c(option = "A",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  gg + scale_fill_viridis_c(option = "B",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800))  ,
  
  
  gg + scale_fill_viridis_c(option = "C",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  gg + scale_fill_viridis_c(option = "D",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  gg + scale_fill_viridis_c(option = "E",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  gg + scale_fill_viridis_c(option = "F",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  gg + scale_fill_viridis_c(option = "G",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,
  
  
  gg + scale_fill_viridis_c(option = "H",
                            limits=c(0,1800),
                            breaks = c(300, 600, 900, 1200, 1500, 1800)) ,ncol=4,nrow=2 )

gg + scale_fill_etopo() + theme(legend.backgroun)
gg + scale_fill_distiller(
  palette = "Spectral",
  limits = c(0, 1800), 
  breaks = c(300, 600, 900, 1200, 1500)) + theme_classic()


sf_piedemont 

elevation_sfp <- elevatr::get_elev_raster(locations = sf_piedemont, z = 5, clip = "locations")
cropped_elev <- crop(elevation_sfp, sf_piedemont)
library(raster)
elevate <- as.data.frame(cropped_elev, xy = TRUE)

colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ]
library(marmap)
# ggcolr <- scale_fill_distiller(
#   palette = "Spectral",
#   limits = c(0, 1800),
#   breaks = c(300, 600, 900, 1200, 1500)
# )
library(MetBrewer)

gg_pied_base <- ggplot() +
  # geom_sf(data = st_as_sfc(st_bbox(elevation_1)),color = "grey", fill = "grey",alpha = 0.05) +
  geom_tile(data = elevate, aes(x = x, y = y, fill = elevation_value)) +
  geom_sf(data = sf_piedemont, color = "white", fill = NA) +
  labs(title = "Elevation in Piedemont", x = "Longitude", y = "Latitude", fill = "Elevation (meters)")




ggcolr <- scale_fill_gradientn(colours = met.brewer("Tara")) 

gg1 = gg_pied_base +
  geom_point(
    data = site_detail,
    aes(x = dec_long_va,
        y = dec_lat_va),color = "red"
  )  + scale_size(name = "Area(sq.km)") + ggcolr + theme_classic()

gg2 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$slope),
  color = "red"
)  + scale_size(name = "Slope")


gg3 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Averageslope),
  color = "red"
)  + scale_size(name = "Avg.Slope")


gg4 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$length),
  color = "red"
)  + scale_size(name = "Length(km)")


gg5 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$perimeter),
  color = "red"
)  + scale_size(name = "Perimeter(sq.km)")


gg6 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$shapefactor),
  color = "red"
)  + scale_size(name = "Shapefactor")

gg7 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Area),
  color = "red"
)  + scale_size(name = "Area(sq.km)") 

gg8 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Mean.elevation),
  color = "red"
)  + scale_size(name = "MeanElevation")

gg9 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Maximum.elevation),
  color = "red"
)  + scale_size(name = "MaxElevation")


gg10 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Minimumelevation),
  color = "red"
)  + scale_size(name = "MinElevation")

gg11 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Mean.precipitation),
  color = "red"
)  + scale_size(name = "Mean Precip")


gg12 <- gg_pied_base + ggcolr +  geom_point(
  data = site_detail,
  aes(x = dec_long_va,
      y = dec_lat_va,
      size = regression_data$Percent.forests),
  color = "red"
)  + scale_size(name = "Percent Forests")

ggsave(filename = "gg_whatever",width=13,height = 14,dpi = 600,device = "pdf")
#getwd() to check the location of ggsave



