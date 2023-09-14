# Milos Popovic,Crisp Topo Map of ITALY.
#run the insides outside first


windowsFonts(georg = windowsFont('Georgia'))
# 1. GET COUNTRY MAP
#---------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_sf <- function(country_sf, country_transformed) {
  
  country_sf <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10",
    country = "italy")
  
  country_transformed <- st_transform(country_sf, crs = crsLONGLAT)
  
  return(country_transformed)
}

country_transformed <- get_sf()

# 2. GET ELEVATION DATA
#---------
# First, locations is the type of the object that we will use to crop the global
#elevation data and this could be a data.frame, shapefile, or raster object.
#In our case, we will pass the shapefile of Italy. 
#If you would like to capture the topography of the surrounding countries, 
#then you should define a bounding box in the form of a data.frame.
# In the second argument, z = 9 we set the ground resolution of any given
#pixel cell. We chose a bit lower zoom to limit the amount of resources used.
#But you can choose other values ranging from 1 (high-level detail)
#to 14 (low-level detail). We declare that the elevation data should
#be clipped by locations that we have previously defined.

get_elevation_data <- function(country_elevation, country_elevation_df) {
  
  country_elevation <- get_elev_raster(
    locations = country_transformed, 
    z = 9, 
    clip = "locations") 
  
  country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
    na.omit()
  
  colnames(country_elevation_df)[3] <- "elevation"
  
  return(country_elevation_df)
}

get_elevation_map <- function(country_map) {
  
  country_map <- ggplot() +
    geom_tile(data = country_elevation_df, 
              aes(x = x, y = y, fill = )) +
    scale_fill_etopo() +
    coord_sf(crs = crsLONGLAT)+
    theme_minimal() +
    theme(text = element_text(family = "georg", color = "#22211d"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_line(color = "white", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
          plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
          plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank()) +
    labs(x = "", 
         y = NULL, 
         title = "Topographic map of ITALY", 
         subtitle = "", 
         caption = "©2022 Milos Popovic (https://milospopovic.net)")
  
  return(country_map)
}

country_map <- get_elevation_map()

ggsave(
  filename = "italy_topo_map.png",
  width = 7,
  height = 8.5,
  dpi = 600,
  device = 'png',
  country_map
)













