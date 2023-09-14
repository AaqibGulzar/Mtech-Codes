library(httr)
library(tidyverse)
library(sf)
# 1. GET RIVERS DATA
#---------

get_data <- function() {
  
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_gr_shp.zip"
  
  res <- GET(url,
             write_disk("Green_rivers.zip",overwrite=T),
             progress())
  unzip("Green_rivers.zip")
  filenames <- list.files("HydroRIVERS_v10_gr_shp", pattern="*.shp", full.names=T)
  
  riv_list <- lapply(filenames, st_read)
  
  return(riv_list)
}

filenames = get_data()


load_rivers <- function() {
  list_riv <- lapply(filenames, sf::st_read)
  Green_riv <- list_riv[[1]] |>
    sf::st_cast("LINESTRING")
  
  return(Green_riv)
}


Green_riv <- load_rivers() # this will take a lot of time!!!!!!!!!!!!!!!!!!!!!!!!!!

get_river_width <- function() {
  Green_riv_width <- filenames |>
    dplyr::mutate(
      width = as.numeric(ORD_FLOW),
      width = dplyr::case_when(
        width == 3 ~ 1,
        width == 4 ~ 0.8,
        width == 5 ~ 0.6,
        width == 6 ~ 0.4,
        width == 7 ~ 0.2,
        width == 8 ~ 0.2,
        width == 9 ~ 0.1,
        width == 10 ~ 0.1,
        TRUE ~ 0
      )
    ) |>
    sf::st_as_sf()
  
  return(Green_riv_width)
}

Green_riv_width <- get_river_width()

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                                                                
# get_bounding_box <- function(bbox, new_prj, bb) {                             |
#   bbox <- st_sfc(                                                             |
#     st_polygon(list(cbind(                                                    | 
#       c(-10.5, 48.5, 48.5, -10.5, -10.5),                                     |
#       c(35.000, 35.000, 69.5, 69.5, 35.000)                                   |
#     ))),                                                                      |  
#     crs = crsLONGLAT                                                          |
#   )                                                                           |
#                                                                               |
#   new_prj <- sf::st_transform(bbox, crs = 4087)                               |
#   bb <- sf::st_bbox(new_prj)                                                  |
#                                                                               |
#   return(bb)                                                                  |
# }                                                                             
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bounding box ::
xmin = -72.66458 
ymin = 59.80208
xmax = -12.30208 
ymax = 83.58958

get_river_map <- function() {
  p <-
    ggplot() +
    geom_sf(
      data = eu_riv_width,
      aes(
        color = factor(ORD_FLOW), size = width,
        alpha = factor(ORD_FLOW)
      )
    ) +
    coord_sf(
      crs = 4087,
      xlim = c(xmin,xmax),
      ylim = c(ymin,ymax])
    ) +
    labs(
      y = "", subtitle = "",
      x = "",
      title = "",
      caption = ""
    ) +
    scale_color_manual(
      name = "",
      values = c(
        "#08306b", "#08519c", "#2171b5",
        "#4292c6", "#6baed6", "#9ecae1",
        "#c6dbef", "#deebf7"
      )
    ) +
    scale_size(range = c(0, .3)) +
    scale_alpha_manual(values = c(
      "3" = 1, "4" = 1, "5" = .7, "6" = .6,
      "7" = .4, "8" = .3, "9" = .2, "10" = .1
    )) +
    theme_minimal() +
    theme(
      panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 40, color = "#2171b5", hjust = 0.5, vjust = 0
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 10, color = "grey60", hjust = 0.5, vjust = 10
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  return(p)
}

p1 <- get_river_map()

ggsave(
  filename = "Greenland_rivers.png",
  width = 8.5, height = 7, dpi = 600,
  device = "png", bg = "white", p1
)
