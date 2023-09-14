# Rayshader Tutorial,https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
library(rayshader)
library(sf)
library(elevatr)

data <- st_read("c:/Users/ASUS/Desktop/R Programming/Shape file/Grand Canyon/GRCA_boundary.shp") 
g_canyon_elev <- get_elev_raster(data, z = 9, clip = "location")
# Convert our raster data in 
mat <- raster_to_matrix(g_canyon_elev)

# Plot data with `plot_3d()`
mat %>%
  height_shade() %>%
  plot_3d(heightmap = mat)

# Close the window when you're done
rgl::rgl.close()

library(MetBrewer)
# Specify the palette name in its own variable so that
# we can reference it easily later.
pal <- "Demuth"
colors <- met.brewer(pal)

mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat) 


# Dynaimcally set window height and width based on object size
w <- nrow(mat)
h <- ncol(mat)

# Scale the dimensions so we can use them as multipliers
wr <- w / max(c(w,h))
hr <- h / max(c(w,h))

# Limit ratio so that the shorter side is at least .75 of longer side
if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}


# Make sure to close previous windows

rgl::rgl.close()

mat %>%
  height_shade(texture = grDevices::colorRampPalette(colors)(256)) %>%
  plot_3d(heightmap = mat, 
          windowsize = c(800*wr,800*hr), 
          solid = FALSE, 
          zscale = 10,
          phi = 90, 
          zoom = .5, 
          theta = 0) 

render_highquality(
  "c:/Users/ASUS/Documents/gcnp_highres.png", 
  parallel = TRUE, 
  samples = 300,
  light = FALSE, 
  interactive = FALSE,
  environment_light = "c:/Users/Default/Desktop/phalzer_forest_01_4k.hdr",
  intensity_env = 1.5,
  rotate_env = 180,
  width = round(6000 * wr), 
  height = round(6000 * hr)
)
# Load magick library, which provides R interface with ImageMagick
library(magick)

# Read in image, save to `img` object
img <- image_read("plots/gcnp_highres.png")

# Set text color
text_color <- colors[1]

# Title
img_ <- image_annotate(img, "A Portrait of", font = "Cinzel Decorative",
                       color = colors[1], size = 125, gravity = "north",
                       location = "+0+200")
# Subtitle
img_ <- image_annotate(img_, "Grand Canyon National Park", weight = 700, 
                       font = "Cinzel Decorative", location = "+0+400",
                       color = text_color, size = 200, gravity = "north")



# Square miles, converted from square meters
area <- as.numeric(st_area(data)) / 2.59e6

# Elevation range, converted to feet from meters
elev_range <- (max(mat, na.rm = TRUE) - min(mat, na.rm = TRUE)) * 3.281

# Area
img_ <- image_annotate(img_, glue("Area: {label_comma()(round(area))} sq mi"),
                       font = "Cinzel Decorative", location = "+1200-1000",
                       color = text_color, size = 110, gravity = "west")

# Elevation range
img_ <- image_annotate(img_, glue("Elevation Range: {label_comma()(round(elev_range))} ft"),
                       font = "Cinzel Decorative", location = "+1200-1300",
                       color = text_color, size = 110, gravity = "west")



states <- spData::us_states 

spot <- st_buffer(st_centroid(data), 100000)
text_color <- colors[length(colors)]


loc_plot <- ggplot() + 
  geom_sf(data = states, fill = "transparent", color = text_color, size = 0.2) + 
  geom_sf(data = spot, fill = NA, color = colors[2]) +
  theme_void() + 
  coord_sf(crs = 3347)

loc_plot
ggsave(loc_plot, filename = glue("plots/gcnp_inset.png"), w = 4*1.5, h = 3*1.5)


# Caption
img_ <- image_annotate(img_, glue("Graphic by Spencer Schien (     @MrPecners) | ", 
                                  "Data from AWS Terrain Tiles and USGS"), 
                       font = "Cinzel Decorative", location = "+0+50",
                       color = alpha(text_color, .5), size = 75, gravity = "south")

# Twitter
twitter <- fa("twitter", fill = text_color, fill_opacity = .5)
grid.newpage()

tmp <- tempfile()
png(tmp, bg = "transparent")
grid.draw(read_svg(twitter))
dev.off()

tw <- image_read(tmp)
tw <- image_scale(tw, "x75")

img_ <- image_composite(img_, tw, gravity = "south",
                        offset = "-530+65")


image_write(img_mosaic, glue("plots/gcnp_fully_annotated.png"))




































