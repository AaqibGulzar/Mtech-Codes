library(rivnet)
fp <- system.file("extdata/wigger.tif", package="rivnet")
raster(fp) # check the contents of the raster file.
r <- extract_river(outlet=c(637478,237413),
                   DEM=fp)
r <- aggregate_river(r)

plot(r)


#--------------------------------------------------

r <- extract_river(outlet = c(-121.0,38.9),
                   DEM=elevation_ARW,EPSG=NULL)
