lon = site_detail$dec_long_va
lat = site_detail$dec_lat_va
elevation_piedemont <- elevatr::get_elev_raster(locations = sf_piedemont, z = 7, clip = "locations")

elevate <-  as.data.frame(elevation_piedemont, xy = TRUE)
colnames(elevate)[3] <- "elevation_value"
elevate <- elevate[complete.cases(elevate), ]       

ggplot() + 
  geom_sf(data = sf_piedemont, color = "dodgerblue4",alpha = 0.1) +
  geom_raster(data = elevate, aes(x = x, y = y, fill = elevation_value))  + 
  scale_fill_distiller(palette="Spectral",
                       limits = c(0, 1800),
                       breaks = c(300, 600, 900, 1200, 1500)) + 
  labs(title = "Elevation Map of the Study Area",
       x = "long",
       y = "lat",
       fill = "Elevation(m)") + theme_minimal() + 
  geom_point(aes(x =lon,y=lat ),size = 2.5,color = "black",alpha = 0.5) +
  guides(fill = guide_colorbar(barheight=15)) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) + theme_bw()

ggsave("study area.png",dpi=600,width=15,height=12,units="cm")



Rec_per <- function(site) {
  site = filter(data , site_no == site)
  years = site[, 3] %>% na.omit() %>% format(format = "%Y")
  Rec_length = length(years)
  return(Rec_length)
}

rec_lengths <- numeric()

for(i in 1:92){
  rec_lengths[i] = Rec_per(site_numbs[i])
}

print(rec_lengths)
RLT = data.frame(rec_lengths)

#histogram for the record lengths in this region.
ggplot(data = RLT) + 
  geom_histogram(aes(x = rec_lengths), color ="dodgerblue4", fill = "dodgerblue3",binwidth = 1) +
  theme_minimal() + scale_x_continuous(breaks = seq(25,125,10)) + labs(x = "Record Length",y =" Count")

ggplot(data = RLT) + 
  geom_histogram(aes(x = rec_lengths), color = "dodgerblue4", fill = "dodgerblue3", binwidth = 1)  +
  scale_x_continuous(breaks = seq(25,125,10)) +
  labs(x = "Record Length", y = "Count") + theme_minimal() +
  theme(axis.title = element_text(size = 8), axis.text = element_text(size = 8))  +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) + theme_bw()



ggsave("histogram.png",dpi=600)
