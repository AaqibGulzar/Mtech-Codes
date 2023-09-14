library(tidyverse)
library(dataRetrieval)
library(rnaturalearth)
library(sf)

#here we choose the 9th observation in the first station on 1961-02-22.
#I have to check if there are peaks on the same date in other stations too!!!!!
#this function below will give values if the data matches wrt first station.
#if there are no concurrent peaks,it will give <0 rows> (or 0-length row.names).

site_numbs <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970",
                "02192300","02193500","02204500","02208450","02212600","02213050","02217500","02217900",
                "02219000","02219500","02220550","02220900","02221000","02221525","02335700",
                "02337000","02337400","02338660","02340500","02344500","02344700","02345000",
                "02346500","02347500","02394400","02411800","02413000","02413200","02077200",
                "02081500","02081747","02082770","02082950","02085000","0208521324","02085500",
                "02086000","02087500","02093800","02094000","02095000","02096500",
                "02098500","02099500","02100500","02101800","02102000","02114450","02120780",
                "02121500","02123500","02125000","02126000","02127000","02128000","02142900",
                "02143500","02144000","02146900","02147500","02157500","02159000",
                "02160000","02160500","02162010","02165000","02165200","02186000",
                "02192500","02196000","02044000","02044200","02044500","02046000","02051000",
                "02051500","02051600","02052500","02058400","02064000",
                "02065500","02066500","02075500","02076500","02079640")


site_peaks <- readNWISpeak(site_numbs) #find peaks 
site_detail <- readNWISsite(site_numbs) #details

#get the shapefiles.....
sf_usa <-
  ne_states(country = "united states of america", returnclass = "sf")
#now the filtering is done as i only need few states,name_sv is inside the shape file itself.
sf_piedemont <-
  filter(sf_usa,
         name_sv == "South Carolina" |
           name_sv == "North Carolina" | name_sv == "Georgia" | name_sv=="Virginia" |
           name_sv == "Alabama")


#following chunk of code to find the dates for highest
#ever recorded peaks till date for each station
max_peak_date <-   function(site) {
  station_df = filter(site_peaks , site_no == site)
  station_df <- station_df[, c(3, 5)] %>% na.omit()
  for (i in 1:nrow(station_df)) {
    if (station_df[i, "peak_va"] == max(station_df$peak_va)) {
      x = station_df[i, ]
      return(x$peak_dt)
    }
  }
}

max_peak_date(site_numbs[1])
max_peak_date(site_numbs[3])
max_peak_date(site_numbs[44])


library(geosphere)

d_ij=matrix(data = NA,
            nrow = 92,
            ncol = 92)#create an empty matrix and loop the function over it


for(i in 1:92){
  for (j in 1:92){
    d_ij[i,j]= distm(
      c(site_detail$dec_long_va[i], site_detail$dec_lat_va[i]),
      c(site_detail$dec_long_va[j], site_detail$dec_lat_va[j])
    )
  }
}

d_ij = d_ij *  0.000621  #the distances will be in metres,so we convert those into miles.
# 1 metre = 0.000621 miles

#For the first station,highest peak value has been seen on 1977-03-31.
#so here we can apply the function as per that date. similarly we can check other dates too,
#Here we will try on the station number 82.
# max_peak_date(site_numbs[82])
# peak_dt          peak_va
# 2018-10-11       55000

con_stat_peaks <-   function(site,date) {
  station_df = filter(site_peaks , site_no == site)
  station_df <- station_df[, c(3, 5)] %>% na.omit()
  output = station_df %>% filter(peak_dt == date)  # for station 2
  return(if (nrow(output) > 0) {
    print("Yes ")
  } else {
    print("No")
  })
}

#TRY this.
con_stat_peaks(site_numbs[2],max_peak_date(site_numbs[91])) # with site_numb[...]


Results <-
  matrix(data = NA, nrow = 92, ncol = 1)   #create an empty matrix to overwrite
# with return output of above function

for(i in 1:92) {
  Results[i] = con_stat_peaks(site_numbs[i],max_peak_date(site_numbs[11]))
}


#Create a data frame which can be plotted as points as coloured as per "yes" or "no"
dframe = data.frame(site_detail$dec_lat_va, site_detail$dec_long_va, Results)
colnames(dframe) = c("Lat", "Long", "Concurrent Peak ")


# For the time being we will simply see which stations have any same day flooding event.

 ggplot() + geom_sf(data = sf_piedemont) +
  labs(x = "long",
       y = "lat",)  +
  scale_fill_viridis_c() +
  geom_point(data = dframe,
             aes(x = Long, y = Lat, color = `Concurrent Peak `),
             size = 2)

# Publication Ready Plot
 # if we want to distinguish that particular station .
 
  ggplot() + geom_sf(data = sf_piedemont,alpha = 0.2) +
   labs(x = "long",
        y = "lat",)  +
   scale_fill_viridis_c() +
   geom_point(data = dframe,
              aes(x = Long, y = Lat, color = `Concurrent Peak `),
              size = 2) + geom_point(data=dframe[2,],aes(x = Long,y = Lat),
                                     color ="dodgerblue4",shape = 3,size = 3) +
   theme(panel.background=element_rect("white"),
         panel.grid.major=element_line("lightgrey"),
         plot.title = element_text(color = "dodgerblue4", size = 12,
                                   face = "bold")) +
   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
   theme(legend.text = element_text(size = 10), 
         legend.title = element_text(size = 13),
         panel.background=element_rect("white"),
         panel.grid=element_line(colour="grey",linetype="dashed",linewidth=0.1)) +
   ggtitle("USGS Site(+):02413475\nPeak Date:1956-03-16") +
    xlab("Longitude") + ylab("Latitude") +
    theme(axis.title = element_text(size = 9),
          axis.title.y = element_text(size = 9))

 
# ggsave("Correspondance.png",dpi = 800)

 
 
 
 
 
 
 
 
 
 
 
 
 
 
  
 
 