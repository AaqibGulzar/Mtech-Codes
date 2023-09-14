library(raster)
library(ggplot2)
library(sf)
library(rgdal)
library(RColorBrewer)
library(reshape2)


umer = function(lake) {
  path1 = paste0("c:/Users/ASUS/OneDrive/Documents/umer/Mean_LSWT2/", lake)
  setwd(path1)
  raster_file <- list.files(pattern = ".tif$")
  stack_r <- stack(raster_file)
  df <- as.data.frame(stack_r, xy = TRUE) %>% 
        melt(id.vars=c('x','y'))
  
  df$variable <- gsub("^X", "", df$variable)
  min = min(df$value %>% na.omit()) %>% floor()
  max = max(df$value %>% na.omit()) %>% floor()
  
  gg = ggplot() + geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    facet_wrap( ~ variable) +
    geom_line() +
    theme_void() +
    scale_fill_gradientn(colors = rainbow(10),
                           #rev(grDevices::heat.colors(ceiling((max-min)/8))),
                         na.value=NA, name = 'LSWT(\u00B0C)',
                         breaks = seq(min,max, ceiling((max-min)/8 ))) +
    #scale_fill_viridis_c(option="A",name = 'LSWT(\u00B0C)', na.value = NA, 
     #                    begin = 0,end=1,direction=-1) + ggtitle(lake) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.2, "cm"),
      plot.title = element_text(
        color = "dodgerblue3",
        size = 15,
        face = "bold",
        hjust = 0.5,
        vjust = 1
      )
    )
  return(gg)
}

umer("Dal")
umer("Gangbal")
umer("Manasbal")
umer("Shishnag")

#ggsave("facets.jpg",dpi=1000,path="d:/R4DS",height=20,width=20,units="cm")

hist_umer = function(lake) {
  path1 = paste0("c:/Users/ASUS/OneDrive/Documents/umer/Mean_LSWT2/", lake)
  
  path2 = paste0(path1, "/Mean_LSWT_", lake, ".xlsx")
  
  df =  readxl::read_excel(path2)
  df$Raster = 1992:2021
  gg = ggplot(data = df) +
    geom_bar(
      aes(x = Raster, y = Mean),
      stat = "identity",
      fill = "skyblue",
      color = "darkred",
      alpha = 0.3
    ) + ggtitle(lake) +
    scale_x_continuous(breaks = seq(1992, 2021, 3)) + xlab("Year") + ylab("Mean Temperature(\u00B0C)") +
    theme_minimal() + geom_smooth(
      aes(x = Raster, y = Mean),
      method = "lm",
      se = F,
      color = "black"
    ) + theme(plot.title = element_text(
      color = "dodgerblue3",
      size = 15,
      face = "bold",
      hjust = 0.5,
      vjust = 1))
  return(gg)
}

hist_umer("Dal")
hist_umer("Gangbal")
hist_umer("Shishnag")
hist_umer("Manasbal")

#ggsave("hist.jpg",dpi=1000,path="d:/R4DS",height=15,width=25,units="cm")


