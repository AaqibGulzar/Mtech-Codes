
library(raster)
library(ggplot2)
library(sf)
library(rgdal)
library(RColorBrewer)
library(reshape2)
library(dplyr)

my_breaks <- seq(8, 32, 4)
my_labels <- c("29-32","25-28", "21-24", "17-20", "13-16","8-12")


Dal_bin = function() {
  path1 = paste0("c:/Users/ASUS/OneDrive/Documents/umer/Mean_LSWT2/Dal/")
  setwd(path1)
  raster_file <- list.files(pattern = ".tif$")
  stack_r <- stack(raster_file)
  df <- as.data.frame(stack_r, xy = TRUE) %>% 
    melt(id.vars=c('x','y'))
  
  df$variable <- gsub("^X", "", df$variable)
  min = min(df$value %>% na.omit()) %>% floor()
  max = max(df$value %>% na.omit()) %>% ceiling()
  df$bin   <- cut(df$value, breaks = my_breaks, labels = rev(my_labels))
  
  gg = ggplot() +
    geom_raster(data = na.omit(df),aes(x = x,y = y,fill = bin)) +
    facet_wrap(~ variable) +
    geom_line() +
    theme_void() +
    scale_fill_manual(values=turbo(6),na.value=NA,name = 'LSWT(\u00B0C)') +
    theme(
      legend.position = "right",
      legend.key.width = unit(0.5, "cm"),
      legend.key.height = unit(2.5, "cm"),
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

Dal_bin()

ggsave("bin.jpg",dpi=1000,path="d:/R4DS/",height=30,width=30,units="cm")

