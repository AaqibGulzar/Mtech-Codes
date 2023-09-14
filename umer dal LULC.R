library(sf)
library(tidyverse)
library(raster)
library(RColorBrewer)

img <- raster("d:/R Programming/Shape file/LULC2010/DAL_LULC_2010.tif")

img = setMinMax(img)

df = as.data.frame(attributes(img@data)[[7]] )
# Define the colors for each class
colormap <- brewer.pal(10, "Set3") # Change 10 to the number of classes in your raster
names(colormap) <- df1$Class_Name # Get the unique values in the raster and use them as names for the colormap



df1 = as.data.frame(img,xy = T) %>% na.omit()
# Create a ggplot object and set the style
  ggplot(data=df1,aes(x =x, y=y, fill=Area_Class_Name)) +
  geom_raster() +
  scale_fill_manual(values=colormap, name="Land Use and Land Cover Map") +
  ggtitle("Land Use and Land Cover Map")+ theme_void()


  
df1 %>%
    group_by(Area_Class_Name) %>%
    summarize(count = n()) %>%
    mutate(pct = count/sum(count) * 100) %>% 
    ggplot(aes(x = Area_Class_Name, y = pct, fill = Area_Class_Name)) +
    geom_text(aes(label = paste0(round(pct,1),"%")),
              position=position_stack(vjust=1.05)) +
    geom_bar(stat = "identity") +
    labs(title = "Percentage of land Use", x = "Cut", y = "Percentage") + theme_void()


  


  
  






