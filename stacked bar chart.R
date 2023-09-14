library(readxl)
library(tidyverse)




stacked_bar = function(sheet,station) {
  data_frame <- read_excel("~/umer/LULC Area Change new.xlsx",
    sheet = sheet,
    skip = 1,
    .name_repair = janitor::make_clean_names
  )

  data_frame = data_frame[1:9, 1:5]
  
  
  data_frame2 =
    data_frame %>%
    mutate_at(vars(2:5), as.numeric)
  
  colnames(data_frame2) = c("Class",
                          "Year 1992 to 2000",
                          "Year 2000 to 2010",
                          "Year 2010 to 2021",
                          "Year 1992 to 2021")
  
  

  # # Plotting using ggplot2
  # ggplot(manasbal2, aes(x = Class, y = `Year 1992 to 2000`)) +
  #   geom_bar(stat = "identity", fill = "steelblue") +
  #   labs(x = "Land Type", y = "Percent Change",
  #        title = "Percent Area Change by Land Type from\n1992 to 2000 in Manasbal Lake ") + theme_minimal()
  #
  
  
  # Melt the data into a long format for easier plotting
  melted_df <- reshape2::melt(data_frame2, id.vars = "Class") %>% na.omit()
  
  # Plotting using ggplot2
  ggp = ggplot(melted_df, aes(x = Class, y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    labs(
      x = "Land Type",
      y = "Percent Change",
      fill = "Time Period") +
        scale_fill_manual(
          values = c("#3E4A89FF","#35B779FF","#B4DE2CFF","#FDE725FF"),
          labels = c("1992-2000", "2000-2010", "2010-2021", "1992-2021")
        ) + ggtitle(paste0("Percent Area Change by Land Type"," for ",station)) +
        theme_minimal() + theme_bw() + theme( axis.title = element_text(size = 12),
                                              axis.title.y = element_text(size = 12),
                                              axis.text = element_text(size = 9, face = "bold"),  # Make the axis tick labels bold
                                              axis.ticks.length = unit(0.3, "cm"),
                                            panel.border = element_rect(color = "black",
                                                                        fill = NA, linewidth = 1),
                                            plot.title = element_text(color = "dodgerblue3", size = 15,
                                                                      face = "bold",
                                                                      hjust=0.1,
                                                                      vjust=-8)) +
    guides(fill = guide_legend(override.aes = list(size = 15))) + 
    scale_y_continuous(breaks=seq(-200,800,50)) 
      
  return(ggp)
}

stacked_bar(sheet="Sheet1",station="Manasbal")
#ggsave("Manasbal.jpg",dpi=500,width=30,height=20,units="cm")

stacked_bar(sheet="Sheet2",station="Dal lake")
#ggsave("Dal lake.jpg",dpi=500,width=30,height=20,units="cm")

stacked_bar(sheet="Sheet3",station="Shishnag")
#ggsave("Shishnag.jpg",dpi=500,width=30,height=20,units="cm")

stacked_bar(sheet="Sheet4",station="Gangbal")
#ggsave("Gangbal.jpg",dpi=500,width=30,height=20,units="cm")
















