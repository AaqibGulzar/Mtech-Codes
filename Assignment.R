
library(tidyverse)
library(ggplot2)
humaira <-
  read.table(
    "c:/Users/ASUS/Desktop/R Programming/assignment.csv",
    sep = ",",
    header = T
  )

inputs <- as.data.frame(humaira)   #convert into a data-frame

str(inputs)   #check the structure of the data-frame

dim(inputs)    #check the dimensions of the data-frame

june <- filter(inputs,DoY == 6)  #using filter
#OR
june <- inputs[inputs$DoY==6,]  #using sub-setting

august <- filter(inputs,DoY == 8)     #using filter
#OR
august <- inputs[inputs$DoY==8,]     #using sub-setting

august[6:15,] #all values for the month of august from 6 to 15
june[6,] # june 6 values
august[6,] # august 6 values

june$Day_Month = paste0(june$HH,"-",june$DoY) #combining the day andminth column,
june$Day_Month=factor(june$Day_Month,
                      levels = unique(june$Day_Month)) # convert into factors for plotting
                                                      # in sequence
colnames(june)[7] = "PRESS"

ggplot(data = june, aes(x = Day_Month)) + #form the base x-axis of the plot
  geom_point(aes(y = RH, colour = "RH"),size=4) +  # add the first y-axis
  geom_point(aes(y = PRESS, colour = "PRESS"),size=4) +
  scale_y_continuous(sec.axis = sec_axis(~.,name="PRESS")) +
  labs(colour="",x ="Day_Month", title = "Variation of RH and Press for the month of June") + #add labels
  scale_color_manual(breaks = c("PRESS","RH"),values = c("gold","violet")) +
  theme_minimal() + #set a theme 
  theme( axis.text.x = element_text(angle = 45),     #text angle and legend custom
        legend.key.size = unit(1,"cm"), legend.key.height = unit(2,"cm") ,legend.key = element_rect(fill = "grey")) 
  

