library(tidyverse)

humaira <-
  read.table(
    "c:/Users/Hp/Desktop/R Programming/ecornell/assignment.csv",
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

ggplot(data = june, aes(x = HH)) + #form the base x-axis of the plot
  geom_line(aes(y = RH, color = "RH")) +  # add the first y-axis
  geom_line(aes(y = press, color = "press")) + # add the second y-axis
  scale_y_continuous(sec.axis = sec_axis( ~ ., name = "PRESS")) + # transform the second y-axis
  labs(x ="Days", title = "Variation of RH and Press for the month of June",color=" ") + #add labels
  scale_x_continuous(breaks = seq(0, 30, 1))  + # add breaks for the month as per days
  theme_minimal() #set a theme +
  theme(legend.key.size = unit(2,"cm"),       # customise the legend
        legend.text = element_text(size = 20),
        legend.title = element_text(colour = "green"),
        legend.position = "bottom")


  
  ggplot(data = june, aes(x = HH)) + #form the base x-axis of the plot
    geom_line(aes(y = RH, color = "RH")) +  # add the first y-axis
    geom_line(aes(y = press, color = "press")) + # add the second y-axis
    scale_y_continuous(sec.axis = sec_axis( ~ ., name = "PRESS")) + # transform the second y-axis
    labs(x ="Days", title = "Variation of RH and Press for the month of June",color=" ") + #add labels
    scale_x_continuous(breaks = seq(0, 30, 1))  + # add breaks for the month as per days
    theme_minimal()  + 
    theme(legend.key.size = unit(2,"cm"),
          legend.text = element_text(size = 20),legend.position = "bottom")
  
  
  
  
  