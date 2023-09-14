library(dplyr)
library(plotly)
CV=read.csv("C:/Users/ASUS/OneDrive/Documents/meh maam/CV_new.csv",header=T,sep=",")
CV$Dates = as.Date.character(CV$Dates)

CV$Precipitation_rev <- rev(CV$Precipitation)

error_max <- max(max(CV$CV_Rainfall.error), max(CV$CV_Runoff.error),
                 max(CV$Total.error))


precipitation_max <- max(CV$Precipitation_rev)
scale_factor <- error_max / precipitation_max

my_colors<-c(
  'blue',
  'red',
  'green',
  'black'  
) 
# Set the scaling factor for the precipitation data
plot_ly(data = CV, x = ~Dates) %>%
  add_trace(y = ~CV_Rainfall.error, name = "CV Rainfall Error", type = "scatter", mode = "lines",line = list(color = my_colors[1])) %>%
  add_trace(y = ~CV_Runoff.error, name = "CV Runoff Error", type = "scatter", mode = "lines",line = list(color = my_colors[2])) %>%
  add_trace(y = ~Total.error, name = "CV Total Error", type = "scatter", mode = "lines",line = list(color = my_colors[3])) %>%
  add_trace(y = ~Precipitation_rev, name = "Precipitation", yaxis = "y2", type = "bar",marker = list(color = my_colors[4])) %>%
  layout(title = "Dual Axis Plot with Precipitation and CV",
         yaxis = list(title = "CV", range = c(0, error_max)),
         yaxis2 = list(title = "Precipitation", overlaying = "y", side = "right",
                       autorange=F, range = c(250, 0)))

