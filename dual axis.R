library(dplyr)
library(plotly)
CV=read.csv("c:/Users/ASUS/OneDrive/Documents/meh maam/CV.csv",header=T,sep=",")
CV$Dates = as.Date.character(CV$Dates)

CV$Precipitation_rev <- rev(CV$Precipitation)

error_max <- max(max(CV$CV_Rainfall.error), max(CV$CV_Runoff.error))
precipitation_max <- max(CV$Precipitation_rev)

# Set the scaling factor for the precipitation data
scale_factor <- error_max / precipitation_max
plot = plot_ly(data = CV, x = ~Dates) %>%
  add_trace(y = ~CV_Rainfall.error, name = "CV Rainfall Error", type = "scatter", mode = "lines+markers") %>%
  add_trace(y = ~CV_Runoff.error, name = "CV Runoff Error", type = "scatter", mode = "lines+markers") %>%
  add_trace(y = ~Precipitation_rev, name = "Precipitation", yaxis = "y2", type = "bar") %>%
  layout(title = "Dual Axis Plot with Precipitation and Errors",
         yaxis = list(title = "Error", range = c(0, error_max)),
         yaxis2 = list(title = "Precipitation", overlaying = "y", side = "right",
                       autorange="reversed", range = c(0, precipitation_max*scale_factor*9)))

plot
