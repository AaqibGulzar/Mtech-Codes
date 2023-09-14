library(zoo)
library(hydromad)
library(readxl)
library(xlsx)
library(hydroGOF)
library(dplyr)
###Read input file
GR4Jparamsinp = read.csv("c:/Users/ASUS/OneDrive/Documents/paramsgr4jinp1.csv",
                         header = T)

true_params=c(x1 = 800, x2 = 1.18, x3 = 90, x4 = 2.8)
x1=GR4Jparamsinp$X1
x2=GR4Jparamsinp$X2
color=GR4Jparamsinp$NSE_Calib


library(ggplot2)
###plot x1 versus x2,with color as 3rd variable
gg1 =
  ggplot(data = GR4Jparamsinp, aes(x = X1, y = X2, color = NSE_Calib)) +
  geom_point() +
  scale_color_viridis_c(
    breaks = c(0, 0.5, 0.75, 0.9, 1),
    labels = c("0", "0.5", "0.75+", "0.9", "1")
  ) +
  geom_point(
    aes(x = 800,
        y = 1.18),
    color = "dodgerblue1" ,
    size = 3,
    shape = 3
  ) +
  geom_point(
    aes(x = 1326.7,
        y = 1.2),
    color = "magenta" ,
    size = 5,
    shape = 3
  ) + theme_classic()
library(plotly)
ggplotly(gg1)    

### plot seems to be ok but it doesnt take NSE_Calib as 3rd variable 
##i.e colour variable,all black dots should be colored with gradient color
##color=GR4Jparamsinp$NSE_Calib should be added to this graph and mapped with x1 and x2

###since most NSE values are above 0.75
### you can add color gradient as
###NSE 0.75-0.80 blue color
###NSE 0.80-0.85 orange
###NSE 0.85-0.90 red
## NSE 0.90-9.05 darkred