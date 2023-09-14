library(switchboard)
for (i in 1:1e6) {
  x=cos(i/400)
  y=sin(2*i/400)
  switchboard(skip=6)%>%
    eavesdropper_2D(c(x,y),
    minimum=c(-1,-1),maximum=c(1,1),forget=300,switch=T,size=3)
}
switchboard_close()



for (i in 1:10000) {
  x = cos(i) 
  y = sin(i) 
  switchboard(skip = 0)  %>%
  eavesdropper_2D(c(x, y),switch=T,size=3)
}
switchboard_close()