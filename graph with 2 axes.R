set.seed(25852)
x=rnorm(30)                              #create sample data
y1=x+rnorm(30)
y2=x+rnorm(30,5)

par(mar=c(5,4,4,4)+0.3)                  #additional space for 2nd y-axis
plot(x,y1,pch=18,col=2)
par(new= T)                               #add new plot      
plot(x,y2,pch=19,col=3,
     axes=F,xlab = "",ylab = "")          #create 2nd plot without axes
axis(side=4,at=pretty(range(y2)))         #add 2nd axis
mtext("y2",side=4,line=3)                #add 2nd axis label
