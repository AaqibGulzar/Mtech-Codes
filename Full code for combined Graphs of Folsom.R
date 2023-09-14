setwd("c:/Users/Hp/Desktop/R Programming/excel files")

folsomlake=read.csv("c:/Users/Hp/Desktop/R Programming/excel files/folsomlake.csv")

Aaqib=lm(outflow~inflow+evap,data=folsomlake)

Yhat=array(data=NA,dim = nrow(folsomlake))

coefficients(Aaqib)

est_coef=Aaqib$coefficients

for(t in 1:nrow(folsomlake)) {
  Yhat[t] = est_coef[1] + est_coef[2] * folsomlake$inflow[t] + est_coef[3] *
    folsomlake$evap[t]
}

x=folsomlake[1:366, ]

y=Yhat[1:366]

plot(x$outflow,main="Outflow for year 2000",xlab = "Days",ylab="Outflow",col=5,pch=20,type = "h")

points(y,col=2,pch=20,type="o")

legend(x="topright",cex=0.5,legend=c("Obs","Est"),fill = c(5,2))

grid(nx=NULL,ny=NULL,col="gray")