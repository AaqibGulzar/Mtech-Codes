install.packages("tidyverse")
answers <- c (TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
answers
A <- matrix(1:10, nrow=5, byrow = TRUE)
AR <- array(c(1:12), dim = c(6,2,1))
AR
List_A <- list(1:20, "Classroom", matrix (c(1:4), nrow=2), list(TRUE, FALSE, FALSE) )
List_A[1]
list_A[2]
lis
weather_report <- data.frame (precip = c(10.2, 20.1, 25.2, 19.3), grid = c("A", "B", "C", "D"), working = c(TRUE, TRUE, TRUE, FALSE))
score
if (score > 90) {
  "A"
} else if (score > 80) {
  "B"
}
  f1 <- function(x, y) {
    s = x + y
    m = x*y
    return(c (s, m))
  }
  
 library(hydromad) 
   
p2007=read.table("C:/Users/ASUS/Downloads/areal_average_precip_1950-2007.csv",header=F) 
q2007 = qdat[qdat$year >= 1950 & qdat$year <= 2007,]
tmax2007<- read.table("C:/Users/ASUS/Downloads/areal_average_tmax_1950-2007.csv",header=F)
tmin2007=read.table("C:/Users/ASUS/Downloads/areal_average_tmin_1950-2007.csv",header=F)
  
  pqdat<-cbind(q2007, p2007,tmax2007, tmin2007)
  colnames(pqdat)<-c("Year","Month","Day","Flow","P", "Tmax.daily","Tmin.daily")
  row.names(pqdat)<-NULL
  pqdat$Date<-with(pqdat,as.Date(ISOdate(Year, Month,Day)))
  pqdat$P[pqdat$P<0]<-NA
  pqdat$P[pqdat$Flow<0]<-NA
  pqdat$Q<-convertFlow(pqdat$Flow, from="m^3/day",area.km2=4740)
  folsomPQ<-zoo(cbind(P=pqdat$P,Q=pqdat$Q,E=pqdat$Tmax.daily),pqdat$Date,frequency=1)
  folsomPQCalib<-window(folsomPQ, start="1990-01-01",end="1999-12-31")
  folsomPQValid<-window(folsomPQ, start="2000-01-01",end="2007-12-31")
  summary(folsomPQ)
cor(folsomPQ)
xyplot(folsomPQ[1:366],type='l',col=c('green','red','blue'))
cmd_e<-0.166
cmd_d<-200
cmd_shape<-c(0,100)
cmd_f<-c(0.01,1)
mod0<-hydromad(folsomPQCalib,sma="snow",routing="powuh",e=cmd_e,d=cmd_d,shape=cmd_shape,f=cmd_f,Tmax=15,Tmin=5)
mod_fitted<-fitByOptim(mod0,samples=1000,method="PORT")
print(mod_fitted)
fitted(mod_fitted)
residuals(mod_fitted)
plot(residuals(mod_fitted))
observed(mod_fitted)
sims_valid<-update(mod_fitted,newdata=folsomPQValid)
head(sims_valid$fitted.values)
summary(sims_valid)
xyplot(sims_valid,with.P=TRUE,xlim =as.Date(c("2005-06-01","2006-05-01")))

  

  
  
  
  
  
  
  
  
  