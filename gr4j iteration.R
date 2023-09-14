#Output errors GR4J
library(zoo)
library(hydromad)
library(readxl)
library(xlsx)

#import the excel files for precipitation,evaporation and Qobs.
#the location path will change as per the directory.
P_E=read_excel("C:/Users/ASUS/OneDrive/Documents/Copy of GR4J_INPUTS(63).xlsx")
View(P_E)
#plot(P_E$DatesR,P_E$P,type="l")
#bind only those data columns which are needed
dat <-
  cbind(
    P = c(P_E$P),
    E = c(P_E$E))

#convert above input data to zoo format  
datz <- zoo(dat, as.Date("2010-12-31") + 1:nrow(dat))

#Step 1: Assume true params,true model and compute Qsim=Qtrue

## Define the exact model to be used
modz <- hydromad(DATA=datz, sma = "gr4j", routing = "gr4jrouting",
                 x1 = 800, x2 = 1.18, x3 = 90, x4 = 2.8)

## Calculate exact Q resulting from calculated ET
Qtrue <- modz$fitted.values
plot(P_E$DatesR,Qtrue,type="l")
#Discharge or output errors
n=length(Qtrue)
#Case a)Heterosecedastic variance
errors=rnorm(n=n,mean=0,sd=0.1*Qtrue)
Q_fake=Qtrue+errors
#plot(P_E$DatesR[1:365],Q_fake[1:365],col="red",type="l")
#lines(P_E$DatesR[1:365],Qtrue[1:365])


plot(P_E$DatesR[60:120],Q_fake[60:120],col="red",type="l")
lines(P_E$DatesR[60:120],Qtrue[60:120],col="purple")
lines(P_E$DatesR[60:120],Qsim_calib1[60:120],col="green",type="l")
abline(a = 0, b= 0 )
#Step 2: Calibrate the model using 7 year data set
# Optimising the parameter set
datC<-
  cbind(
    P = c(P_E$P),
    Q=as.numeric(Q_fake),
    E = c(P_E$E))

#convert above input data to zoo format
datCz1 <- zoo(datC, as.Date("2010-12-31") + 1:nrow(datC))

# split the data to use for calibration(7 yr data only)
Data_Cal1<- window(datCz1, start = "2011-01-01",end = "2017-12-31")

#defining the parameter ranges in the object
modCz1 <- hydromad(Data_Cal1,
                   sma="gr4j", routing="gr4jrouting",
                   x1 = c(10,2000), x2 = c(-8,6),
                   x3 =c(10,500), x4 = c(0.5,4))

#A)Use algorithm type fitbyoptim to know best params
folsomfit1 <- fitByOptim(modCz1,
                         objective=hmadstat("r.squared"),
                         method="PORT",
                         control = list(iter.max=100000),
                         vcov = FALSE)


parameters  = matrix(data=NA,nrow=10,ncol=5)  

system.time(
  for(i in 1:10){
    parameters[i,] = unlist(fitByOptim(modCz1, objective = hmadstat("r.squared"),
                                       method = "PORT", control = list(maxit=100000),
                                       initpars = NULL, multistart = FALSE)$parlist)
  }
)
#I need outputs X1,X2,X3,X4 from all runs
#You can delete samples =10000,that is not mandatory
#check if we can get NSE values from calibration and validation through for loop
#⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚⌚
# To use this function on an empty matrix for overwriting requires a nested
# for-loop and that loop can take 15-20 mins to run using base nested for loops.
# So we will do whats called a Pro-gamer`s move🤣.Lets try parallel computing,
# its gonna be a bit tricky.here we use multiple cores of the CPU to fasten
# the process (reduce loop run-time).foreach and doParallel are used to do the same.

library(dplyr)
library(foreach)
library(doParallel)
library(hydromad)
# first mention the number of cores you want to cluster and also
# if we use more than 10 cores,you will get warnings and even though theres a
# 9 second decrease in runtime,its not advisable as 10 cores is sufficient.
#cluster call the main library you will be using like in my case
#the pipe operator which is in magrittr package but is incorporated in dplyr.

detectCores()
#16 in my case.
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function()c(library(dplyr),library(lubridate),library(hydromad)))



parameters.fast = foreach(i = 1:10, combine = cbind) %dopar% {
  fitByOptim(
    modCz1,
    objective = hmadstat("r.squared"),
    method = "PORT",
    control = list(maxit = 100000),
    initpars = NULL,
    multistart = FALSE
  )$parlist
} %>% unlist() %>% matrix(nrow = 10, ncol = 5, byrow = T)

stopCluster(cl)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

update(folsomfit1, newdata = Data_Cal1)
Q_cali1 = update(folsomfit1, newdata = Data_Cal1)
Qsim_calib1=Q_cali1$fitted.values
NSE(Qsim_calib1, Qtrue[1:2557])  
update(folsomfit1, newdata = Vdatz)
Q_vali1 = update(folsomfit1, newdata = Vdatz)
Qsim_valid1 <- Q_vali1$fitted.values
#compute NSE validation period for PORT
NSE(Qsim_valid1, Qtrue[2558:3653])

#No need to check code written below

fooreps <- replicate(10, fitByOptim(modCz1, objective = hmadstat("r.squared"),
                                    method = "PORT", control = list(maxit=100000),
                                    samples = 10000,
                                    initpars = NULL, multistart = FALSE))
ptm <- proc.time()
time_taken <- proc.time() - ptm
hydrofit <- fooreps[[33]]

names(fooreps) <- paste("rep.", seq_along(fooreps))

traces <- lapply(fooreps, optimtrace)

tracesraw <- lapply(fooreps, optimtrace, raw = TRUE)

fooreps[[5]]$parlist
#model output summary
summary(folsomfit1)
print(folsomfit1) #gives convergence time
coef(folsomfit1)
folsomfit1$funevals
## return value from optim (with 'objseq' added):
str(folsomfit1$timing)

## plot objective function value convergence over time
xyplot(optimtrace(folsomfit1), type = "b",
       xlab = "function evaluations", ylab = "NSE")

#Use above optimal param set and computed Qsim for calibration
#Use 7 yr data for calibration
## Define the exact model to be used and take calib params

update(folsomfit1, newdata = Data_Cal1)
Q_cali1 = update(folsomfit1, newdata = Data_Cal1)
coef(Q_cali1)
coef(folsomfit1)

# Calculate exact Q resulting from calibration
Qsim_calib1=Q_cali1$fitted.values
library(hydroGOF)
NSE(Qsim_calib1, Qtrue[1:2557])  #0.999

mean(abs(Qsim_calib1-Qtrue[1:2557]))/mean(Qtrue[1:2557])

# plot observed vs modelled with the rainfall (Figure 5)
xyplot(folsomfit1, with.P=TRUE, 
       xlim=as.Date(c("2011-01-01", "2017-12-31")))

#Step 3: Validate the model using 3 year data set(2018-2020)
#Simulate discharge using estimated params
Vdat <-
  cbind(
    P = c(P_E$P[2558:3653]),
    E = c(P_E$E[2558:3653]))

Vdatz <- zoo(Vdat, as.Date("2017-12-31") + 1:nrow(Vdat))
update(folsomfit1, newdata = Vdatz)
Q_vali1 = update(folsomfit1, newdata = Vdatz)
coef(Q_vali1)
coef(folsomfit1)
## Calculate exact Q resulting from calibration
Qsim_valid1 <- Q_vali1$fitted.values
#compute NSE validation period for PORT
NSE(Qsim_valid1, Qtrue[2558:3653])  #0.99
plot(P_E$DatesR[2558:3653],Qsim_valid1,type="l")
#compute errors in validation period
error_port=Qsim_valid1-Q_fake[2558:3653]

plot(P_E$DatesR[2831:3195],Qsim_valid1[2831:3195],type="l")


# B)Now vary the algorithm(use algo2=dream)
library(dream)
#calibrate model using dream algo to know best params set
#in calib step,inputs are P,E,Qtrue and output is param set
set.seed(1234)
folsomfit2 <- fitByDream(modCz1,hmadstat("r.squared"), 
                         control = list(ndraw = 241))

#use optimal parameters from above calibration to determine Qsim,calib
update(folsomfit2, newdata = Data_Cal1)
Q_cali2 = update(folsomfit2, newdata = Data_Cal1)
Qsim_calib2=Q_cali2$fitted.values
library(hydroGOF)
NSE(Qsim_calib2, Q_fake[1:2557])
#model output summary
summary(folsomfit2)
coef(Q_cali2)
coef(folsomfit2)
print(folsomfit2)
## calculate corresponding objective function values over time.
xyplot(optimtrace(folsomfit2, objective = hmadstat("r.squared")),
       xlab = "function evaluations", ylab = " R Squared")

#Validation
update(folsomfit2, newdata = Vdatz)
Q_vali2 = update(folsomfit2, newdata = Vdatz)
coef(Q_vali2)
coef(folsomfit2)
## Calculate exact Q resulting from validation
Qsim_vali2 <- Q_vali2$fitted.values
#compute NSE validation period for dream
NSE(Qsim_vali2, Q_fake[2558:3653])  #0.966
#compute errors in validation period
error_dream=Qsim_vali2-Q_fake[2558:3653]


#C)Now vary the algorithm(use algo3=DE)
library(DEoptim)

#calibrate model using DE algo to know best params set
#in calib step,inputs are P,E,Qtrue and output is param set
set.seed(1234)
folsomfit3 <- fitByDE(modCz1,hmadstat("r.squared"),
                      control = DEoptim.control(itermax = 23))

#use optimal parameters from above calibration to determine Qsim,calib
update(folsomfit3, newdata = Data_Cal1)
Q_cali3 = update(folsomfit3, newdata = Data_Cal1)
Qsim_calib3 = Q_cali3$fitted.values
NSE(Qsim_calib3, Q_fake[1:2557])

#model output summary
summary(folsomfit3)
coef(Q_cali3)
coef(folsomfit3)
print(folsomfit3)

#Validation step(use above optimal params set)
update(folsomfit3, newdata = Vdatz)
Q_vali3 = update(folsomfit3, newdata = Vdatz)
coef(Q_vali3)
coef(folsomfit3)
## Calculate exact Q resulting from validation
Qsim_vali3 <- Q_vali3$fitted.values
#compute NSE validation period for PORT
NSE(Qsim_vali3, Q_fake[2558:3653])  #0.966
#compute errors in validation period
error_de=Qsim_vali3-Q_fake[2558:3653]


## plot objective function value convergence over time
xyplot(optimtrace(folsomfit3), type = "b",
       xlab = "function evaluations", ylab = "objective fn. value")



#D)Use algorithm type fitbySCE

## Set SCE control option for numbers of complex and maximum iterations

set.seed(1234)
folsomfit4 <- fitBySCE(modCz1,hmadstat("r.squared"), 
                       control = list(maxit = 8, ncomplex = 2))

#use optimal parameters from above calibration to determine Qsim,calib
update(folsomfit4, newdata = Data_Cal1)
Q_cali4 = update(folsomfit4, newdata = Data_Cal1)
Qsim_calib4 = Q_cali4$fitted.values
NSE(Qsim_calib4, Q_fake[1:2557])

#model output summary
summary(folsomfit4)
coef(Q_cali4)
coef(folsomfit4)
print(folsomfit4)

#Validation step(use above optimal params set)
update(folsomfit4, newdata = Vdatz)
Q_vali4 = update(folsomfit4, newdata = Vdatz)
coef(Q_vali4)
coef(folsomfit4)
## Calculate exact Q resulting from validation
Qsim_vali4 <- Q_vali4$fitted.values
#compute NSE validation period for PORT
NSE(Qsim_vali4, Q_fake[2558:3653])  #0.991
#compute errors in validation period
error_sce=Qsim_vali4-Q_fake[2558:3653]




datav=data.frame(P_E$DatesR[2558:3653],Qtrue[2558:3653],
                 error_port,error_dream,error_de,error_sce)
colnames(datav)=c("Year","Qtrue","optim(PORT)","DREAM","DE","SCE")


# 4 figures arranged in 2 rows and 2 columns

par(mfrow=c(2,2))
plot(datav$Year,datav$`optim(PORT)`, type='l',xlab="Year",
     ylab="",main="optim(PORT)",ylim = c(-10, 30),lty = 4,lwd=2)
lines(datav$Year, Qtrue[2558:3653], col="blue", type="l",
      lty = 1,lwd=1)
legend(x = "topright", inset=c(0,0),lty = c(1,4),lwd = c(1,2),
       col= c("blue","black","white"),text.col = "black",cex=0.7,pt.lwd=0.7, 
       legend=c("true Q", "error_PORT","NSE 0.976"),bty="n",
       x.intersp=0.2,
       y.intersp=0.1,xpd = TRUE,box.lty = 1,text.font=3, 
       box.lwd = 0.1,bg="white")


plot(datav$Year,datav$DREAM,type='l',xlab="Year",ylab="",main="DREAM",ylim = c(-10, 30),lty=3)
lines(datav$Year, Qtrue[2558:3653],  col="blue", type="l")

legend(x = "topright", inset=c(0,0),lty = c(1,3),lwd = c(1,2),
       col= c("blue","black","white"),text.col = "black",cex=0.7,pt.lwd=0.7, 
       legend=c("true Q", "error_DREAM","NSE 0.939"),bty="n",
       x.intersp=0.2,
       y.intersp=0.1,xpd = TRUE,box.lty = 1,text.font=3, 
       box.lwd = 0.1,bg="white")


plot(datav$Year,datav$DE,type='l',xlab="Year",ylab="", main="DE",ylim = c(-10, 30),lty=3)
lines(datav$Year, Qtrue[2558:3653],  col="blue", type="l")
legend(x = "topright", inset=c(0,0),lty = c(1,3),lwd = c(1,2),
       col= c("blue","black","white"),text.col = "black",cex=0.7,pt.lwd=0.7, 
       legend=c("true Q", "error_DE","NSE 0.976"),bty="n",
       x.intersp=0.2,
       y.intersp=0.1,xpd = TRUE,box.lty = 1,text.font=3, 
       box.lwd = 0.1,bg="white")


plot(datav$Year,datav$SCE,type='l',xlab="Year",ylab="", main="SCE",ylim = c(-10, 30),lty=3)
lines(datav$Year, Qtrue[2558:3653],  col="blue", type="l",lty=1)

legend(x = "topright", inset=c(0,0),lty = c(1,3),lwd = c(1,2),
       col= c("blue","black","white"),text.col = "black",cex=0.7,pt.lwd=0.7, 
       legend=c("true Q", "error_SCE","NSE 0.976"),bty="n",
       x.intersp=0.2,
       y.intersp=0.1,xpd = TRUE,box.lty = 1,text.font=3, 
       box.lwd = 0.1,bg="white")




# creating a dummy data frame
X1 = c(800,
       843.001747,
       1107.900399,
       854.602142,
       843.908897)
X2 = c(1.18,
       1.122266,  
       1.724473,  
       1.450782,  
       1.147614)
X3 = c(90,
       84.384979,   
       107.554662,    
       91.366505,   
       86.437286)
X4 = c(2.8,
       3.011322, 
       3.075539, 
       2.909929, 
       2.950183)
data <- data.frame(TYPE=c("A","B","C","D","E"),
                   X1,X2,X3,X4)
colnames(data)=c("Algo","X1","X2","X3","X4")

param1=data$X1
param2=data$X2
param3=data$X3
param4=data$X4
legends = c("A", "B", "C","D","E")
library(ggplot2)
par(mfrow = c(1, 4))
barplot(param1, main = "X1",ylim=c(0,1400),
        col = c("tan", "orange1", "magenta", "cyan", "sandybrown"))

barplot(param2, main = "X2",ylim=c(0,4),
        col = c("tan", "orange1", "magenta", "cyan", "sandybrown"))
barplot(param3, main = "X3",ylim=c(0,140),
        col = c("tan", "orange1", "magenta", "cyan", "sandybrown"))
barplot(param4, main = "X4",ylim=c(0,4),
        col = c("tan", "orange1", "magenta", "cyan", "sandybrown"))
legend = c("True","PORT","DREAM","DE","SCE")
fill = c( "tan", "orange1", "magenta", "cyan", "sandybrown")

par(oma = c(4,1,1,1), mfrow = c(2, 3), mar = c(2, 2, 1, 1))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'h', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend =legend , fill=fill,lwd = 0,
       text.width=c(0.035,0.035,0.055,0.015),x.intersp=0.2,
       xpd = TRUE, horiz = TRUE, cex = 1, seg.len=0, bty = 'n')


lines(1:3653, Qtrue, col='red')
abs(Qsim_calib1-Qtrue[1:2557])/mean(Qtrue[1:2557])
sum(abs(Qsim_calib1-Qtrue[1:2557]))/mean(Qtrue[1:2557])
mean(abs(Qsim_calib1-Qtrue[1:2557]))/mean(Qtrue[1:2557])


#Discharge or output errors
#Case b)Homosecedastic variance
n=length(Qtrue)
errors=rnorm(n=n,mean=0,sd=0.25)
hist(errors)
Q_fake=Qtrue+errors
plot(P_E$DatesR[1:365],Q_fake[1:365],col="red",type="l")
lines(P_E$DatesR[1:365],Qtrue[1:365])
#Step 2: Calibrate the model using 7 year data set
#A)Use algorithm type fitbyoptim to know best params
set.seed(123)
folsomfit1 <- fitByOptim(modCz1,
                         objective=hmadstat("r.squared"),
                         method="PORT",
                         control = list(iter.max=100000),
                         vcov = FALSE)

update(folsomfit1, newdata = Data_Cal1)
Q_cali1 = update(folsomfit1, newdata = Data_Cal1)
Qsim_calib1=Q_cali1$fitted.values
library(hydroGOF)
NSE(Qsim_calib1, Qtrue[1:2557])  #0.999

plot(P_E$DatesR[60:120],Q_fake[60:120],col="red",type="l")
lines(P_E$DatesR[60:120],Qtrue[60:120],col="purple")
lines(P_E$DatesR[60:120],Qsim_calib1[60:120],col="green",type="l")
