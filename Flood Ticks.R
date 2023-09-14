
### STEP 2
### Loading two specific packages into RWater
library(dataRetrieval) 
library(xts)

### STEP 3
### Get the Peak Annual Discharge
mysite<-site_numbs[1]
annualpeak<-readNWISpeak(mysite)

### STEP 4
### Split the downloaded data into two periods
bb71_90<-subset(annualpeak,
                peak_dt>="1953-01-10"
                &peak_dt<="1985-12-13")
bb91_10<-subset(annualpeak,
                peak_dt>="1987-03-01"
                &peak_dt<="2022-02-05")

### STEP 5
### Split the plot window in two columns
par(mfrow=c(1,2))

### STEP 6
### Perform Flood Freqency Analysis 
### Locate the column of your data set that has the peak discharges
### Click the 'bb71_90' from your workspace (upper right)
### You can see that peak discharges are stored in the 6th column (peak_va)


Q = bb71_90$peak_va  
graphlab = "1953-01-10 - 1985-12-13"

#Generate plotting positions
n = length(Q)
r = n + 1 - rank(Q)  # highest Q has rank r = 1
T = (n + 1)/r

# Set up x axis tick positions and labels
Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100)
y = -log(-log(1 - 1/T))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)

# Fit a line by method of moments, along with 95% confidence intervals
KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
QTtick = mean(Q) + KTtick*sd(Q) 
nQ = length(Q)
se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
LB = QTtick - qt(0.975, nQ - 1)*se
UB = QTtick + qt(0.975, nQ - 1)*se
max = max(UB)
Qmax = max(QTtick)

# Plot peak flow series with Gumbel axis
plot(y, Q,
     ylab = expression( "Annual Peak Flow (cfs)" ) ,
     xaxt = "n", xlab = "Return Period, T (year)",
     ylim = c(0, Qmax),
     xlim = c(xmin, xmax),
     pch = 21, bg = "red",
     main = paste( "02412000,",graphlab )
)  
par(cex = 0.65)
axis(1, at = ytick, labels = as.character(xtlab))

# Add fitted line and confidence limits
lines(ytick, QTtick, col = "black", lty=1, lwd=2)  
lines(ytick, LB, col = "blue", lty = 1, lwd=1.5)
lines(ytick, UB, col = "red", lty = 1, lwd=1.5)  

# Draw grid lines
abline(v = ytick, lty = 3, col="light gray")             
abline(h = seq(500, floor(Qmax), 500), lty = 3,col="light gray") 
par(cex = 1)


### STEP 7
### Similarly, perform Flood Freqency Analysis for the second time period

Q = bb91_10$peak_va    
graphlab = "1987-03-01 - 2022-02-05"

#Generate plotting positions
n = length(Q)
r = n + 1 - rank(Q)  # highest Q has rank r = 1
T = (n + 1)/r

# Set up x axis tick positions and labels
Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100)
y = -log(-log(1 - 1/T))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)

# Fit a line by method of moments, along with 95% confidence intervals
KTtick = -(sqrt(6)/pi)*(0.5772 + log(log(Ttick/(Ttick-1))))
QTtick = mean(Q) + KTtick*sd(Q) 
nQ = length(Q)
se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
LB = QTtick - qt(0.975, nQ - 1)*se
UB = QTtick + qt(0.975, nQ - 1)*se
max = max(UB)
Qmax = max(QTtick)

# Plot peak flow series with Gumbel axis
plot(y, Q,
     ylab = expression( "Annual Peak Flow (cfs)" ) ,
     xaxt = "n", xlab = "Return Period, T (year)",
     ylim = c(0, Qmax),
     xlim = c(xmin, xmax),
     pch = 21, bg = "red",
     main = paste( "02412000,",graphlab )
)  
par(cex = 0.65)
axis(1, at = ytick, labels = as.character(xtlab))

# Add fitted line and confidence limits
lines(ytick, QTtick, col = "black", lty=1, lwd=2)  
lines(ytick, LB, col = "blue", lty = 1, lwd=1.5)
lines(ytick, UB, col = "red", lty = 1, lwd=1.5)  

# Draw grid lines
abline(v = ytick, lty = 3, col="light gray")             
abline(h = seq(500, floor(Qmax), 500), lty = 3,col="light gray") 
par(cex = 1)

