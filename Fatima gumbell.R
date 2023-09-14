library(readxl)
library(lmomco)
library(tidyverse)

Sheshnag <- read_excel("D:/Lidder Data/Sheshnag.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric"))

Sheshnag %>% is.na() %>% sum()

Sheshnag = Sheshnag[,c(1,2)] %>% na.omit()
Sheshnag$Date = as.Date(Sheshnag$Date)
colnames(Sheshnag) = c("date","q")
Sheshnag$date = year(Sheshnag$date)
Sheshnag %>% head()


years = unique(Sheshnag$date)
maxval = function(year) {
  max = (Sheshnag %>% filter(date == year))$q %>% max()
  return(max)
}


max_values <- sapply(years, maxval)

df <- data.frame(year = years, max_values = max_values)


Q = df$max_value  
graphlab = "1975 - 2018"

#Generate plotting positions
n = length(Q)
r = n + 1 - rank(Q)  # highest Q has rank r = 1
T = (n + 1)/r

# Set up x axis tick positions and labels
Ttick = c(1.001,1.01,1.1,1.5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,25,30,35,40,45,50,60,70,80,90,100,200)
xtlab = c(1.001,1.01,1.1,1.5,2,NA,NA,5,NA,NA,NA,NA,10,NA,NA,NA,NA,15,NA,NA,NA,NA,20,NA,30,NA,NA,NA,50,NA,NA,NA,NA,100,200)
y = -log(-log(1 - 1/T))
ytick = -log(-log(1 - 1/Ttick))
xmin = min(min(y),min(ytick))
xmax = max(ytick)

# Fit a line by method of moments, along with 95% confidence intervals
KTtick = -(0.78)*(0.5772 + log(log(Ttick/(Ttick-1))))
QTtick = mean(Q) + KTtick*sd(Q) 
nQ = length(Q)
se = (sd(Q)*sqrt((1+1.14*KTtick + 1.1*KTtick^2)))/sqrt(nQ) 
LB = QTtick - qt(0.975, nQ - 1)*se
UB = QTtick + qt(0.975, nQ - 1)*se
max = max(UB)
Qmax = max(QTtick)


#png("Sheshnag.png", width = 10, height = 8,res=1000,units="in")  # Adjust the width and height as needed

# Plot peak flow series with Gumbel axis



plot(y, Q,
     ylab = expression( "Annual Peak Flow (cfs)" ) ,
     xaxt = "n", xlab = "Return Period, T (year)",
     ylim = c(0, Qmax),
     xlim = c(xmin, xmax),
     pch = 21, bg = "red",
     main = paste( " Flood Frequency Curve for \nSheshnag using Gumbell distribution,",graphlab )
)  

axis(1, at = ytick, labels = as.character(xtlab))



# Add fitted line and confidence limits
lines(ytick, QTtick, col = "black", lty=1, lwd=2)  
lines(ytick, LB, col = "red", lty = 1, lwd=1.5)
lines(ytick, UB, col = "red", lty = 1, lwd=1.5)  

# Draw grid lines
abline(v = ytick, lty = 3, col="light gray")             

abline(h = seq(500, floor(Qmax), 500), lty = 3,col="light gray") 

# Add legend
legend("bottomright", legend = c("95% Confidence Intervals", "Observed Data",
                              "Fitted Curve (Gumbel)"), pch = c(-1, 21, -1),
       col = c("red", "red", "black"), lty = c(1, -1, 1), lwd = c(1.5, -1, 2),
       bg = "white")
#dev.off()

