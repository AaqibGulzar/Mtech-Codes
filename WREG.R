# OLS Regression


Y_ries <- regression_data$skew
X_ries <- regression_data[ ,-1]

#````````````````````````
X0 <- rep(1,nrow(X_ries))
X_ries <- cbind(X0,X_ries)
transY <- 'ln'
#````````````````````````
ries.OLS <- WREG.OLS(Y_ries, X_ries,transY="none")
plot(ries.OLS$fitted.values, ries.OLS$residuals,
     main = 'Residuals versus Estimated Flow Characteristics',
     xlab = 'Estimated Flow Characteristic', ylab = 'Residual',
     xaxs = 'i', yaxs = 'i' )
plot(ries.OLS$Y, ries.OLS$ResLevInf$Leverage,
     main = 'Leverage Values versus Observations',
     xlab = 'Observation', ylab = 'Leverage Value',
     xaxs = 'i', yaxs = 'i')
abline(ries.OLS$LevLim, 0, col = 'red')

plot(ries.OLS$Y, ries.OLS$ResLevInf$Influence,
     main = 'Influence Value versus Observation',
     xlab = 'Observation', ylab = 'Influence Value',
     xaxs = 'i', yaxs = 'i')
abline(ries.OLS$InflLim, 0, col = 'red')


#WLS Regression
#````````````````````````````````````````````````
library(moments)
library(dplyr)
LP3_params = function(site) {
  sdf = site_peaks %>% filter(site_no == site)
  sdf = sdf [, 5] %>% na.omit()
  avg = mean(log10(sdf))
  std = var(log10(sdf)) ^ 0.5
  sk = skewness(log10(sdf))
  return(c(avg = avg,std = std,sk =  sk))
}

Params = matrix(data=NA,nrow=92,ncol=3)
for(i in 1:92){
  Params[i,] = LP3_params(site_numbs[i])
}

Params= as.data.frame(Params)
colnames(Params) = c("mean","std","sk")
#``````````````````````````````````````````

library(readxl)
Record_Lengths <- read_excel("D:/R Programming/Reis Et Al Data/Record Lengths.xlsx")
View(Record_Lengths)
RL <- Record_Lengths$`Record Length`


