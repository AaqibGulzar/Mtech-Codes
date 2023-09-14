library(dplyr)
library(moments)
library(WREG)
weighted_skew<-   function(site) {
  station_df = filter(site_peaks , site_no == site)
  station_df <- station_df[, c(3, 5)] %>% na.omit()
  station_df$logq <- log10(station_df$peak_va)
  n = length(station_df$peak_va)

  Var = var(station_df$logq)
  sd = Var ^ 0.5
  skew = skewness(station_df$logq)
  
  if(abs(skew)<0.9)
  {
    A = -0.33 + 0.08 * abs(skew)
  }
  else {
    A = -0.52 + 0.3 * abs(skew)
  }                                             # this loop is as per B17-B
  
  if (abs(skew < 1.5)) {
    B = 0.94 - 0.26 * abs(skew)
  } else{
    B = 0.5
  }
  
  mseG <- 10 ^ (A - B * log10(n / 10))
  
  weig_skew <-
    (0.302 * skew + mseG * (-0.101)) / (0.302 + mseG)
  return(weig_skew = weig_skew)
}
  
wt_skew = matrix(data=NA,nrow=92,ncol=1)

for(i in 1:92){
  wt_skew[i,] = weighted_skew(ries_sites$ries_sites[i])
}

plot(at_site_sk,type = "o",col ="red")
points(wt_skew,type = "o",col ="green")


lp3k = function(quant, wsk) {
  fftlm = lm(fft[,quant] ~ fft$gsk) #fitting a linear model for easy interpolation.
  fft_coeff = coefficients(fftlm)  #establish coefficients of the linear model.
  k = fft_coeff[1] + fft_coeff[2] * wsk
  return(k)
}


Quantile_k = matrix(data=NA,nrow=92,ncol=8)
for(i in 1:92){
  Quantile_k[i,1] = lp3k("q1",wt_skew[i])
  Quantile_k[i,2] = lp3k("q2",wt_skew[i])
  Quantile_k[i,3] = lp3k("q5",wt_skew[i])
  Quantile_k[i,4] = lp3k("q10",wt_skew[i])
  Quantile_k[i,5] = lp3k("q25",wt_skew[i])
  Quantile_k[i,6] = lp3k("q50",wt_skew[i])
  Quantile_k[i,7] = lp3k("q100",wt_skew[i])
  Quantile_k[i,8] = lp3k("q200",wt_skew[i])
  }

Quantile_k = data.frame(Quantile_k)
colnames(Quantile_k) = c ("q1","q2","q5","q10","q25","q50","q100","q200")
ries_sites  = data.frame(ries_sites)
Quantile_k$Staion_id = ries_sites$ries_sites
Quantile_k = Quantile_k[ ,c(9,1:8)]

X =Reis_Et_Al_2020_ExplanatoryVariables[,c(-1,-2,-17)]
X0 <- rep(1,nrow(X))
X <- cbind(X0,X)  
X <- as.matrix(X)
Y = at_site_sk
Y <- as.matrix(Y)

my_lp3 = data.frame(S = sd_site,K = Quantile_k$q100,G = wt_skew)
RL = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  RL[i,] = rec_len(ries_sites$ries_sites[i])
}
RL = as.vector(RL)
WREG.WLS(Y = Y, X= X, recordLengths = RL, LP3 = my_lp3,transY="none")

#Alhumdulilah 




