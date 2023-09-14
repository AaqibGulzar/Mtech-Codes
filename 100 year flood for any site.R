library(moments) # for moment calculations
library(tidyverse) # for data cleaning
library(dataRetrieval) #for data retrieval from USGS

fft <- read_csv("c:/Users/ASUS/Desktop/R Programming/freqfactortable.csv") #load the frequency factor table
#from the desktop

fft = data.frame(fft)

colnames(fft) = c("gsk","1.01RP","2RP","5RP","10RP","25RP","50RP","100RP","200RP")

fftlm=lm(fft$`100RP` ~ fft$gsk) #fitting a linear model for easy interpolation.

fft_coeff=coefficients(fftlm)  #establish coefficients of the linear model.

Flood100yr<-   function(site, regional_skew=0) {

  peaks_df <- readNWISpeak(site)
  peaks_df <- peaks_df[, c(2, 5)] %>% na.omit()
  peaks_df$rankedpeak <- sort(peaks_df$peak_va, decreasing = T)
  peaks_df$logq <- log10(peaks_df$rankedpeak)
  peaks_df$rank <- order(desc(peaks_df$rankedpeak))
  n = length(peaks_df$peak_va)
  
  for (i in peaks_df$rank)
  {
    peaks_df$ret_per[i] <- (n + 1) / peaks_df$rank[i]
  }
  
  peaks_df$exc_pro <- 1 / peaks_df$ret_per
  Var = var(peaks_df$logq)
  sd = Var ^ 0.5
  skew = skewness(peaks_df$logq)
  
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
  
  gen_skew <-
    (0.302 * skew + mseG * (regional_skew)) / (0.302 + mseG) 
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(peaks_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
}
