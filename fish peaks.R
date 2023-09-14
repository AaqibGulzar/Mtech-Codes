# The following will try to reproduce the results of the paper published by
# Chowdhury and Stedinger,1991
#working on the following site,lets see if i get the Flood Quantiles right!!!!!!


# Fishkill creek, New York <- 01373500
library(tidyverse)
library(moments)
library(ggpubr)
library(dataRetrieval)
fish_peaks <- readNWISpeak("01373500")
fish_detail <- readNWISsite("01373500")

fish_peaks$peak_va


fft <-
  read_csv("d:/R Programming/excel files/freqfactortable.csv")
fft = data.frame(fft)

colnames(fft) = c("gsk", "q1", "q2", "q5", "q10", "q25", "q50", "q100", "q200")

fish_return_yr <-   function(fish_peaks,quant) {
  fish_peaks = fish_peaks[, c(2, 5)] %>% na.omit()
  fish_peaks$rankedpeak <- sort(fish_peaks$peak_va, decreasing = T)
  fish_peaks$logq <- log10(fish_peaks$rankedpeak)
  fish_peaks$rank <- order(desc(fish_peaks$rankedpeak))
  n = length(fish_peaks$peak_va)
  
  for (i in fish_peaks$rank)
  {
    fish_peaks$ret_per[i] <- (n + 1) / fish_peaks$rank[i]
  }
  
  fish_peaks$exc_pro <- 1 / fish_peaks$ret_per
  sd = sd(log10(fish_peaks$peak_va))
  skew = skewness(fish_peaks$logq)
  
  if (abs(skew) < 0.9)
  {
    A = -0.33 + 0.08 * abs(skew)
  }
  else {
    A = -0.52 + 0.3 * abs(skew)
  }                                             # this loop is as per B17-B
  
  if (abs(skew < 1.5)) {
    B = 0.94 - 0.26 * abs(skew)
  } else {
    B = 0.5
  }
  
  mseG <- 10 ^ (A - B * log10(n / 10))
  
  gen_skew <-
    (0.302 * skew + mseG * (0.59)) / (0.302 + mseG) #0.7 is the regional skew for this area.
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(fish_peaks$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(list(colnames(fft[quant]), round(flood_Q, digits = 2)))
}

fish_return_yr(fish_peaks,"q50") #check the class() and type of() the output.

#-------------------------------------------------------------------------------
#Constructing the CI for each Station's Skewness value(OLS)
#-------------------------------------------------------------------------------

    k_upper = skewness(log10(fish_peaks$peak_va)) + 1.96*(sd(log10(fish_peaks$peak_va)))
    k_lower = skewness(log10(fish_peaks$peak_va)) - 1.96*(sd(log10(fish_peaks$peak_va)))
    
# Calculate the flood quantiles for these k values.(OLS)
flood.quantile.fsih <-   function(fish_peaks,quant,K) {    #quant should be in characters,like "q1","q100".
  fish_peaks = fish_peaks[, c(2, 5)] %>% na.omit()
  fish_peaks$rankedpeak <- sort(fish_peaks$peak_va, decreasing = T)
  fish_peaks$logq <- log10(fish_peaks$rankedpeak)
  fish_peaks$rank <- order(desc(fish_peaks$rankedpeak))
  n = length(fish_peaks$peak_va)
  
  for (i in fish_peaks$rank)
  {
    fish_peaks$ret_per[i] <- (n + 1) / fish_peaks$rank[i]
  }
  
  fish_peaks$exc_pro <- 1 / fish_peaks$ret_per
  sd = sd(log10(fish_peaks$peak_va))
  skew = K
  
  if (abs(skew) < 0.9)
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
    (0.302 * skew + mseG * (0.59)) / (0.302 + mseG) #0.59 for fish kill
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(fish_peaks$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  return(round(flood_Q,digits=2))
}
