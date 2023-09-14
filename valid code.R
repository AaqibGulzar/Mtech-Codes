library(moments)
sk_valid <- function(site) {
  site = filter(validation_data , site_no == site)
  sk_peak = site[, 5] %>% na.omit() %>% log10() %>% skewness()
  return(sk_peak)
}

valid.skews <- numeric()

for (i in 1:32) {
  valid.skews[i] = sk_valid(validation.set[i])
}



Flood_T_yr<-   function(site,regional_skew,quant) {  
  station_df = filter(validation_data , site_no == site)  
  station_df <- station_df[, c(2, 5)] %>% na.omit()
  station_df$rankedpeak <- sort(station_df$peak_va, decreasing = T)
  station_df$logq <- log10(station_df$rankedpeak)
  station_df$rank <- order(desc(station_df$rankedpeak))
  n = length(station_df$peak_va)
  
  for (i in station_df$rank)
  {
    station_df$ret_per[i] <- (n + 1) / station_df$rank[i]
  }
  
  station_df$exc_pro <- 1 / station_df$ret_per
  std = sd(station_df$logq)
  skew = station_df$logq %>% skewness()
  
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
  
  wt_skew <-
    (0.302 * skew + mseG * (regional_skew)) / (0.302 + mseG) #-0.109 is for Piedmont
  
  # NOW THIS FOLLOWING PART IS FOR INTERPOLATION
  gsk_values <- fft$gsk %>% as.numeric() # extract the "gsk" values from the dataset
  gsk_min <- max(gsk_values[gsk_values <= wt_skew]) # find the maximum "gsk" value less than or equal to 0.56
  gsk_max <- min(gsk_values[gsk_values >= wt_skew]) # find the minimum "gsk" value greater than or equal to 0.56
  
  # gsk_min is the lower limit, and gsk_max is the upper limit for interpolation
  
  
  RPmax <- fft[ ,quant][which(fft$gsk == gsk_max)] %>% as.numeric()
  RPmin <- fft[ ,quant][which(fft$gsk == gsk_min)] %>% as.numeric()
  
  RPint <- RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  
  K = RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  Q = mean(station_df$logq) + K * std    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
}

Flood_T_yr(valid_sites[1],estimated.skewness[1],"RP1")
Flood_T_yr(valid_sites[1],estimated.skewness[1],"RP100")


sd_q <- function(site) {
  site = filter(validation_data , site_no == site)
  sd_Q = site[ ,5] %>% na.omit() %>% sd()
  return(sd_Q)
}

sd_Q <- numeric()

for (i in 1:32) {
  sd_Q[i] = sd_q(validation.set[i])
}

val_rec_len <- function(site) {
  site = filter(validation_data , site_no == site)
  years = site[, 3] %>% na.omit() %>% format(format = "%Y")
  len = length(years)
  return(len)
}

rlv <- numeric()

for(i in 1:32){
  rlv[i] = val_rec_len(validation.set[i])
}


# Flood Frequency Curve for Station 1.(GLS)

s1_quantiles = matrix(data=NA,nrow=8,ncol=1)

for(i in 2:9) {
  s1_quantiles[i - 1, ] = Flood_T_yr(valid_sites[1],
                                     estimated.skewness[1],
                                     colnames(fft)[i])

}

RP = c("1", "2", "5", "10", "25", "50", "100", "200")

s1_quantiles = data.frame(s1_quantiles)
s1_quantiles$RP = RP
s1_quantiles =  s1_quantiles[,c(2,1)]
colnames(s1_quantiles) = c("RP","Discharge")


s1_quantiles$Discharge + 1.96*sd_Q[1]/(rlv[1])^0.5       # 10268.94
s1_quantiles$Discharge - 1.96*sd_Q[1]/(rlv[1])^0.5













