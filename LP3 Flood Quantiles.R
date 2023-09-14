library(tidyverse)
library(moments)
library(ggpubr)




fft <-
  read_csv("d:/R Programming/excel files/freqfactortable.csv")
fft = data.frame(fft)

colnames(fft) = c("gsk", "q1", "q2", "q5", "q10", "q25", "q50", "q100", "q200")

fftlm = lm(fft$q100 ~ fft$gsk) #fitting a linear model for easy interpolation.

fft_coeff = coefficients(fftlm)  #establish coefficients of the linear model.

regression_data$Aaqib_skew %>% mean()
              # = -0.08140817

#===================================================================================
# FUNCTION FOR CALCULATING FLOOD QUANTILES,ARGUMENTS TAKE SITE NO. AND RETURN PERIOD                                     =
#===================================================================================


pied_100_yr <-   function(site, quant) {    #quant should be in characters,like "q1","q100".
  station_df = filter(site_peaks , site_no == site)
  station_df = station_df[, c(2, 5)] %>% na.omit()
  station_df$rankedpeak <- sort(station_df$peak_va, decreasing = T)
  station_df$logq <- log10(station_df$rankedpeak)
  station_df$rank <- order(desc(station_df$rankedpeak))
  n = length(station_df$peak_va)
  
  for (i in station_df$rank)
  {
    station_df$ret_per[i] <- (n + 1) / station_df$rank[i]
  }
  
  station_df$exc_pro <- 1 / station_df$ret_per
  Var = var(station_df$logq)
  sd = Var ^ 0.5
  skew = skewness(station_df$logq)
  
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
    (0.302 * skew + mseG * (-0.08140817)) / (0.302 + mseG) #-0.081 is for Piedmont
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(station_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(list(colnames(fft[quant]), round(flood_Q, digits = 2)))
}

pied_100_yr(site_numbs[1],"q50") #check the class() and type of() the output.

#-------------------------------------------------------------------------------
#Constructing the CI for each Station's Skewness value(OLS)
#-------------------------------------------------------------------------------
k_upper = vector()
k_lower = vector()
for(i in 1:92){
  for(j in 1:92){
    
k_upper[i] = skews$skews[i] + 1.96*(samp_var[i]^0.5)
k_lower[j] = skews$skews[j] - 1.96*(samp_var[j]^0.5)
  }
}

# Calculate the flood quantiles for these k values.(OLS)
flood.quantile <-   function(site,quant,K) {    #quant should be in characters,like "q1","q100".
  station_df = filter(site_peaks , site_no == site)
  station_df = station_df[, c(2, 5)] %>% na.omit()
  station_df$rankedpeak <- sort(station_df$peak_va, decreasing = T)
  station_df$logq <- log10(station_df$rankedpeak)
  station_df$rank <- order(desc(station_df$rankedpeak))
  n = length(station_df$peak_va)
  
  for (i in station_df$rank)
  {
    station_df$ret_per[i] <- (n + 1) / station_df$rank[i]
  }
  
  station_df$exc_pro <- 1 / station_df$ret_per
  Var = var(station_df$logq)
  sd = Var ^ 0.5
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
    (0.302 * skew + mseG * (-0.08140817)) / (0.302 + mseG) #-0.081 is for Piedmont
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(station_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  return(round(flood_Q,digits=2))
}




# Flood Frequency Curve for Station 1.(OLS)

s1.upper = matrix(data=NA,nrow=8,ncol=1)

for(i in 2:9) {
  s1.upper[i - 1, ] = flood.quantile(ries_sites[1],
                                     colnames(fft)[i],
                                     k_upper[1])
}

s1.upper = data.frame(s1.upper)
colnames(s1.upper) = "upper"

s1.lower= matrix(data=NA,nrow=8,ncol=1)
for(i in 2:9){
  s1.lower[i-1,] =flood.quantile(ries_sites[1],
                              colnames(fft)[i],
                              k_lower[1])
}
s1.lower = data.frame(s1.lower)
colnames(s1.lower) = "lower"

RP = c("1", "2", "5", "10", "25", "50", "100", "200")

s1 = cbind(RP,s1.lower,s1.upper)

s1$RP <- factor(s1$RP, levels = unique(s1$RP)) # Convert the RP into factors,for plotting.

#-------------------------------------------------------------------------------
#Constructing the CI for each Station's Skewness value(WLS)
#-------------------------------------------------------------------------------
k.upper.wls = vector()
k.lower.wls = vector()
for(i in 1:92){
  for(j in 1:92){
    
    k.upper.wls[i] = skews$skews[i] + 1.96*(samp_var_wls[i]^0.5)
    k.lower.wls[j] = skews$skews[j] - 1.96*(samp_var_wls[j]^0.5)
  }
}

# Calculate the flood quantiles for these k values.(WLS)
flood.quantile <-   function(site,quant,K) {    #quant should be in characters,like "q1","q100".
  station_df = filter(site_peaks , site_no == site)
  station_df = station_df[, c(2, 5)] %>% na.omit()
  station_df$rankedpeak <- sort(station_df$peak_va, decreasing = T)
  station_df$logq <- log10(station_df$rankedpeak)
  station_df$rank <- order(desc(station_df$rankedpeak))
  n = length(station_df$peak_va)
  
  for (i in station_df$rank)
  {
    station_df$ret_per[i] <- (n + 1) / station_df$rank[i]
  }
  
  station_df$exc_pro <- 1 / station_df$ret_per
  Var = var(station_df$logq)
  sd = Var ^ 0.5
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
    (0.302 * skew + mseG * (-0.08140817)) / (0.302 + mseG) #-0.081 is for Piedmont
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(station_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  return(round(flood_Q,digits=2))
}




# Flood Frequency Curve for Station 1.(WLS)

s1.upper.wls = matrix(data=NA,nrow=8,ncol=1)

for(i in 2:9) {
  s1.upper.wls[i - 1, ] = flood.quantile(ries_sites[1],
                                         colnames(fft)[i],
                                         k.upper.wls[1])
}

s1.upper.wls = data.frame(s1.upper.wls)
colnames(s1.upper.wls) = "upper"

s1.lower.wls= matrix(data=NA,nrow=8,ncol=1)
for(i in 2:9){
  s1.lower.wls[i-1,] =flood.quantile(ries_sites[1],
                                     colnames(fft)[i],
                                     k.lower.wls[1])
}
s1.lower.wls = data.frame(s1.lower.wls)
colnames(s1.lower.wls) = "lower"

RP = c("1", "2", "5", "10", "25", "50", "100", "200")

s1.wls = cbind(RP,s1.lower.wls,s1.upper.wls)

s1.wls$RP <- factor(s1.wls$RP, levels = unique(s1.wls$RP))

#-------------------------------------------------------------------------------
#Constructing the CI for each Station's Skewness value(GLS)
#-------------------------------------------------------------------------------
k.upper.gls = vector()
k.lower.gls = vector()
for(i in 1:92){
  for(j in 1:92){
    
    k.upper.gls[i] = skews$skews[i] + 1.96*(samp_var_gls[i]^0.5)
    k.lower.gls[j] = skews$skews[j] - 1.96*(samp_var_gls[j]^0.5)
  }
}

# Calculate the flood quantiles for these k values.(GLS)
flood.quantile <-   function(site,quant,K) {    #quant should be in characters,like "q1","q100".
  station_df = filter(site_peaks , site_no == site)
  station_df = station_df[, c(2, 5)] %>% na.omit()
  station_df$rankedpeak <- sort(station_df$peak_va, decreasing = T)
  station_df$logq <- log10(station_df$rankedpeak)
  station_df$rank <- order(desc(station_df$rankedpeak))
  n = length(station_df$peak_va)
  
  for (i in station_df$rank)
  {
    station_df$ret_per[i] <- (n + 1) / station_df$rank[i]
  }
  
  station_df$exc_pro <- 1 / station_df$ret_per
  Var = var(station_df$logq)
  sd = Var ^ 0.5
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
    (0.302 * skew + mseG * (-0.08140817)) / (0.302 + mseG) #-0.081 is for Piedmont
  
  
  fftlm = lm(fft[, quant] ~ fft$gsk)
  
  fft_coeff = coefficients(fftlm)
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(station_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  return(round(flood_Q,digits=2))
}




# Flood Frequency Curve for Station 1.(GLS)

s1.upper.gls = matrix(data=NA,nrow=8,ncol=1)

for(i in 2:9) {
  s1.upper.gls[i - 1, ] = flood.quantile(ries_sites[1],
                                         colnames(fft)[i],
                                         k.upper.gls[1])
}

s1.upper.gls = data.frame(s1.upper.gls)
colnames(s1.upper.gls) = "upper"

s1.lower.gls= matrix(data=NA,nrow=8,ncol=1)
for(i in 2:9){
  s1.lower.gls[i-1,] =flood.quantile(ries_sites[1],
                                     colnames(fft)[i],
                                     k.lower.gls[1])
}
s1.lower.gls = data.frame(s1.lower.gls)
colnames(s1.lower.gls) = "lower"

RP = c("1", "2", "5", "10", "25", "50", "100", "200")

s1.gls = cbind(RP,s1.lower.gls,s1.upper.gls)

s1.gls$RP <- factor(s1.gls$RP, levels = unique(s1.gls$RP))

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# NOW WE CREATE THE PLOTS OF THE 3 CURVES,SEPARATELY AND THEN OVERLAY THEM.
#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

color1 <- c("95% CI for Skewness(ols)" = "darkred")         # FIRST WE 
color2 <- c("95% CI for Skewness(wls)" = "dodgerblue4")     # MAKE MANUAL
color3 <- c("95% CI for Skewness(gls)" = "violet")          # COLOR SCALES
colors <- c("95% CI for Skewness(ols)" = "red",
            "95% CI for Skewness(wls)" = "blue",
            "95% CI for Skewness(gls)" = "gold")

gols = ggplot(s1) + 
  geom_ribbon(aes(x =RP,
                  ymin = lower,                         #PLOT USING RIBBON GEOM
                  ymax = upper,
                  group ="group",
              fill = "95% CI for Skewness(ols)"),alpha = 0.5)  + 
  scale_fill_manual(values=color1) + theme_minimal() +
  labs(x = "Return Period", y = "Discharge(cfs)") +
  theme(legend.title=element_blank()) 

gwls = ggplot(s1.wls) +
  geom_ribbon(aes(x =RP,
                  ymin = lower,
                  ymax = upper,
                  group ="group",
                  fill = "95% CI for Skewness(wls)"),alpha = 0.4)  +
  scale_fill_manual(values=color2) + theme_minimal() +
  labs(x = "Return Period", y = "Discharge(cfs)") +
  theme(legend.title=element_blank()) 

ggls = ggplot(s1.gls) + 
  geom_ribbon(aes(x =RP,
                  ymin = lower,
                  ymax = upper,
                  group ="group",
                  fill = "95% CI for Skewness(gls)"),alpha = 0.4)  + 
  scale_fill_manual(values=color3) + theme_minimal() +
  labs(x = "Return Period", y = "Discharge(cfs)") +
  theme(legend.title=element_blank()) 

ggcomb = ggplot() + 
  geom_ribbon(aes(x =s1$RP,
                  ymin = s1$lower,
                  ymax = s1$upper,
                  group ="group",
                  fill = "95% CI for Skewness(ols)"),alpha = 0.4)  +
  geom_ribbon(aes(x =s1.wls$RP,
                  ymin = s1.wls$lower,
                  ymax = s1.wls$upper,
                  group ="group",
                  fill = "95% CI for Skewness(wls)"),alpha = 0.4)  +
  geom_ribbon(aes(x =s1.gls$RP,
                  ymin = s1.gls$lower,
                  ymax = s1.gls$upper,
                  group ="group",
                  fill = "95% CI for Skewness(gls)"),alpha = 0.4)  + 
  scale_fill_manual(values=colors) + theme_minimal() +
  labs(x = "Return Period", y = "Discharge(cfs)") +
  theme(legend.title=element_blank()) 

ggarrange(gols, gwls, ggls, ggcomb, nrow = 2, ncol = 2)

arrangeGrob(gols,
            gwls,
            ggls,                     #----------This may not work.
            left = "gols",
            right = "gwls",
            bottom = "ggls") %>% grid.arrange()
  
  
 ggplot(s1) + 
    geom_line(aes(y = lower,x = RP,group = "group"),color = "red") +
    geom_line(aes(x = RP,y =upper,group = "group"))
 
 
#-----------------------------------------------------------------------------------------------  
#THE CODE BELOW CAN HASN'T BEEN USED FOR ANY PLOTTING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!------------
#Now we calculate the flood quantiles for the respective return periods for all the stations----
#-----------------------------------------------------------------------------------------------

 

# 1-YEAR RETURN PERIOD
q1.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
q1.upper[i]=flood.quantile(ries_sites[i],
                           colnames(fft)[2],
                           k_upper[i]) 
}

q1.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
 q1.lower[i] =flood.quantile(ries_sites[i],
                             colnames(fft)[2],
                             k_lower[i])
 }


#2 YEAR RETURN PERIOD
q2.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q2.upper[i]=flood.quantile(ries_sites[i],
                             colnames(fft)[3],
                             k_upper[i]) 
}

q2.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q2.lower[i] =flood.quantile(ries_sites[i],
                              colnames(fft)[3],
                              k_lower[i])
}

#5 YEAR RETURN PERIOD
q5.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q5.upper[i]=flood.quantile(ries_sites[i],
                             colnames(fft)[4],
                             k_upper[i]) 
}

q5.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q5.lower[i] =flood.quantile(ries_sites[i],
                              colnames(fft)[4],
                              k_lower[i])
}

#q10 YEAR RETURN PERIOD
q10.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q10.upper[i]=flood.quantile(ries_sites[i],
                             colnames(fft)[5],
                             k_upper[i]) 
}

q10.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q10.lower[i] =flood.quantile(ries_sites[i],
                              colnames(fft)[5],
                              k_lower[i])
}

#25 YEAR RETURN PERIOD
q25.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q25.upper[i]=flood.quantile(ries_sites[i],
                             colnames(fft)[6],
                             k_upper[i]) 
}

q25.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q25.lower[i] =flood.quantile(ries_sites[i],
                              colnames(fft)[6],
                              k_lower[i])
}


#50 YEAR RETURN PERIOD
q50.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q50.upper[i]=flood.quantile(ries_sites[i],
                              colnames(fft)[7],
                              k_upper[i]) 
}

q50.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q50.lower[i] =flood.quantile(ries_sites[i],
                               colnames(fft)[7],
                               k_lower[i])
}

#100 YEAR RETURN PERIOD
q100.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q100.upper[i]=flood.quantile(ries_sites[i],
                              colnames(fft)[8],
                              k_upper[i]) 
}

q100.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q100.lower[i] =flood.quantile(ries_sites[i],
                               colnames(fft)[8],
                               k_lower[i])
}


#200 YEAR RETURN PERIOD
q200.upper = matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q200.upper[i]=flood.quantile(ries_sites[i],
                               colnames(fft)[9],
                               k_upper[i]) 
}

q200.lower= matrix(data=NA,nrow=92,ncol=1)
for(i in 1:92){
  q200.lower[i] =flood.quantile(ries_sites[i],
                                colnames(fft)[9],
                                k_lower[i])
}


