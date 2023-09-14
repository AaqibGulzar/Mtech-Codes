library(tidyverse) # AS ALWAYSSSSS!😍

site_numbs <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970",
                "02192300","02193500","02204500","02208450","02212600","02213050","02217500","02217900",
                "02219000","02219500","02220550","02220900","02221000","02221525","02335700",
                "02337000","02337400","02338660","02340500","02344500","02344700","02345000",
                "02346500","02347500","02394400","02411800","02413000","02413200","02077200",
                "02081500","02081747","02082770","02082950","02085000","0208521324","02085500",
                "02086000","02087500","02093800","02094000","02095000","02096500",
                "02098500","02099500","02100500","02101800","02102000","02114450","02120780",
                "02121500","02123500","02125000","02126000","02127000","02128000","02142900",
                "02143500","02144000","02146900","02147500","02157500","02159000",
                "02160000","02160500","02162010","02165000","02165200","02186000",
                "02192500","02196000","02044000","02044200","02044500","02046000","02051000",
                "02051500","02051600","02052500","02058400","02064000",
                "02065500","02066500","02075500","02076500","02079640")

site_numbs <- matrix(data = site_numbs,nrow = 92,ncol = 1)



#start with an example.
lat1=37.07793
long1=-78.19694
lat2=37.0346
long2=-78.17361

library(geosphere)

distm(c(long1,lat1),c(long2,lat2))  
# This gives us the distance(in metres) between two points
# using lat long .
# For our sites,we will use the following command.
# Remember the site_detail/peaks file can be formed by using the following code

library(dataRetrieval)

site_peaks <- readNWISpeak("your concerned site/sites")
site_detail <- readNWISsite("your concerned site/sites")
site_detail$dec_lat_va
site_detail$dec_long_va

#--------------------------------------------------------------

#plotting record periods on a histogram
rec_len <- function(site) {
  site = filter(site_peaks , site_no == site)
  years = site[, 3] %>% na.omit() %>% format(format = "%Y")
  Rec_length = length(years)
  return(Rec_length)
}

rec_lengths <- matrix(data = NA,nrow = 92,ncol = 1)

for(i in 1:92){
  rec_lengths[i] = rec_len(site_numbs[i])
}

Record_Lengths = data.frame(rec_lengths)

ggplot(data = Record_Lengths) + 
  geom_histogram(aes(x = rec_lengths), color ="red", fill = "skyblue",binwidth = 1) +
  theme_minimal() + scale_x_continuous(breaks = seq(25,125,10))
#-------------------------------------------------------------------------

seq(0,100,20)

d_ij=matrix(data = NA,nrow = 92,ncol = 92)
#create an empty matrix and loop the function over it
for(i in 1:92){
  for (j in 1:92){
    d_ij[i,j]= distm(
      c(site_detail$dec_long_va[i], site_detail$dec_lat_va[i]),
      c(site_detail$dec_long_va[j], site_detail$dec_lat_va[j])
    )
  }
}

d_ij = d_ij *  0.000621  #the distances will be in metres,so we convert those into miles.
# 1 metre = 0.000621 miles

# Now we calculate the cfij as given in the paper.

cf_ij <- function(siteA, siteB) {
  siteA = filter(site_peaks , site_no == siteA)
  yearsA = siteA[, 3] %>% na.omit() %>% format(format = "%Y")
  siteB = filter(site_peaks , site_no == siteB)
  yearsB = siteB[, 3] %>% na.omit() %>% format(format = "%Y")
  inters = intersect(yearsA, yearsB)
  crp <- length(inters)
  x_ob_A = length(setdiff(yearsA, inters))
  x_ob_B = length(setdiff(yearsB, inters))
  denom = ((crp + x_ob_A) * (crp + x_ob_B)) ^ 0.5
  cf_ij = (crp / denom)
  return(cf_ij)
}

#Now as usual,we loop the function over the entire dataset.
#create an empty matrix first.
cf_mat = matrix(data = NA,nrow = 92,ncol = 92)

for(i in 1:92) {
  for (j in 1:92) {
    cf_mat[i, j] = 
      cf_ij(site_numbs[i], site_numbs[j]) # This loop might take some time.⌚
  }
}


# user     system   elapsed 
# 172.19   12.81    220.95 


#Now making the smoothed correlation for the piedmont.

rho_dij <- matrix(data = NA,nrow = 92,ncol = 92)

for(i in 1:92) {
  for (j in 1:92) {
    rho_dij[i, j] =
      0.993 ^ ((2.78 * d_ij[i, j]) / (0.00989 * d_ij[i, j] + 1))  #As per the paper.
  }
}


# Finally we calculate the intersite correlation coefficient between two at site skewness
# estimators in terms of the intersite correlation coefficient.

rho_cap = matrix(data = NA,ncol = 92,nrow = 92)
for(i in 1:92) {
  for (j in 1:92) {
    rho_cap[i,j] = (cf_mat[i, j]) * (rho_dij[i, j] ^ (-0.12))  # here we try k=-0.12
  }
}

#now we create a skewness dataframe
library(moments)
sk_station <- function(site) {
  site = filter(site_peaks , site_no == site)
  sk_peak = site[ ,5] %>% na.omit() %>% log10() %>% skewness()
  return(sk_peak)
}

at_site_sk <- matrix(data = NA, nrow = 92, ncol = 1)

for (i in 1:92) {
  at_site_sk[i] = sk_station(site_numbs[i])
}

at_site_sk = data.frame(at_site_sk)


#now we calculate the standard deviations.
sd_station <- function(site) {
  site = filter(site_peaks , site_no == site)
  sd_peak = site[ ,5] %>% na.omit() %>% log10() %>% sd()
  return(sd_peak)
}

sd_site <- matrix(data = NA, nrow = 92, ncol = 1)

for (i in 1:92) {
sd_site[i] = sd_station(site_numbs[i])
}

#finally we create a covariance matrix,but it doesn`t resemble the original one😒 
covariance_matrix <- matrix(data = NA,nrow = 92,ncol = 92)

for(i  in 1:92){
  for(j in 1:92){
    covariance_matrix[i,j] = sd_site[i]*sd_site[j]*rho_cap[i,j]
  }
}

library(xlsx)
getwd()
setwd()
write.xlsx(covariance_matrix,
           file = "Covariance Matrix for Piedmont.xlsx", 
           sheetName = "Covariance Matrix")









