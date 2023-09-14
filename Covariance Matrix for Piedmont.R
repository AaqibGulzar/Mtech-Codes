library(tidyverse) # AS ALWAYSSSSS!😍

ries_sites <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970",
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

ries_sites <- matrix(data = ries_sites,nrow = 92,ncol = 1)
ries_sites =as.vector(ries_sites)
site_numbs = site_detail$site_no
site_numbs = ries_sites
# For our sites,we will use the following command.
# Remember the site_detail/peaks file can be formed by using the following code

library(dataRetrieval)
site_peaks <- readNWISpeak(site_numbs)
site_detail <- readNWISsite(site_numbs)
site_detail$dec_lat_va
site_detail$dec_long_va
site_numbs =  site_detail$site_no
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
  rec_lengths[i] = rec_len(ries_sites[i])
}

Record_Lengths = data.frame(rec_lengths)

library(ggplot2)
#histogram for the record lengths in this region.
ggplot(data = Record_Lengths) + 
  geom_histogram(aes(x = rec_lengths), color ="red", fill = "skyblue",binwidth = 1) +
  theme_minimal() + scale_x_continuous(breaks = seq(25,125,10)) + labs(x = "Record Length",y =" Count")
#-------------------------------------------------------------------------

seq(0,100,20)


#start with an example.
lat1=37.07793
long1=-78.19694
lat2=37.0346
long2=-78.17361

library(geosphere)

distm(c(long1,lat1),c(long2,lat2))  
# This gives us the distance(in metres) between two points
# using lat long 

ries_coord = function(site){
lat = filter(site_detail,site_no == site )$dec_lat_va
long = filter(site_detail,site_no == site )$dec_long_va
return(c(lat ,long))
}

df_coord = matrix(data=NA,nrow=92,ncol=2)
for (i in 1:92) {
  df_coord[i,] = ries_coord(ries_sites[i]) 
}
df_coord = data.frame(df_coord)
colnames(df_coord) = c("lat","long")


#create an empty matrix and loop the function over it

d_ij=matrix(data = NA,nrow = 92,ncol = 92)
for(i in 1:92){
  for (j in 1:92){
    d_ij[i,j]= distm(
      c(df_coord$long[i], df_coord$lat[i]),
      c(df_coord$long[j], df_coord$lat[j])
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
      cf_ij(ries_sites[i], ries_sites[j]) # This loop might take some time.⌚
  }
}

# user     system   elapsed 
# 172.19   12.81    220.95 

#Now we do in a much faster way!!!!
library(dplyr)
library(foreach)
library(doParallel)
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function() library(dplyr))

cf_mat =
  foreach(i = 1:92,combine = cbind) %:%
  foreach (j = 1:92)  %dopar% {
    cf_ij(ries_sites[i], ries_sites[j]) 
    } %>% unlist() %>% matrix(nrow = 92, ncol = 92)

# user  system elapsed 
# 2.03    0.21   20.46



#Now making the smoothed correlation for the Piedmont.
rho_dij <- matrix(data = NA,nrow = 92,ncol = 92)

for(i in 1:92) {
  for (j in 1:92) {
    rho_dij[i, j] =
      0.993 ^ ((2.78 * d_ij[i, j]) / (0.00989 * d_ij[i, j] + 1))  #As per the paper.
  }
}


# Finally we calculate the inter site correlation coefficient between two at site skewness
# estimators in terms of the inter site correlation coefficient.

rho_cap = matrix(data = NA,ncol = 92,nrow = 92)
for(i in 1:92) {
  for (j in 1:92) {
    rho_cap[i,j] = (cf_mat[i, j]) * (rho_dij[i, j] ^ (3))  # here we try k=-0.12
  }
}

ggplot() +
  geom_point(aes(x = d_ij, y = rho_dij), col = "darkgreen")+
  theme_classic()

#now we create a skewness dataframe
library(moments)
sk_station <- function(site) {
  site = filter(site_peaks , site_no == site)
  sk_peak = site[, 5] %>% na.omit() %>% log10() %>% skewness()
  sam_size =  site[, 5] %>% na.omit() %>% length()
  unbiased_skew = sk_peak * (1 + (6 / sam_size))
  return(c(unbiased_skew,sk_peak))
}

at_site_sk <- matrix(data = NA, nrow = 92, ncol = 2)

for (i in 1:92) {
  at_site_sk[i,] = sk_station(ries_sites[i])
}
at_site_sk = data.frame(at_site_sk)
colnames(at_site_sk) = c("Tasker.skew","Skew")
plot(at_site_sk$Tasker.skew,type="o",col = "green")
points(at_site_sk$Skew,type="o",col = "blue")
points(skew_compare$Reis_skew,type="o",col = "red")

Reis_Et_Al_2020_Skew <-
  read_csv(
    "https://ecommons.cornell.edu/bitstream/handle/1813/56521.2/Reis_Et_Al_2020_Skew.txt?sequence=29&isAllowed=y",
    col_names = FALSE
  )

skew_compare <- data.frame(at_site_sk$at_site_sk,Reis_Et_Al_2020_Skew$skew)
colnames(skew_compare) = c("Aaqib_skew","Reis_skew")
view(skew_compare)

plot(skew_compare$Aaqib_skew, type = 'o',col = "green")
lines(skew_compare$Reis_skew, type = 'o', col = "black")







#now we calculate the standard deviations//////////////////
sd_station <- function(site) {
  site = filter(site_peaks , site_no == site)
  sd_peak = site[ ,5] %>% na.omit() %>% log10() %>% sd()
  return(sd_peak)
}

sd_site <- matrix(data = NA, nrow = 92, ncol = 1)

for (i in 1:92) {
sd_site[i] = sd_station(ries_sites[i])
}
#///////////////////////////////////////////////////////////

# Creating a function to calculate the variances of the various stations as per the paper.
#this will only calculate the variance of the site with itself and not covariance with other site.
#this will basically give us the diagonals of the covariance matrix.
Var_G <- function(site) {
  site = filter(site_peaks , site_no == site)
  size = site[, 5] %>% na.omit() %>% length()
  a = (-17.75 / (size^2)) + (50.06 / (size) ^ 3)
  b = (3.92 / (size ^ 0.3)) - (31.1 / (size ^ 0.6)) + (34.86 /(size ^ 0.9))
  c = (-7.31 / (size ^ 0.59)) + (45.9 / (size ^ 1.18)) - (86.5 /(size ^ 1.77))
  var = ((6 / size) + a) * (1 + ((9 / 6) + b) * (-0.09411405)^2  + (15 / 48 + c) * (-0.09411405 ^ 4))
  var_of_skew = var*((1 + (6/size))^2)
  return(var_of_skew)
}

variances = matrix(data = NA,nrow = 92,ncol = 1)


for(i in 1:92){
  variances[i] = Var_G(ries_sites[i])
}
variances = as.vector(variances)
var_mat = diag(variances,nrow=92,ncol=92)








#now we try to see the differences between our and their values
#for that we need to extract the diagonal entries of their dat

library(readxl)
cov_mat_ries <-
  read_excel("D:/R Programming/Reis Et Al Data/covariance matrix reis.xlsx")
View(cov_mat_ries)
cov_mat_ries = as.matrix(cov_mat_ries)
diagonals = diag(as.matrix(cov_mat_ries)) # extract its diagonals
variances = data.frame(variances)
view(variances)
df = variances %>% mutate(diagonals) # create the new df
view(df)
df = df %>% mutate(diff = variances - diagonals)
view(df)
colnames(df) = c("our", "their", "diff")
plot(df$our, df$their)
which(df$diff == max(df$diff))
which(df$diff == min(df$diff))
plot(df$their, type = 'o')
lines(df$our, type = 'o', col = 'red')


#finally we create a covariance matrix,but it does not resemble the original one😒 

cov_mat_pied <- matrix(data = NA,nrow = 92,ncol = 92)

for(i  in 1:92){
  for(j in 1:92){
    cov_mat_pied[i,j] = (Var_G(ries_sites[i])^0.5)*(Var_G(ries_sites[j])^0.5)*rho_cap[i,j]
  }
}

#faster way to loop
cov_mat_pied = foreach(i = 1:92, combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    (Var_G(ries_sites[i]) ^ 0.5) * (Var_G(ries_sites[j]) ^ 0.5) * rho_cap[i, j]
  } %>% unlist() %>% matrix(nrow = 92, ncol = 92)  

stopCluster(cl)


library(xlsx)
getwd()
setwd("d:/R Programming/excel files/")
write.xlsx(cov_mat_pied,
           file = "Covariance Matrix for Piedmont.xlsx", 
           sheetName = "Covariance Matrix")

plot(c(cov_mat_ries[1:10,1:10]),col="red")
points(c(cov_mat_pied[1:10,1:10]),col="blue")



