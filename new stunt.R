# sf_usa <- ne_states(country = "united states of america", returnclass = "sf")
setwd("d:/Project/HD plots/")

library(tidyverse)                        # FOR CLEANING.# FOR PLOTTING.
library(dataRetrieval)                # FOR DATA RETRIEVAL.
library(moments)                      # FOR SKEWNESS CALCULATION.
library(geosphere)                    # FOR FINDIND DISTANCES BETWEEN SITES.
library(sf)                           # FOR WORKING WITH SHAPE FILES.
library(raster)                       # FOR WORKING WITH RASTER FILES(NOT NEEDED HERE).
library(ggmap)                        # FOR MAPPING IN GGPLOT2.
library(rnaturalearth)                # FOR GETTING SHAPEFILE OF THE STATES.
library(ggspatial)
library(rvest)

site_numbs <- c("02412000","02413475","02415000","02188500","02191200","02191300","02191970","02192300"
                ,"02193500","02204500","02208450","02212600","02213050","02217500","02217900"
                ,"02219000","02219500","02220550","02220900","02221000","02221525","02335700"
                ,"02337000","02337400","02338660","02340500","02344500","02344700","02345000"
                ,"02346500","02347500","02394400","02411800","02413000","02413200","02077200",
                "02081500","02081747","02082770","02082950","02085000","0208521324","02085500"
                ,"02086000","02087500","02093800","02094000","02095000","02096500"
                ,"02098500","02099500","02100500","02101800","02102000","02114450","02120780"
                ,"02121500","02123500","02125000","02126000","02127000","02128000","02142900"
                ,"02143500","02144000","02146900","02147500","02157500","02159000"
                ,"02160000","02160500","02162010","02165000","02165200","02186000"
                ,"02192500","02196000","02044000","02044200","02044500","02046000","02051000"
                ,"02051500","02051600","02052500","02058400","02064000"
                ,"02065500","02066500","02075500","02076500","02079640")


#Model training, selecting 60 stations,randomly.
set.seed(123)

training.set <- sample(site_numbs, 60, replace = FALSE)
validation.set <- setdiff(site_numbs, training.set)

training_coords <- readNWISsite(training.set)
training_data = readNWISpeak(training.set,endDate="2021-12-31")

validation.coords <- readNWISsite(validation.set)
validation_data = readNWISpeak(validation.set,endDate="2021-12-31")





#====================<| WORKING WITH TRAINING SET |>==================================


#plotting record periods on a histogram
rec_len <- function(site) {
  site = filter(training_data , site_no == site)
  years = site[, 3] %>% na.omit() %>% format(format = "%Y")
  Rec_length = length(years)
  return(Rec_length)
}

rec_lengths_train <- numeric()

for(i in 1:60){
  rec_lengths_train[i] = rec_len(training.set[i])
}

print(rec_lengths_train)
RLT = data.frame(rec_lengths_train)

#histogram for the record lengths in this region.
ggplot(data = RLT) + 
  geom_histogram(aes(x = rec_lengths_train), color ="dodgerblue4", fill = "dodgerblue3",binwidth = 1) +
  theme_minimal() + scale_x_continuous(breaks = seq(25,125,10)) + labs(x = "Record Length",y =" Count")
#-------------------------------------------------------------------------
#ggsave("histogram.jpeg",dpi=800)
seq(0,100,20)


#start with an example.
lat1=37.07793
long1=-78.19694
lat2=37.0346
long2=-78.17361

distm(c(long1,lat1),c(long2,lat2))  
# This gives us the distance(in metres) between two points
# using lat long 

train_coord = function(site){
  lat = filter(training_coords,site_no == site )$dec_lat_va
  long = filter(training_coords,site_no == site )$dec_long_va
  return(c(lat ,long))
}

df_coord = matrix(data=NA,nrow=60,ncol=2)
for (i in 1:60) {
  df_coord[i,] = train_coord(training.set[i]) 
}
df_coord = data.frame(df_coord)
colnames(df_coord) = c("lat","long")

print(df_coord)



# Define a function to add direction labels to dataframe columns
addDirectionToColumn <- function(df, column, direction) {
  col_values <- as.numeric(as.character(df[[column]]))
  direction_labels <-
    ifelse(col_values >= 0,
           paste0(round(col_values, 3), direction),
           paste0(round(abs(col_values), 3), direction))
  return(list(values = col_values, labels = direction_labels))
}

# Add columns for the latitude and longitude with directions
# lat_dir <- addDirectionToColumn(df_coord, "lat", "N")
# long_dir <- addDirectionToColumn(df_coord, "long", "W")
# df_coord$lat_dir_values <- lat_dir$values
# df_coord$long_dir_values <- long_dir$values

# Set up the breaks and labels for the x and y axes
x_breaks <-
  seq(floor(min(df_coord$long)), ceiling(max(df_coord$long)), by = 1)
y_breaks <-
  seq(floor(min(df_coord$lat)), ceiling(max(df_coord$lat)), by = 1)
x_labels <-
  addDirectionToColumn(data.frame(x = x_breaks), "x", "W")$labels
y_labels <-
  addDirectionToColumn(data.frame(y = y_breaks), "y", "N")$labels

# Create a ggplot object with latitude and longitude on the x and y axes, with direction labels
p <- ggplot(df_coord, aes(x = long, y = lat)) +
  geom_point() +
  labs(x = "Longitude", y = "Latitude") +
  scale_x_continuous(breaks = x_breaks,
                     labels = x_labels,
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = y_breaks,
                     labels = y_labels,
                     expand = c(0, 0))

# Display the plot
p




#create an empty matrix and loop the function over it
library(geosphere)
d_ij=matrix(data = NA,nrow = 60,ncol = 60)
for(i in 1:60){
  for (j in 1:60){
    d_ij[i,j]= distm(
      c(df_coord$long[i], df_coord$lat[i]),
      c(df_coord$long[j], df_coord$lat[j])
    )
  }
}

d_ij = d_ij *  0.000621  #the distances will be in metres,so we convert those into miles.
# 1 metre = 0.000621 miles

# Now we calculate the cfij as given in the paper.

recCor = function(siteA, siteB) {
  siteA = filter(training_data , site_no == siteA)
  siteA = siteA[, c(3, 5)] %>% na.omit()
  siteA$cal_year = year(ymd(siteA$peak_dt))
  siteA$month = month(ymd(siteA$peak_dt))
  siteA$wat_year = ifelse(siteA$month <= 9, siteA$cal_year, siteA$cal_year + 1)
  
  
  siteB = filter(training_data , site_no == siteB)
  siteB = siteB[, c(3, 5)] %>% na.omit()
  siteB$cal_year = year(ymd(siteB$peak_dt))
  siteB$month = month(ymd(siteB$peak_dt))
  siteB$wat_year = ifelse(siteB$month <= 9, siteB$cal_year, siteB$cal_year + 1)
  
  inters = intersect(siteA$wat_year, siteB$wat_year)
  
  if (length(inters) > 15) {
    siteA = siteA %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    siteB = siteB %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    correlation = ifelse(length(siteA$peak_va) == length(siteB$peak_va),
                         cor(log10(siteA$peak_va), log10(siteB$peak_va)),
                         0)
    return(correlation)
  } else {
    0
  }
}


library(foreach)
library(doParallel)
cl = makeCluster(8)
registerDoParallel(cl)
clusterCall(cl, function() {
  library(lubridate)
  library(dplyr)
})

reccor =
  foreach(i = 1:60,combine = cbind) %:%
  foreach (j = 1:60)  %dopar% {
    recCor(training.set[i], training.set[j]) 
  } %>% unlist() %>% matrix(nrow = 60, ncol = 60)



sk_train <- function(site) {
  site = filter(training_data , site_no == site)
  sk_peak = site[, 5] %>% na.omit() %>% log10() %>% skewness()
  sam_size =  site[, 5] %>% na.omit() %>% length()
  unbiased_skew = sk_peak * (1 + (6 / sam_size))
  return(c(unbiased_skew,sk_peak))
}

at_site_sk <- matrix(data = NA, nrow = 60, ncol = 2)

for (i in 1:60) {
  at_site_sk[i,] = sk_train(training.set[i])
}
at_site_sk = data.frame(at_site_sk)
colnames(at_site_sk) = c("Tasker.skew","Skew")

#===========================|> FUN TIME OVER <|=================================



#now we calculate the standard deviations//////////////////
sd_training <- function(site) {
  site = filter(training_data , site_no == site)
  sd_peak = site[ ,5] %>% na.omit() %>% log10() %>% sd()
  return(sd_peak)
}

sd_site <- numeric()

for (i in 1:60) {
  sd_site[i] = sd_training(training.set[i])
}
#///////////////////////////////////////////////////////////

# Creating a function to calculate the variances of the various stations as per the paper.
#this will only calculate the variance of the site with itself and not covariance with other site.
#this will basically give us the diagonals of the covariance matrix.
Var_G <- function(site) {
  site = filter(training_data , site_no == site)
  size = site[, 5] %>% na.omit() %>% length()
  a = (-17.75 / (size^2)) + (50.06 / (size) ^ 3)
  b = (3.92 / (size ^ 0.3)) - (31.1 / (size ^ 0.6)) + (34.86 /(size ^ 0.9))
  c = (-7.31 / (size ^ 0.59)) + (45.9 / (size ^ 1.18)) - (86.5 /(size ^ 1.77))
  var = ((6 / size) + a) * (1 + ((9 / 6) + b) * (-0.09411405)^2  + (15 / 48 + c) * (-0.09411405 ^ 4))
  var_of_skew = var*((1 + (6/size))^2)
  return(var_of_skew)
}

variances = matrix(data = NA,nrow = 60,ncol = 1)


for(i in 1:60){
  variances[i] = Var_G(training.set[i])
}
variances = as.vector(variances)
var_mat = diag(variances,nrow=60,ncol=60)









#finally we create a covariance matrix,but it does not resemble the original one😒 

# cov_mat_train <- matrix(data = NA,nrow = 60,ncol = 60)
# 
# for(i  in 1:60){
#   for(j in 1:60){
#     cov_mat_pied[i,j] = (Var_G(training.set[i])^0.5)*(Var_G(training.set[j])^0.5)*rho_cap[i,j]
#   }
# }

#faster way to loop



cov_mat_train = foreach(i = 1:60, combine = cbind) %:%
  foreach(j = 1:60) %dopar% {
    (Var_G(training.set[i]) ^ 0.5) * (Var_G(training.set[j]) ^ 0.5) * reccor[i, j]
  } %>% unlist() %>% matrix(nrow = 60, ncol = 60)  

stopCluster(cl)




#REGRESSION PART😢
Reis_et_al <- read.csv("d:/R Programming/excel files/Reis_Et_Al_2020_ExplanatoryVariables.csv")




# Remove leading zeros from each entry
train.stations <- sub("^0+", "", training.set)

# Convert the "train.stations" vector to a data frame
train.stations<- data.frame(USGS.code = train.stations)

# Merge the "train.stations_df" with "Reis_et_al" based on "USGS.code"
merged_df <- merge(train.stations, Reis_et_al, by = "USGS.code")

# Order the rows in "merged_df" based on the order of "train.stations"
output_df <- merged_df[match(train.stations$USGS.code, merged_df$USGS.code),]

train_var = output_df[ ,c(12,15,16)]
train_var = cbind(at_site_sk$Tasker.skew,train_var)
colnames(train_var) = c("Tasker Skew","Mean Ppt","Mean SDI","Mean HSI")
row.names(train_var) = NULL



#----------------------CORELOGRAM--------------------------------------------
plotdata = data.frame("dist" = as.vector(d_ij),"cor" = as.vector(reccor))
plotdata = plotdata[plotdata$cor != 0,]

plot(plotdata$dist,plotdata$cor)

#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#The follwoing algorithm checks if its correct, how do we do that ? We check the   
# results that we get from lm() with this one,we simply remove the covariance
# matrix as its zero in OLS and they are pretty close!!!!!      |   
                                                                                  

OLS = lm(data= train_var,`Tasker Skew` ~.)
summary(OLS,cor = T)
#GLS
coeffs = matrix(OLS$coefficients, nrow = 4, ncol = 1)
x_gls = c(0.01, coeffs)
idm = diag(1, nrow = 60, ncol = 60)
X = model.matrix(OLS)



# Define the objective function to be minimized
objective <- function(x) {
  error_variance <- x[1]
  parameters <- matrix(x[2:5], nrow = 4, ncol = 1)
  result <- 0.5 * log(det(error_variance * idm), base = exp(1)) +
    0.5 * t((train_var$`Tasker Skew` - X %*% parameters)) %*% solve((error_variance * idm)) %*%
    (train_var$`Tasker Skew` - X %*% parameters)
  return(result)
}


# Minimize the objective function using the optim function
optim(par = x_gls, fn = objective, method = "L-BFGS-B",
      lower = c(0,-1,-1,-1,-1), upper = c(0.3,1,1,1,1),
      control = list(maxit = 20))


# Minimize the objective function using the optim function
optim(par = x_gls, fn = objective, method = "Nelder-Mead",
                      control = list(maxit = 200))

# Get the best parameter values found by the optimization
result_optim$par

# Get the minimum value of the objective function
result_optim$value
#--------------------------------------------------------------------------------------

MLE_gls = function(x) {
  error_variance <- x[1]
  parameters <- matrix(x[2:5], nrow = 4, ncol = 1)
  result <- 0.5 * log(det(error_variance * idm + cov_mat_train), base = exp(1)) +
    0.5 * t((train_var$`Tasker Skew` - X %*% parameters)) %*% 
    solve(error_variance * idm + cov_mat_train) %*%
    matrix(train_var$`Tasker Skew` - X %*% parameters,nrow = 60,ncol = 1)
  return(result)
}

# Minimize the objective function using the optim function
gls <- optim(par = x_gls, fn = MLE_gls, method = "Nelder-Mead",
                      control = list(maxit = 454))

# Minimize the objective function using the optim function
gls = optim(par = x_gls, fn = MLE_gls, method = "L-BFGS-B",
      lower = c(0,-1,-1,-1,-1), upper = c(0.3,1,1,1,1),
      control = list(maxit = 200))
# Get the best parameter values found by the optimization
gls$par

# Get the minimum value of the objective function
gls$value

#================================|>VALIDATION<|=================================

validation_data
valid_sites = validation_data$site_no %>% unique()

#FINDING THE OBSERVED SKEWNESS VALUES FOR THE VALIDATION SITES.
obs.skew.valid <- function(site) {
  site = filter(validation_data , site_no == site)
  sk_peak = site[, 5] %>% na.omit() %>% log10() %>% skewness()
  sam_size =  site[, 5] %>% na.omit() %>% length()
  unbiased_skew = sk_peak * (1 + (6 / sam_size))
  return(c(unbiased_skew,sk_peak))
}

obs.skew <- matrix(data = NA, nrow = 32, ncol = 2)

for (i in 1:32) {
  obs.skew[i,] = obs.skew.valid(validation.set[i])
}
obs.skew = data.frame(obs.skew)
colnames(obs.skew) = c("unbiased_skew","station peak")

#FINDING THE ESTIMATED SKEWNESS VALUES FOR THE VALIDATION SITES.
# Here we will need to fit the gls model😢😒


# Remove leading zeros from each entry
valid.stations <- sub("^0+", "", validation.set)

# Convert the "train.stations" vector to a data frame
valid.stations<- data.frame(USGS.code = valid.stations)

# Merge the "train.stations_df" with "Reis_et_al" based on "USGS.code"
merged_df2 <- merge(valid.stations, Reis_et_al, by = "USGS.code")

# Order the rows in "merged_df" based on the order of "train.stations"
output_df2 <- merged_df2[match(valid.stations$USGS.code, merged_df2$USGS.code),]

valid_var = output_df2[ ,c(12,15,16)]

colnames(valid_var) = c("Mean Ppt","Mean SDI","Mean HSI")
row.names(valid_var) = NULL


params = gls$par[-1]

# using GLS parameters for estimation
estimated.skewness = valid_var$`Mean Ppt` * params[2] +
  valid_var$`Mean SDI` * params[3] +
  valid_var$`Mean HSI` * params[4] +
  params[1]





# Set the URL of the web page
url <- "https://streamflow.engr.oregonstate.edu/analysis/floodfreq/skew.htm"

# Read the HTML content of the web page
html <- read_html(url)

# Extract the table from the HTML content
table <- html %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table() 

# Clean the downloaded table for proper usage
fft = as.data.frame(table[-1:-3,])
colnames(fft) = c("gsk","RP1","RP2","RP5","RP10","RP25","RP50","RP100","RP200")
fft = fft[-1,]
row.names(fft) = NULL
fft %>% head()


#the below function is a multi-step function that will give us the 50year flood quantiles
#for the piedemont region. The regional skew ,if updated as per the region,then can be changed
#to get the 100year flood quantiles for other regions.(this is as per bulletin 17-B)

Var_Gv <- function(site) {
  site = filter(validation_data , site_no == site)
  size = site[, 5] %>% na.omit() %>% length()
  a = (-17.75 / (size^2)) + (50.06 / (size) ^ 3)
  b = (3.92 / (size ^ 0.3)) - (31.1 / (size ^ 0.6)) + (34.86 /(size ^ 0.9))
  c = (-7.31 / (size ^ 0.59)) + (45.9 / (size ^ 1.18)) - (86.5 /(size ^ 1.77))
  var = ((6 / size) + a) * (1 + ((9 / 6) + b) * (-0.09411405)^2  + (15 / 48 + c) * (-0.09411405 ^ 4))
  var_of_skew = var*((1 + (6/size))^2)
  return(var_of_skew)
}

variances = matrix(data = NA,nrow = 60,ncol = 1)


for(i in 1:60){
  variances[i] = Var_G(training.set[i])
}
variances = as.vector(variances)
var_mat = diag(variances,nrow=60,ncol=60)

Flood_50_yr<-   function(site,skew,regional_skew) {  #-0.1753616 is the mean of est.skew values.
  station_df = filter(validation_data , site_no == site)  # 0.06722554 is the mean of the obs. skew
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
  Var = var(station_df$logq)
  sd = Var ^ 0.5
  skew = skew
  
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
  
  
  RPmax <- fft$RP50[which(fft$gsk == gsk_max)] %>% as.numeric()# value of RP100 for gsk_max
  RPmin <- fft$RP50[which(fft$gsk == gsk_min)] %>% as.numeric()# value of RP100 for gsk_min
  
  RPint <- RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  # RP100 is the estimated value of RP50 for gsk=2.64
  K50 = RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  Q = mean(station_df$logq) + K50 * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
}

# for example
Flood_50_yr(valid_sites[1],skew=obs.skew$unbiased_skew[1],mean(obs.skew$unbiased_skew))
Flood_50_yr(valid_sites[1],skew=estimated.skewness[1], mean(estimated.skewness))



Qobs = numeric()
for (i in 1:32) {
  Qobs[i] = Flood_50_yr(valid_sites[i],obs.skew$unbiased_skew[i],mean(obs.skew$unbiased_skew)) 
}
Qest = numeric()
for (i in 1:32) {
  Qest[i] =  Flood_50_yr(valid_sites[i],skew=estimated.skewness[i], mean(estimated.skewness))
}

lat  = validation.coords$dec_lat_va
lon = validation.coords$dec_long_va
ratio = 100*((Qobs - Qest)/Qobs)
Quantiles = data.frame(valid_sites,Qobs,Qest,lat,lon,ratio)



sf_usa <- ne_states(country = "united states of america", returnclass = "sf")
#now the filtering is done as i only need few states,name_sv is inside the shape file itself.
sf_piedemont <-
  filter(sf_usa,
         name_sv == "South Carolina" |
           name_sv == "North Carolina" | name_sv == "Georgia" | name_sv=="Virginia" |
           name_sv == "Alabama")

min = round(digits=0,min(Quantiles$Qest))
max = round(digits =0,max(Quantiles$Qest))


#============================<| PLOTTING TIME |>========================================
#SIMPLE PLOT
#================================ ESTIMATED ===========================================
gg1 = ggplot(Quantiles) + 
  geom_point(aes(x =lon,y=lat,color = Qest ),size = 4) + 
  geom_sf(data = sf_piedemont, color = "dodgerblue4",alpha = 0.1) +
  scale_color_gradientn(colors =
                          c("darkblue", "blue", "cyan", "yellow", "red","magenta"),
                        limits = c(1000,61000),
                        breaks = seq(1000,61000,10000)) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color = guide_colorbar(title="Discharge(cfs)", nbin = 20,
                                title.position = "top",
                                barwidth=1,
                                barheight=18)) +
  theme(panel.background=element_rect("white"),
        panel.grid.major=element_line("lightgrey"),
        plot.title = element_text(color = "dodgerblue3", size = 15,
                                  face = "bold",
                                  hjust=0.1,
                                  vjust=-8)) + ggtitle("Estimated") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 15),
        panel.background=element_rect("white"),
        panel.grid=element_line(colour="grey",linetype="dashed",linewidth=0.1)) 

#================================ OBSERVED ===========================================

gg2 = ggplot(Quantiles) + 
  geom_point(aes(x =lon,y=lat,color = Qobs ),size = 4) + 
  geom_sf(data = sf_piedemont, color = "dodgerblue4",alpha = 0.1) +
  scale_color_gradientn(colors =
                          c("darkblue", "blue", "cyan", "yellow", "red","magenta"),
                        limits = c(1000,61000),
                        breaks = seq(1000,61000,10000)) +
  xlab("Longitude") + ylab("Latitude") +
  guides(color = guide_colorbar(title="Discharge(cfs)", nbin = 20,
                                title.position = "top",
                                barwidth=1,
                                barheight=18)) +
  theme(panel.background=element_rect("white"),
        panel.grid.major=element_line("lightgrey"),
        plot.title = element_text(color = "dodgerblue3", size = 15,
                                  face = "bold",
                                  hjust=0.1,
                                  vjust=-8)) + ggtitle("Observed") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  theme(legend.text = element_text(size = 9), 
        legend.title = element_text(size = 15),
        panel.background=element_rect("white"),
        panel.grid=element_line(colour="grey",linetype="dashed",linewidth=0.1)) 

#=======================================================================================

ggpubr::ggarrange(gg1,gg2) + theme_bw()


#ggsave("CombinedPlot.png",dpi = 800,width=35,height=20,units="cm")
#================================================================================
#COMPARISON PLOT
ggplot(Quantiles, aes(x = Qobs, y = Qest, color = valid_sites)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Comparison of Qobs and Qest values", x = "Qobs", y = "Qest", color = "Valid sites") +
  theme(legend.position = "bottom")




#========================== RATIO PLOT =========================================
ggplot(Quantiles) +
  geom_point(aes(x = lon, y = lat, color = ratio), size = 4) +
  geom_sf(data = sf_piedemont,
          color = "dodgerblue4",
          alpha = 0.1) +
  scale_color_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red")) +
  guides(color = guide_colorbar(
    title = "Percentage Error",
    barwidth = 1,
    barheight = 12,
    nbin = 20
  )) +
  xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(
    color = "dodgerblue3",
    size = 12,
    face = "italic"
  )) +
  theme(panel.border = element_rect(
    color = "black",
    fill = NA,
    linewidth = 1
  )) +
  theme_bw() +
  theme(axis.title = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# ggsave("Ratio.png",dpi = 800)

#===========================  BINNED PLOT ======================================
# Define the color ranges and labels
# Define the breaks and labels for binning
my_breaks <- c(-Inf,-15, -10, -5, 0, 5,10,15,Inf)
my_labels <- c( "> 15", "15 to 10", "10 to 5", "5 to 0", 
                "0 to -5", " -5 to -10","-10 to -15","< -15")
my_colors <- RColorBrewer::brewer.pal(8, "RdYlBu")
# Create the factor variable based on the ratio values
Quantiles$bin <- cut(Quantiles$ratio, breaks = my_breaks, labels = my_labels)

# Use the factor variable to assign colors
ggplot(Quantiles) + geom_sf(data = sf_piedemont,
                            color = "dodgerblue4",
                            alpha = 0.3) +
  geom_point(aes(x = lon, y = lat, color = bin),
             size = 4) +
  scale_color_manual(values = my_colors, name = "Percentage") +
  theme(plot.title = element_text(
    color = "dodgerblue3",
    size = 12,
    face = "italic"
  )) + guides(barheight = 1) + theme_light() + 
  annotation_north_arrow(
    location = "tl",
    height=unit(1,"cm"),
    width=unit(1,"cm"),
    which_north = "true",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm")
  ) + annotation_scale(location = "br") +theme(panel.border = element_rect(color = "black",
                                                                           fill = NA,
                                                                           linewidth = 1)) + theme_bw() 


#ggsave("percentage error.png",dpi = 800)












