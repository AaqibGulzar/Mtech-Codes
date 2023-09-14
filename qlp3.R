# cf1 <- lapply(training.set, function(x) sapply(training.set, function(y) cf_ij_train(x, y)))
# cf2 <- do.call(rbind, cf1)


library(tidyverse)                    # FOR CLEANING.# FOR PLOTTING.
library(dataRetrieval)                # FOR DATA RETRIEVAL.
library(moments)                      # FOR SKEWNESS CALCULATION.
library(geosphere)                    # FOR FINDIND DISTANCES BETWEEN SITES.
library(sf)                           # FOR WORKING WITH SHAPE FILES.
library(raster)                       # FOR WORKING WITH RASTER FILES(NOT NEEDED HERE).
library(ggmap)                        # FOR MAPPING IN GGPLOT2.
library(rnaturalearth)                # FOR GETTING SHAPEFILE OF THE STATES.
library(ggspatial)
library(rvest)
library(smwrBase)                     # for qlp3
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
  geom_histogram(aes(x = rec_lengths_train), color ="red", fill = "skyblue",binwidth = 1) +
  theme_minimal() + scale_x_continuous(breaks = seq(25,125,10)) + labs(x = "Record Length",y =" Count")
#-------------------------------------------------------------------------

#finding the intersite distances based on coordinates

#start with an example.
lat1=37.07793
long1=-78.19694
lat2=37.0346
long2=-78.17361

distm(c(long1,lat1),c(long2,lat2))  
# This gives us the distance(in metres) between two points
# using lat long 

valid_coord = function(site){
  lat = filter(training_coords,site_no == site )$dec_lat_va
  long = filter(training_coords,site_no == site )$dec_long_va
  return(c(lat ,long))
}

df_coord = matrix(data=NA,nrow=60,ncol=2)
for (i in 1:60) {
  df_coord[i,] = valid_coord(training.set[i]) 
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

cf_ij_train <- function(siteA, siteB) {
  siteA = filter(training_data , site_no == siteA)
  yearsA = siteA[, 3] %>% na.omit() %>% format(format = "%Y")
  siteB = filter(training_data , site_no == siteB)
  yearsB = siteB[, 3] %>% na.omit() %>% format(format = "%Y")
  inters = intersect(yearsA, yearsB)
  crp <- length(inters)
  x_ob_A = length(setdiff(yearsA, inters))
  x_ob_B = length(setdiff(yearsB, inters))
  denom = ((crp + x_ob_A) * (crp + x_ob_B)) ^ 0.5
  cf_ij = (crp / denom)
  return(cf_ij)
}

# #Now as usual,we loop the function over the entire dataset.
# #create an empty matrix first.
# cf_mat_train = matrix(data = NA,nrow = 60,ncol = 60)
# 
# for(i in 1:60) {
#   for (j in 1:60) {
#     cf_mat[i, j] = 
#       cf_ij_train(training.set[i], training.set[j]) # This loop might take some time.⌚
#   }
# }
# 
# # user     system   elapsed 
# # 172.19   12.81    220.95 

#Now we do in a much faster way!!!!
library(dplyr)
library(foreach)
library(doParallel)
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function() library(dplyr))

cf_mat =
  foreach(i = 1:60,combine = cbind) %:%
  foreach (j = 1:60)  %dopar% {
    cf_ij_train(training.set[i], training.set[j]) 
  } %>% unlist() %>% matrix(nrow = 60, ncol = 60)

# user  system elapsed 
# 2.03    0.21   20.46 less than 10s



#Now making the smoothed correlation for the Piedmont.
rho_dij <- matrix(data = NA,nrow = 60,ncol = 60)

for(i in 1:60) {
  for (j in 1:60) {
    rho_dij[i, j] =
      0.993 ^ ((2.78 * d_ij[i, j]) / (0.00989 * d_ij[i, j] + 1))  #As per the paper.
  }
}


# Finally we calculate the inter site correlation coefficient between two at site skewness
# estimators in terms of the inter site correlation coefficient.

rho_cap = matrix(data = NA,ncol = 60,nrow = 60)
for(i in 1:60) {
  for (j in 1:60) {
    rho_cap[i,j] = (cf_mat[i, j]) * (rho_dij[i, j] ^ (3))  # here we try k=3
  }
}

ggplot() +
  geom_point(aes(x = d_ij, y = rho_dij), col = "darkgreen")+
  theme_classic()

#now we create a skewness dataframe

sk_train <- function(site) {
  site = filter(training_data , site_no == site)
  org_skew = site[, 5] %>% na.omit() %>% log10() %>% skewness()
  sam_size =  site[, 5] %>% na.omit() %>% length()
  tasker_skew = org_skew * (1 + (6 / sam_size))
  return(c(tasker_skew,org_skew))
}

at_site_sk <- matrix(data = NA, nrow = 60, ncol = 2)

for (i in 1:60) {
  at_site_sk[i,] = sk_train(training.set[i])
}
at_site_sk = data.frame(at_site_sk)
colnames(at_site_sk) = c("Tasker.skew","Base.Skew")
at_site_sk %>% head()


#=============================|>NOW SOME FUN TIME<|===============================
# convert the dataframe to a long format using tidyr::pivot_longer()

at_site_sk_long <-
  tidyr::pivot_longer(
    at_site_sk,
    cols = c("Tasker.skew", "Base.Skew"),
    names_to = "Variable",
    values_to = "Value"
  )

# plot using ggplot2
ggplot(at_site_sk, aes(x = seq_along(Tasker.skew), y = Tasker.skew)) +
  geom_point(aes(color = "Tasker.skew"), size = 3) +
  geom_line(aes(color = "Tasker.skew"), size = 1) +
  geom_point(aes(x = seq_along(Base.Skew), y = Base.Skew, color = "Base.Skew"), size = 3) +
  geom_line(aes(x = seq_along(Base.Skew), y = Base.Skew, color = "Base.Skew"), size = 1) +
  scale_x_continuous(breaks = seq_along(at_site_sk), labels = seq_along(at_site_sk)) +
  labs(x = "Value", y = "Value", color = "Variable") +
  theme_classic() +
  scale_color_manual(values = c("green", "blue")) +
  guides(color = guide_legend(title = "Variable"))


# Using Base R plot
plot(at_site_sk$Tasker.skew,type="o",col = "green")
points(at_site_sk$Base.Skew,type="o",col = "blue")
legend("bottom", legend = c("Tasker.skew", "Base.Skew"),
       col = c("green", "blue"), pch = 1)

#===========================|> FUN TIME OVER <|=================================


Reis_Et_Al_2020_Skew <-
  read_csv(
    "https://ecommons.cornell.edu/bitstream/handle/1813/56521.2/Reis_Et_Al_2020_Skew.txt?sequence=29&isAllowed=y",
    col_names = FALSE
  )







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
    (Var_G(training.set[i]) ^ 0.5) * (Var_G(training.set[j]) ^ 0.5) * rho_cap[i, j]
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




OLS = lm(data= train_var,`Tasker Skew` ~.)
summary(OLS)
#GLS
coeffs = matrix(OLS$coefficients, nrow = 4, ncol = 1)
x_gls = c(0, coeffs)
idm = diag(1, nrow = 60, ncol = 60)
X = model.matrix(OLS)
MLE_gls = function(x) {
  error_variance = x[1]
  parameters = matrix(x[2:5], nrow = 4, ncol = 1)
  Result =
    0.5 * log(det(error_variance * idm + cov_mat_train), base = exp(1)) +
    0.5 * t((train_var$`Tasker Skew` - X %*% parameters)) %*% solve((error_variance *
                                                                       idm + cov_mat_train)) %*%
    (train_var$`Tasker Skew` - X %*% parameters)
  return(Result)
}
#the output of gls will be 5 parameters,in the order :
#Model error varaiance,intercept,Mean ppt,Mean sdi and Mean hsi.For the prediction , The MEV is
#not included

gls = optim(par = x_gls, MLE_gls, method = "Nelder-Mead")

params = gls$par[-1]

estimated.skewness = train_var$`Mean Ppt` * params[2] +
  train_var$`Mean SDI` * params[3] +
  train_var$`Mean HSI` * params[4] +
  params[1]





# Now we Calculate the variance of prediction(old) for each site.

vp_old = function(i) {
  mat <- matrix(0, nrow = 60, ncol = 1)
    mat[i,] <- 1
  vp_old =   gls$par[1] + X[i, ] %*% solve((t(X) %*% solve(cov_mat_train)%*%X)) %*% 
    t(t(X[i, ])) - 2*gls$par[1] * X[i, ] %*% 
    solve((t(X) %*%  solve(cov_mat_train) %*% X)) %*% t(X) %*% solve(cov_mat_train)%*% mat

  return(vp_old)
}

vp_vector_old = numeric()
for(i in 1:60){
  vp_vector_old[i] = vp_old(i)
  
}

# ============= Station 1 Analysis ======================================

df1 = filter(training_data,site_no == training.set[1])

t.up = estimated.skewness[1] + 1.984*(vp_vector_old[1]^0.5)


t.lo = estimated.skewness[1] - 1.984*(vp_vector_old[1]^0.5)



dfup = 10^(qlpearsonIII(c(0.0099,0.5,0.8,0.9,0.96,0.98,0.99),meanlog=mean(log10(df1$peak_va)),
                 sdlog= vp_vector_old[1]^0.5,
                 skew=t.up) %>% log(base=exp(1))) %>% as_tibble()

dflo = 10^(qlpearsonIII(c(0.0099,0.5,0.8,0.9,0.96,0.98,0.99),meanlog=mean(log10(df1$peak_va)),
                 sdlog= vp_vector_old[1]^0.5,
                 skew=t.lo) %>% log(base=exp(1))) %>% as_tibble()


df = cbind(dfup,dflo)
colnames(df) = c("up","lo")
df















                                 
#-------------------------------------------------------------------------------
#Constructing the CI for each Station's Skewness value(GLS)
#-------------------------------------------------------------------------------
k.upper.gls = vector()
k.lower.gls = vector()
for(i in 1:60){
  for(j in 1:60){
    
    k.upper.gls[i] = estimated.skewness[i] + 1.96*((vp_vector_old[i])^0.5)
    k.lower.gls[j] = estimated.skewness[j] - 1.96*((vp_vector_old[j])^0.5)
  }
}

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


#THE GRAND FUNCTION
Flood_T_yr<-   function(site,gener.skew,RP) { 
  station_df = filter(training_data , site_no == site)  
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
  stdev = sd(station_df$logq)
  skew = skewness(station_df$logq)
  if(abs(skew)<0.9)
  {
    A = -0.33 + 0.08 * abs(skew)
  } else {
    A = -0.52 + 0.3 * abs(skew)
  }                                             # this loop is as per B17-B
  
  if (abs(skew < 1.5)) {
    B = 0.94 - 0.26 * abs(skew)
  } else {
    B = 0.5
  }
  
  mseG <- 10 ^ (A - B * log10(n / 10))
  
  wt_skew <-
    (0.302 * skew + mseG * (gener.skew)) / (0.302 + mseG)
  
  # NOW THIS FOLLOWING PART IS FOR INTERPOLATION
  gsk_values <- fft$gsk %>% as.numeric() # extract the "gsk" values from the dataset
  gsk_min <- max(gsk_values[gsk_values <= wt_skew]) # find the maximum "gsk" value less than or equal to 0.56
  gsk_max <- min(gsk_values[gsk_values >= wt_skew]) # find the minimum "gsk" value greater than or equal to 0.56
  
  # gsk_min is the lower limit, and gsk_max is the upper limit for interpolation
  
  
  RPmax <- fft[,RP][which(fft$gsk == gsk_max)] %>% as.numeric()# value of RP50 for gsk_max
  RPmin <- fft[,RP][which(fft$gsk == gsk_min)] %>% as.numeric()# value of RP50 for gsk_min
  
  RPint <- RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  
  K = RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  Q = mean(station_df$logq) + K * stdev    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
}


#Check if my loop gives the same answer for station 1 as the base qlp3 function.
# the wt_skew = -0.317


Flood_T_yr(training.set[1],estimated.skewness[1],"RP50")

10^(qlpearsonIII(0.98,meanlog=mean(log10(df1$peak_va)),
                 sdlog=sd(log10(df1$peak_va)),
                 skew=-0.317) %>% log(base=exp(1)))


# now calculating for upper and lower skewness values.








# ============= Station 1 Analysis ======================================


# for example using z value as 1.96.
Flood_T_yr(training.set[1], skew = k.upper.gls[1])
# = 105661
Flood_T_yr(training.set[1], skew = k.lower.gls[1])
# = 71398.46

#============================= T-TABLE ==========================================
t.up = estimated.skewness[1] + 1.984*(vp_vector_old[1]^0.5)


t.lo = estimated.skewness[1] - 1.984*(vp_vector_old[1]^0.5)

10^(qlpearsonIII(0.98,meanlog=mean(log10(df1$peak_va)),sdlog=sd(log10(df1$peak_va)),
                 skew=t.up) %>% log(base=exp(1)))
#upper = 105912

10^(qlpearsonIII(0.98,meanlog=mean(log10(df1$peak_va)),sdlog=sd(log10(df1$peak_va)),
                 skew=t.lo) %>% log(base=exp(1)))
# lower = 71226
#===============================================================================

#using the inbuilt function for estimated skewness
10^(qlpearsonIII(0.98,meanlog=mean(log10(df1$peak_va)),sdlog=sd(log10(df1$peak_va)),
                 skew=estimated.skewness[1]) %>% log(base=exp(1)))
# estimated = 87421.37


#using the same for the atsite skewness
10^(qlpearsonIII(0.98,meanlog=mean(log10(df1$peak_va)),sdlog=sd(log10(df1$peak_va)),
                 skew=skewness(log10(df1$peak_va))) %>% log(base=exp(1)))

# at site = 82619.49



# Flood Frequency Curve for Station 1.(OLS)

s1.upper = matrix(data=NA,nrow=8,ncol=1)

for(i in 2:9) {
  s1.upper[i - 1, ] = Flood_T_yr(site=training.set[1],
                                    RP=colnames(fft)[i],
                                    skew = t.up)
}

s1.upper = data.frame(s1.upper)
colnames(s1.upper) = "upper"

s1.lower= matrix(data=NA,nrow=8,ncol=1)
for(i in 2:9){
  s1.lower[i-1,] =Flood_T_yr(training.set[1],
                                RP = colnames(fft)[i],
                                skew= t.lo)
}
s1.lower = data.frame(s1.lower)
colnames(s1.lower) = "lower"

RP = c("1", "2", "5", "10", "25", "50", "100", "200")

s1 = cbind(RP,s1.lower,s1.upper)

s1$RP <- factor(s1$RP, levels = unique(s1$RP)) # Convert the RP into factors,for plotting.

# Check s1, its again wrong valuessssss
#------------------+--+-+--+------+--  DEAD END -----%-%--%%-----------------------------------

lo = 10^(qlpearsonIII(c(0,0.5,0.8,0.9,0.96,0.98,0.99),
                 meanlog=mean(log10(df1$peak_va)),
                 sdlog=sd(log10(df1$peak_va)),
                 skew=t.lo) %>% log(base=exp(1)))

up = 10^(qlpearsonIII(c(0,0.5,0.8,0.9,0.96,0.98,0.99),
                 meanlog=mean(log10(df1$peak_va)),
                 sdlog=sd(log10(df1$peak_va)),
                 skew=t.up) %>% log(base=exp(1)))


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


#the below function is a multi-step function that will give us the 100year flood quantiles
#for the piedemont region. The regional skew ,if updated as per the region,then can be changed
#to get the 100year flood quantiles for other regions.(this is as per bulletin 17-B)

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
Flood_50_yr(valid_sites[1],skew=estimated.skewness[1], mean(estimated.skewness[1]))


Qobs = numeric()
for (i in 1:32) {
  Qobs[i] = Flood_50_yr(valid_sites[i],obs.skew$unbiased_skew[i],mean(obs.skew$unbiased_skew)) 
}
Qest = numeric()
for (i in 1:32) {
  Qest[i] =  Flood_50_yr(valid_sites[i],skew=estimated.skewness[i], mean(estimated.skewness[i]))
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

