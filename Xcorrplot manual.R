setwd("d:/Project/HD plots/")
library(tidyverse)
library(WREG)
library(dataRetrieval)                # FOR DATA RETRIEVAL.
library(rvest)
library(moments)

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


train_coord = function(site){
  lat = filter(training_coords,site_no == site )$dec_lat_va
  long = filter(training_coords,site_no == site )$dec_long_va
  return(c(lat ,long))
}

df_coord = matrix(data=NA,nrow=60,ncol=2)
for (i in 1:60) {
  df_coord[i,] = train_coord(training.set[i]) 
}
df_coord = data.frame(training.set,df_coord)
colnames(df_coord) = c("Station.ID","Lat","Long")

print(df_coord)

sk_train <- function(site) {
  site = filter(training_data , site_no == site)
  sk_peak = site[, 5] %>% na.omit() %>% log10() %>% moments::skewness()
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



sd_training <- function(site) {
  site = filter(training_data , site_no == site)
  sd_peak = site[ ,5] %>% na.omit() %>% log10() %>% sd()
  return(sd_peak)
}

sd_site <- numeric()

for (i in 1:60) {
  sd_site[i] = sd_training(training.set[i])
}





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

K_50_yr<-   function(site) {  #-0.1753616 is the mean of est.skew values.
  station_df = filter(training_data , site_no == site)  # 0.06722554 is the mean of the obs. skew
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
  skew = moments::skewness(station_df$logq)
  
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
    (0.302 * skew + mseG * (mean(at_site_sk$Tasker.skew))) / (0.302 + mseG) 
  
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
  
  return(K50)
}

K_50_yr(site_numbs[5])

K_50 = numeric()
for(i in 1:60) {
 K_50[i]= K_50_yr(training.set[i])
}



con_rec <- function(siteA, siteB) {
  siteA = filter(training_data , site_no == siteA)
  yearsA = siteA[, 3] %>% na.omit() %>% format(format = "%Y")
  siteB = filter(training_data , site_no == siteB)
  yearsB = siteB[, 3] %>% na.omit() %>% format(format = "%Y")
  inters = intersect(yearsA, yearsB)
  crp <- length(inters)
  return(crp)
}

mat_con = matrix(data=NA,nrow=60,ncol=60)
for(i in 1:60) {
  for (j in 1:60) {
    mat_con[i, j] = con_rec(training.set[i], training.set[j])
  }
}

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
  
  if (length(inters) > 0) {
    siteA = siteA %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    siteB = siteB %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    correlation = ifelse(length(siteA$peak_va) == length(siteB$peak_va),
                         cor(log10(siteA$peak_va), log10(siteB$peak_va)),
                         NA)
    return(correlation)
  } else {
    NA
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

train_var = output_df[ ,c(-1,-2)]
train_var = cbind(at_site_sk$Tasker.skew,train_var)
colnames(train_var)[1] <- "Skew"
colnames(train_var)[2] <- "Area"
colnames(train_var)[3] <- "Slope"
colnames(train_var)[4] <- "AvgSlo"
colnames(train_var)[5] <- "Len"
colnames(train_var)[6] <- "Per"
colnames(train_var)[7] <- "Sf"
colnames(train_var)[8] <- "Meel"
colnames(train_var)[9] <- "Mael"
colnames(train_var)[10] <- "Miel"
colnames(train_var)[11] <- "Mppt"
colnames(train_var)[12] <- "Pi"
colnames(train_var)[13] <- "Pf"
colnames(train_var)[14] <- "Msd"
colnames(train_var)[15] <- "Mhi"
row.names(train_var) = NULL
train_var %>% head()

OLS = lm(data= train_var,Skew ~.)



xcorPlot <- function(alpha,theta,concurrentMin,
                     DistMeth=2,plot=TRUE) {
  # William Farmer, October 22, 2015
  # Revised by WHF, March 02, 2016
  
  #change the value to 10 if value less than 10 is selected (warn later so plot still shows up)
  xCorrWarn <- FALSE
  if (concurrentMin < 10) {
    xCorrWarn <- TRUE
    concurrentMin <- 10
  }
  
  n <- nrow(df_coord)
  plotData <- matrix(NA,ncol=2,nrow=n^2)
  iter <- 0
  maxDist <- maxcor <- -Inf
  mincor <- Inf
  for (i in 1:n) {
    for (j in i:n) {
      if (i!=j) {
        ijDist <- Dist.WREG(df_coord$Lat[i],df_coord$Long[i],
                            df_coord$Lat[j],df_coord$Long[j],method=DistMeth)
        if (mat_con[i,j]>9) {
          maxDist <- max(maxDist,ijDist)
        }
        if (mat_con[i,j] >=concurrentMin) {
          iter <- iter + 1
          plotData[iter,1] <- ijDist
          plotData[iter,2] <- reccor[i,j]
        }
      }
    }
  }
  
  ndx <- which(!is.na(rowSums(plotData)))
  plotData <- plotData[ndx,]
  tester <- reccor
  tester[rec_lengths_train<10] <- NA
  diag(tester) <- NA
  mincor <- floor(min(c(tester),na.rm=T)*10)/10
  maxcor <- ceiling(max(c(tester),na.rm=T)*10)/10
  
  estRhos <- theta^(plotData[,1]/(alpha*plotData[,1]+1))
  
  nse <- 1 - sum((estRhos-plotData[,2])^2)/
    sum((plotData[,2]-mean(plotData[,2]))^2)
  
  #warn after processing so the plot still shows up
  if(xCorrWarn){
    warn("add",paste0('It is not reccommended to use a concurrent record',
                      ' length less than 10 years. The value has been increased to 10.'))
  }
  
  if (plot) {
    ny <- round((maxcor-mincor)/0.1)
    maxpower <- 10
    splits <- 10^0
    for (i in seq(0.2,1,0.1)) {
      splits <- c(splits,10^seq(log10(i*10^1),log10(i*10^maxpower),1))
    }
    splits <- sort(splits)
    splits1 <- splits-(maxDist)
    splits1[splits1<0] <- NA
    ndx <- which(splits1==min(splits1,na.rm=T))
    xlim2 <- round(splits[ndx])
    plotDists <- seq(0,xlim2,length.out=1000)
    plotRhos <- theta^(plotDists/(alpha*plotDists+1))
    xlim <- c(0,xlim2)
    graphics::plot(plotData[,1],plotData[,2],type='p',pch=19,
                   cex.lab =1.5,
                   main=paste0('Correlation Smoothing Function\n',
                               '(alpha=',alpha,', theta=',theta,', NSE=',round(nse,digits=4),')'),
                   xlab='Distance (km)',
                   ylab='Sample correlation',
                   xaxs='i',yaxs='i',
                   ylim=c(-0.3,maxcor),xlim=xlim)
    graphics::lines(plotDists,plotRhos,lty=5,col='red',lwd = 3)
    graphics::grid(nx=round(xlim2/10^floor(log10(xlim2))*4),ny=ny)
    graphics::legend('topright',
                    lwd =2.5,
                     legend=c(paste0('Observation (Y=',concurrentMin,')'),'Model'),lty=c(NA,5),
                     col=c('black','red'),pch=c(19,NA),
                    xjust = 1,
                    yjust = 1,
                    inset = c(0.02, 0.02))
  } else {
    return(nse)
  }
}

#png("NSE.png", width = 10, height = 8, units = "in", res = 400)

xcorPlot(alpha=0.006,theta=0.98,DistMeth=2,concurrentMin=25)
# save the plot in high resolution and high quality
#dev.off()


LP3 <- data.frame(S = sd_site, K = K_50, G = at_site_sk$Tasker.skew)

#WREG.OLS(Y = at_site_sk$Tasker.skew,
 #        X = model.matrix(OLS),
  #       transY="none")

#WREG.GLS(Y = at_site_sk$Tasker.skew, X = model.matrix(OLS), 
 #        recordLengths = mat_con, LP3 = LP3,
  #                 transY = ln, basinChars = df_coord, 
   #                alpha = 0.002, theta = 0.98, distMeth = 2)





