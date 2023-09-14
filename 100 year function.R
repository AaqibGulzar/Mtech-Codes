
 library(moments) # for moment calculations
 library(tidyverse) # for data cleaning
 library(dataRetrieval) #for data retrieval from USGS
 
#mention the site numbers you want to work with.
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
 
full_peak_file <- readNWISpeak(siteNumbers =  site_numbs) #download all peak values
 
fft <- read_csv("c:/Users/Hp/Desktop/freqfactortable.csv") #load the frequency factor table
                                                           #from the desktop

fft = data.frame(fft)
 
colnames(fft) = c("gsk","1.01RP","2RP","5RP","10RP","25RP","50RP","100RP","200RP")

fftlm=lm(fft$`100RP` ~ fft$gsk) #fitting a linear model for easy interpolation.

fft_coeff=coefficients(fftlm)  #establish coefficients of the linear model.
 
 
 
#the below function is a multi-step function that will give us the 100year flood quantiles
#for the piedemont region. The regional skew ,if updated as per the region,then can be changed
#to get the 100year flood quantiles for other regions.(this is as per bulletin 17-B)

 Flood_100_yr<-   function(site, regional_skew=-.109) {
  station_df = filter(full_peak_file , site_no == site)
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
  
  gen_skew <-
    (0.302 * skew + mseG * (-0.109)) / (0.302 + mseG) #-0.109 is for piedemont
  
  k = fft_coeff[1] + fft_coeff[2] * gen_skew
  
  Q = mean(station_df$logq) + k * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
 }

# for example
 Flood_100_yr(site_numbs[1])

 #now we will try to loop the function over all the sites
 flood_quan <- matrix(data = NA, nrow = 92, ncol = 1)
 
 for (i in 1:92) {
   flood_quan[i] = Flood_100_yr(site_numbs[i])
 }
 flood_quan=as.data.frame(flood_quan)
 colnames(flood_quan) = "100 Year Discharge(cfs)"    #view(flood_quan)

 #now we save the data into an excel file.
 library(XLConnect)
 wb = loadWorkbook("c:/Users/Hp/Desktop/R Programming/excel files/cats.xlsx")
 createSheet(wb, "discharge")
 writeWorksheet(wb, flood_quan, "discharge")
 saveWorkbook(wb)
 
 #this function still needs some work to do so that it can be applied to any site across USA.

