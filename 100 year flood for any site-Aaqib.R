library(moments) # for moment calculations
library(tidyverse) # for data cleaning
library(dataRetrieval) #for data retrieval from USGS
library(rvest)


#===================== |> ALHUMDULILLAH <|======================================



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


#THE GRAND FUNCTION
Flood100yr<-   function(site, regional_skew,mse = 0.302) {

  peaks_df <- readNWISpeak(site)
  peaks_df <- peaks_df[, c(3, 5)] %>% na.omit()
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
  
  wt_skew <-
    (mse * skew + mseG * (regional_skew)) / (mse + mseG) 
  
  # NOW THIS FOLLOWING PART IS FOR INTERPOLATION
  gsk_values <- fft$gsk %>% as.numeric() # extract the "gsk" values from the dataset
  gsk_min <- max(gsk_values[gsk_values <= wt_skew]) # find the maximum "gsk" value less than or equal to 0.56
  gsk_max <- min(gsk_values[gsk_values >= wt_skew]) # find the minimum "gsk" value greater than or equal to 0.56
  
  # gsk_min is the lower limit, and gsk_max is the upper limit for interpolation
  
  
  RPmax <- fft$RP100[which(fft$gsk == gsk_max)] %>% as.numeric()# value of RP100 for gsk_max
  RPmin <- fft$RP100[which(fft$gsk == gsk_min)] %>% as.numeric()# value of RP100 for gsk_min
  
  RPint <- RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  # RP100 is the estimated value of RP50 for gsk=2.64
  K100 = RPmin + (RPmax - RPmin) * (wt_skew - gsk_min) / (gsk_max - gsk_min)
  
  Q = mean(peaks_df$logq) + K100 * sd    #Log-Pearson Type 3
  
  flood_Q = 10 ^ Q
  
  return(flood_Q)
}


# EXAMPLE OF THE STATION GIVEN IN THE BOOK.
site = "02103500"
Flood100yr(site,0.4)   # 0.4 is the regional skew of this site as per the BULETTIN MAP 
                       # 14,338.48 cfs, which is almost same as they have got
                       # (Stedinger book).(14,403cfs)



# EXAMPLE OF THE SATTION GIVEN IN THE paper(here to where).
site = "01373500"      #Fish Kill near Beacon.Regional skew = 0.7
Flood100yr(site,0.7)   #this one is way off, by 9,000 cfs approximately as they have 11,000 cfs


# EXAMPLE of the  Alsea TIDEWATER (140306500)
site  = "14306500"
Flood100yr(site,0)    #this value(43,194 cfs)is also close to what Oregon State Edu 
                      #has got,43,493 cfs is what they get. 























