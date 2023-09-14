library(tidyverse)
library(dataRetrieval)

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

site_peaks <- readNWISpeak(site_numbs) #find peaks 
site_detail <- readNWISsite(site_numbs) #details



con_rec_per <- function(siteA, siteB) {
  siteA = filter(site_peaks , site_no == siteA)
  yearsA = siteA[, 3] %>% na.omit() %>% format(format = "%Y")
  siteB = filter(site_peaks , site_no == siteB)
  yearsB = siteB[, 3] %>% na.omit() %>% format(format = "%Y")
  inters = intersect(yearsA, yearsB)
  crp <- length(inters)
  return(crp)
}
con_rec_per(site_numbs[1],site_numbs[2])

#-------------------------------------------------------------------------------
# Creating a function that will give us an idea about if there's an 
#increase in the number of events,1 day prior and 1 day ahead of
#the date of maximum peak flow recorded.
#-------------------------------------------------------------------

one_day_incre_fun = function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)] %>% na.omit()
  s2 = s2[, c(3, 5)] %>% na.omit()
  
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  
  post = s1$peak_dt[match(s2$peak_dt + 1, s1$peak_dt)] %>% na.omit()
  # the post now represents those dates which are present in the s1 but not in the s2
  # and  there's only one day difference between the two with s1 leading s2 by that 1 day.
  
  pre = s1$peak_dt[match(s2$peak_dt - 1, s1$peak_dt)] %>% na.omit()
  # the pre represents the dates present in s1 but not in s2 except when lagged by a day
  # with s2 leading s1 by that 1 day.
  
  crp = con_rec_per(site1, site2)
  ratio1 = round(digits = 2, (length(x) / crp) * 100)
  ratio2 = round(digits = 2, ((length(x) + length(post) + length(pre)) /
                                crp) * 100)
  
  return(
    cat(
      "The ratio(in %) of length of Exact same day",
      "\n",
      
      "peaks to the common record period between",
      
      "\n",
      "these two stations increases from",
      ratio1,
      "to",
      ratio2,"\n","which represents the ratio of 3-day total peak matched dates"
    )
  )
}

# to simply return the values,use this function

one_day_incre = function(site1, site2) {
  
  if(site1 != site2){
    s1 =  site_peaks %>%  filter(site_no == site1)
    s2 = site_peaks %>% filter(site_no == site2)
    s1 = s1[, c(3, 5)] %>% na.omit()
    s2 = s2[, c(3, 5)] %>% na.omit()
    
    x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
    
    post = s1$peak_dt[match(s2$peak_dt + 1, s1$peak_dt)] %>% na.omit()
    # the post now represents those dates which are present in the s1 but not in the s2
    # and  there's only one day difference between the two with s1 leading s2 by that 1 day.
    
    pre = s1$peak_dt[match(s2$peak_dt - 1, s1$peak_dt)] %>% na.omit()
    # the pre represents the dates present in s1 but not in s2 except when lagged by a day
    # with s2 leading s1 by that 1 day.
    
    crp = con_rec_per(site1, site2)
    ratio1 = round(digits = 2, (length(x) / crp) * 100)
    ratio2 = round(digits = 2, ((length(x) + length(post) + length(pre)) /
                                  crp) * 100)
  return(c(ratio1,ratio2)) 
  } else {c(100,100)}
}

one_day_incre(site_numbs[1],site_numbs[1])

# the two ratios indicate when we try to find the exact peak days for station 1 with  
#other stations, say station 2, the ratio is 4.55% and when the matching window is increased to 3-days,
# the percentage bumps to 27.27%, thus meaning that there are more no.of same day peaks within
# that window. !!!!!!!!!!!!!!!!!!!!!
one_day_incre(site_numbs[1],site_numbs[2]) # 4.55 27.27



df1 = matrix(data=NA,nrow=92,ncol=2)
for(i in 1:92){
df1[i,] =  one_day_incre(site_numbs[2],site_numbs[i]) 
}
df1 = data.frame(df1)
colnames(df1) = c("r1","r2")


plot(df1$r2,col ="blue",type = "l")
lines(df3$r2,col ="red")

ggplot(df1) + 
  geom_line(aes(x = 1:92,y =r1,color = "Same Day Matching"),size = 1.4,alpha = 0.5) + 
  geom_line(aes(x = 1:92,y =r2,color ="3 Day Matching" ),size = 1.4,alpha = 0.5)+
  geom_point(aes(x = 1:92,y =r1),size = 3,alpha = 0.7) +
  geom_point(aes(x = 1:92,y =r2),size =3,color = "red",alpha = 0.7) +
  scale_x_continuous(breaks=seq(0,92,8))+
  xlab("Station Index") + ylab("Percentage Increase in Matched\nPeak Dates") +
  scale_color_manual(values=c("Same Day Matching" = "black","3 Day Matching" = "red"))+
  guides(color = guide_legend(title = "Peak Type")) +
  ggtitle("Matching of the flow peaks\nof USGS site 02413475 ") +
  theme(legend.key.size = unit(2.5, "lines"),
        plot.title = element_text(color = "dodgerblue3", size = 15,face = "bold"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background=element_rect("white"),
        panel.grid=element_line(colour="grey",linetype="dashed",linewidth=0.1),
        panel.grid.minor.y=element_line()) +
  theme(axis.title = element_text(size = 14),
        axis.title.y = element_text(size =14))

  
setwd("d:/Project/HD plots/")
#ggsave("S2.png",dpi = 800,width=30,height=15,units="cm")

















#<><><><><>><>>><<><><><><><><><><><><><><>><><><><><><><><><><><>><><><><><><><><>><><>
  con_stat_peaks <-   function(site) {
    station_df = filter(site_peaks , site_no == site)
    station_df <- station_df[, c(3, 5)] %>% na.omit()
    
    
    output = station_df %>% filter(peak_dt == "2018-10-11")
    return(if (nrow(output) > 0) {
      print("Yes ")
    } else {
      print("No")
    })
  }
  
  #TRY this.
  con_stat_peaks(site_numbs[1])
  
  
  Results <-
    matrix(data = NA, nrow = 92, ncol = 1)   #create an empty matrix to overwrite
  # with return output of above function
  
  for(i in 1:92) {
    Results[i] = con_stat_peaks(site_numbs[i])
  }
  
  
  #Create a data frame which can be plotted as points as coloured as per "yes" or "no"
  dframe = data.frame(site_detail$dec_lat_va, site_detail$dec_long_va, Results)
  colnames(dframe) = c("Lat", "Long", "Concurrent Peak ")

#///////////////////////////////////////////////////////////////////////////////  
#Three day function.  
#-------------------------------------------------------------------------------  
  three_day_incre = function(site1, site2) {
    if(site1 != site2){
    s1 =  site_peaks %>%  filter(site_no == site1)
    s2 = site_peaks %>% filter(site_no == site2)
    s1 = s1[, c(3, 5)] %>% na.omit()
    s2 = s2[, c(3, 5)] %>% na.omit()
    
    x =length(s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit())
    
    post1 = length(s1$peak_dt[match(s2$peak_dt + 1, s1$peak_dt)] %>% na.omit())
    post2 = length(s1$peak_dt[match(s2$peak_dt + 2, s1$peak_dt)] %>% na.omit())
    post3 = length(s1$peak_dt[match(s2$peak_dt + 3, s1$peak_dt)] %>% na.omit())
    # the post now represents those dates which are present in the s1 but not in the s2
    # and  there's only one day difference between the two with s1 leading s2 by that 1 day.
    
    pre1 =length( s1$peak_dt[match(s2$peak_dt - 1, s1$peak_dt)] %>% na.omit())
    pre2 = length(s1$peak_dt[match(s2$peak_dt - 2, s1$peak_dt)] %>% na.omit())
    pre3 = length(s1$peak_dt[match(s2$peak_dt - 3, s1$peak_dt)] %>% na.omit())
    # the pre represents the dates present in s1 but not in s2 except when lagged by a day
    # with s2 leading s1 by that 1 day.
    
    crp = con_rec_per(site1, site2)
    ratio1 = round(digits = 2, (x / crp) * 100)
    ratio2 = round(digits = 2, (x + post1 + post2 + post3 + pre1 + pre2 + pre3) /
                     crp * 100)
    
    return(
      c(ratio1,
        ratio2))
    } else {c(100,100)}
  }
  
  
  df3 = matrix(data=NA,nrow=92,ncol=2)
  for(i in 1:92){
    df3[i,] =  three_day_incre(site_numbs[1],site_numbs[i]) 
  }
  df3 = data.frame(df3)
  colnames(df3) = c("r1","r2")
  
 
  
    