library(tidyverse)
library(geosphere)
library(moments)
site_numbs= site_detail$site_no

trial_cor <- function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)] %>% na.omit()
  s2 = s2[, c(3, 5)] %>% na.omit()
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  if (length(x) > 15 ) {
    df1 = matrix(data = NA,
                 nrow = length(x),
                 ncol = 2)
    df2 = matrix(data = NA,
                 nrow = length(x),
                 ncol = 2)
    for (i in 1:length(x)) {
      df1[i,] = filter(s1, peak_dt == x[i]) %>% as.matrix()
      df1 = data.frame(df1)
      colnames(df1) = c("peak_dt1", "peak_va1")
      df2[i,] = filter(s2, peak_dt == x[i]) %>% as.matrix()
      df2 = data.frame(df2)
      colnames(df2) = c("peak_dt2", "peak_va2")
    }
    con_cor = cor( log10(as.numeric(df1$peak_va1)),
                   log10(as.numeric(df2$peak_va2)),
                   method = "pearson")
    return(con_cor)  
  } else {0}
}

trial_fun = function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)] %>% na.omit()
  s2 = s2[, c(3, 5)] %>% na.omit()
  
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  #this gives exact peak date matches
  
  post = s1$peak_dt[match(s2$peak_dt + 1, s1$peak_dt)] %>% na.omit()
  # the post now represents those dates which are present in the s1 but not in the s2
  # and  there's only one day difference between the two with s3 leading s4 by that 1 day.
  
  pre = s1$peak_dt[match(s2$peak_dt - 1, s1$peak_dt)] %>% na.omit()
  # the pre represents the dates present in s1 but not in s2 except when lagged by a day
  # with s2 leading s1 by that 1 day.
  
  if( length(post)>1 & length(pre) > 1 & length(x) > 15 ){
    postframe = matrix(data = NA,
                       nrow = length(post),
                       ncol = 2)
    
    preframe = matrix(data = NA,
                      nrow = length(pre),
                      ncol = 2)
    
    for (i in 1:length(post)) {
      for (j in 1:length(pre)) {
        postframe[i, ] =  filter(s1, peak_dt == post[i]) %>% as.matrix()
        preframe[j, ] = filter(s1, peak_dt == pre[j]) %>% as.matrix()
    
        postframe = data.frame(postframe)
        colnames(postframe) = c("peak_dt", "peak_va")
        postframe$peak_dt = postframe$peak_dt %>% as.POSIXct() %>% as.Date()
       
        preframe = data.frame(preframe)
        colnames(preframe) = c("peak_dt", "peak_va")
        preframe$peak_dt = preframe$peak_dt %>% as.POSIXct() %>% as.Date()
      }
    }
    
    
    #so the preframes and postframes are  contained in the s1 file while x is in both.
    #so to find the correlation between these peak values, we have to get the peaks of the
    #s2 corresponding to these dates.post -1 and pre + 1 and x together will be present in s2.
    
    all_peak_dates1 =  c(x, preframe$peak_dt, postframe$peak_dt)
    all_peak_dates2 =  c(x, preframe$peak_dt + 1, postframe$peak_dt - 1)
    all_peaks1 = matrix(data = NA,
                        nrow = length(all_peak_dates1),
                        ncol = 2)
    all_peaks2 = matrix(data = NA,
                        nrow = length(all_peak_dates2),
                        ncol = 2)
    for (i in 1:length(all_peak_dates1)) {
      for (j in 1:length(all_peak_dates2)) {
        all_peaks1[i,] =  filter(s1, peak_dt == all_peak_dates1[i]) %>% as.matrix()
        all_peaks1 = data.frame(all_peaks1)
        colnames(all_peaks1) = c("peak_dt1", "peak_va1")
        all_peaks2[j,] = filter(s2, peak_dt == all_peak_dates2[j]) %>% as.matrix()
        all_peaks2 = data.frame(all_peaks2)
        colnames(all_peaks2) = c("peak_dt2", "peak_va2")
      }
    }
    
    return(cor(log10(as.numeric(
      all_peaks1$peak_va1
    )),
    log10(as.numeric(
      all_peaks2$peak_va2
    )),
    method = "pearson"))} else {
      trial_cor(site1, site2)
    }
}

library(dplyr)
library(foreach)
library(doParallel)

# first mention the number of cores you want to cluster and also
# if we use more than 10 cores,you will get warnings and even though theres a
# 9 second decrease in run-time,its not advisable as 10 cores is sufficient.
#cluster call the main library you will be using like in my case
#the pipe operator which is in magrittr package but is incorporated in dplyr.

detectCores()
#16 in my case.
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function() library(dplyr))



trial_mat =  foreach(i = 1:92,combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    trial_fun(site_numbs[i], site_numbs[j])
  } %>% unlist() %>% matrix(nrow=92,ncol=92)

stopCluster(cl)
d_ij=matrix(data = NA,nrow = 92,ncol = 92)
for(i in 1:92){
  for (j in 1:92){
    d_ij[i,j]= distm(
      c(site_detail$dec_long_va[i], site_detail$dec_lat_va[i]),
      c(site_detail$dec_long_va[j], site_detail$dec_lat_va[j])
    )
  }
}

dist_mile = 0.000621*d_ij

cor_vec2 = c(trial_mat[upper.tri(trial_mat,diag=T)])
dist_vec = c( dist_mile[upper.tri(dist_mile,diag=T)])
df= data.frame(dist_vec,cor_vec2)
df = df %>% na.omit()
colnames(df) = c("dist(miles)","corellation")
df2 = df %>% filter(corellation < 0.99 & corellation !=0 )
plot(df2$`dist(miles)`,df2$corellation)
abline(lm(df2$corellation~df2$`dist(miles)`))
ggplot(df2,
       aes(x = `dist(miles)`,
           y = corellation)) +  geom_jitter() +  geom_smooth(method="lm") + theme_classic() +
  ggtitle("Correlation of Concurrent Annual Maximum Peaks against the distance between the stations")




trial_mat =  foreach(i = 1:92,combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    trial_cor(site_numbs[i], site_numbs[j])
  } %>% unlist() %>% matrix(nrow=92,ncol=92)





x = length(foreach(i = 1:92, combine = cbind) %:%
             foreach(j = 1:92) %dopar% {
               if (d_ij[i, j] < as.numeric(20000) & d_ij[i, j] > 0) {
                 cross_cor(site_numbs[i], site_numbs[j])
               }
             } %>% unlist())

cor_2 =
  foreach(i = 1:92, combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    if (d_ij[i, j] < 20000 & d_ij[i, j] > 0) {
      cross_cor(site_numbs[i], site_numbs[j])
    }
  } %>% unlist() %>% matrix(nrow = x, ncol = 1)
cor_2 = data.frame(cor_2)
cor_2$logic <-  as.vector(cor_2 == 0)
colnames(cor_2) = c("correlation", "Equals0")




plot_c = ggplot(cor_2) +
  geom_bar(aes(x = Equals0, fill = Equals0)) +
  labs(x = "correlation  = 0") + guides(fill = guide_legend(title = ""))




