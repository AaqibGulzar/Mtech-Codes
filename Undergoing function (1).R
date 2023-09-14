greatest_function_ever2 = function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)] %>% na.omit()
  s2 = s2[, c(3, 5)] %>% na.omit()
  
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  
  post = s1$peak_dt[match(s2$peak_dt + 1, s1$peak_dt)] %>% na.omit()
  # the post now represents those dates which are present in the s3 but not in the s4
  # and  there's only one day difference between the two with s3 leading s4 by that 1 day.
  
  pre = s1$peak_dt[match(s2$peak_dt - 1, s1$peak_dt)] %>% na.omit()
  # the pre represents the dates present in s3 but not in s4 except when lagged by a day
  # with s4 leading s3 by that 1 day.
  
  if ( length(post>0) ) next
    postframe = matrix(data = NA,
                       nrow = length(post),
                       ncol = 2)
    
    preframe = matrix(data = NA,
                      nrow = length(pre),
                      ncol = 2)
    
    for (i in 1:length(post)) {
      for (j in 1:length(pre)) {
        postframe[i, ] =  filter(s1, peak_dt == post[i]) %>% as.matrix()
        postframe = data.frame(postframe)
        colnames(postframe) = c("peak_dt", "peak_va")
        postframe$peak_dt = postframe$peak_dt %>% as.POSIXct() %>% as.Date()
        preframe[j, ] = filter(s1, peak_dt == pre[j]) %>% as.matrix()
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
    if(length(all_peak_dates1) > 20){
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
      cross_cor(site1, site2)
    }
}

library(dplyr)
library(foreach)
library(doParallel)

# first mention the number of cores you want to cluster and also
# if we use more than 10 cores,you will get warnings and even though theres a
# 9 second decrease in runtime,its not advisable as 10 cores is sufficient.
#cluster call the main library you will be using like in my case
#the pipe operator which is in magrittr package but is incorporated in dplyr.

detectCores()
#16 in my case.
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function() library(dplyr))



new_cor_mat2 =  foreach(i = 1:92,combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    greatest_function_ever(site_numbs[i], site_numbs[j])
  } %>% unlist() %>% matrix(nrow=92,ncol=92)

stopCluster(cl)
plot(c(new_cor_mat2))
