library(tidyverse)
cross_cor <- function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)]
  s2 = s2[, c(3, 5)]
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  if (length(x) >= 4) {
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
    con_cor = cor(as.numeric(df1$peak_va1),
                  as.numeric(df2$peak_va2),
                  method = "pearson")
  return(con_cor)  
  } else {0}
}


correlation_matrix =matrix(data=NA,nrow=92,ncol=10)



#dont run this loop for the entire dataset,it gets stuck somewhere.
correlation_matrix = matrix(data=NA,nrow=92,ncol=92)
for(i in 1:92){
  for(j in 1:92)
    correlation_matrix[i,j] =  cross_cor(site_numbs[i], site_numbs[j])
}

ltd=d_ij[lower.tri(d_ij,diag=T)] %>% as.vector()
ltc=correlation_matrix[lower.tri(correlation_matrix,diag=T)]
plot(d_ij,correlation_matrix)














