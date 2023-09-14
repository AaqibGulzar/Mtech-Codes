library(dplyr)
library(geosphere)

#This function is a very good function,i like it. it will give which pairs have 
#a corelation and their distances too so yeah this is great i am proud of myself


cor_dist_fun <- function(site1, site2) {
  s1 =  site_peaks %>%  filter(site_no == site1)
  s2 = site_peaks %>% filter(site_no == site2)
  s1 = s1[, c(3, 5)] %>% na.omit()
  s2 = s2[, c(3, 5)] %>% na.omit()
  d1 = filter(site_detail, site_no == site1)
  d2 = filter(site_detail, site_no == site2)
  x = s1$peak_dt[match(s2$peak_dt, s1$peak_dt)] %>% na.omit()
  if (length(x) > 4 & con_rec_per(site1, site2) > 40) {
    dist =  0.000621 * distm(c(d1$dec_long_va, d1$dec_lat_va),
                             c(d2$dec_long_va, d2$dec_lat_va))
    
    df1 = matrix(data = NA,
                 nrow = length(x),
                 ncol = 2)
    df2 = matrix(data = NA,
                 nrow = length(x),
                 ncol = 2)
    for (i in 1:length(x)) {
      df1[i, ] = filter(s1, peak_dt == x[i]) %>% as.matrix()
      df1 = data.frame(df1)
      colnames(df1) = c("peak_dt1", "peak_va1")
      df2[i, ] = filter(s2, peak_dt == x[i]) %>% as.matrix()
      df2 = data.frame(df2)
      colnames(df2) = c("peak_dt2", "peak_va2")
    }
    con_cor = cor(log10(as.numeric(df1$peak_va1)),
                  log10(as.numeric(df2$peak_va2)),
                  method = "pearson")
    return(c(con_cor, dist))
  } else {c( 0, 0.000621 * distm(
      c(d1$dec_long_va, d1$dec_lat_va),
      c(d2$dec_long_va, d2$dec_lat_va)
    ))
  }
}


finally1 = matrix(data=NA,nrow= 10,ncol=1)
finally2 = matrix(data=NA,nrow=10,ncol=1)

for(i in 1:10) {
  for (j in 1:10) {
    if (cor_dist_fun(site_numbs[i], site_numbs[j])[1] != 0)
    {
     finally1[i,] = cor_dist_fun(site_numbs[i], site_numbs[j])[1]
     finally2[i,] = cor_dist_fun(site_numbs[i], site_numbs[j])[2]
    }
  }
}
##  



















  
