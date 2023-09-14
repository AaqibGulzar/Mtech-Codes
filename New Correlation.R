
library(lubridate)
library(tidyverse)

con_correlation = function(siteA, siteB) {
  
  siteA = filter(site_peaks , site_no == siteA)
  siteA = siteA[, c(3, 5)] %>% na.omit()
  siteA$cal_year = year(ymd(siteA$peak_dt))
  siteA$month = month(ymd(siteA$peak_dt))
  siteA$wat_year = ifelse(siteA$month <= 9, siteA$cal_year, siteA$cal_year + 1)
  
  
  siteB = filter(site_peaks , site_no == siteB)
  siteB = siteB[, c(3, 5)] %>% na.omit()
  siteB$cal_year = year(ymd(siteB$peak_dt))
  siteB$month = month(ymd(siteB$peak_dt))
  siteB$wat_year = ifelse(siteB$month <= 9, siteB$cal_year, siteB$cal_year + 1)
  
  inters = intersect(siteA$wat_year, siteB$wat_year)
  
  if(length(inters) >= 40 ) {
    siteA = siteA %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    siteB = siteB %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    correlation = ifelse(length(siteA$peak_va) == length(siteB$peak_va),cor(log10(siteA$peak_va), log10(siteB$peak_v)),0)
    return(correlation) } else { 0 }
}








library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
 
cl = makeCluster(10)
registerDoParallel(cl)
clusterCall(cl,function()c(library(dplyr),library(lubridate)))



new_cor_mat =  foreach(i = 1:92,combine = cbind) %:%
  foreach(j = 1:92) %dopar% {
    con_correlation(site_numbs[i], site_numbs[j])
  } %>% unlist() %>% matrix(nrow=92,ncol=92)

stopCluster(cl)
d_ij = 0.000621*d_ij
cor_vec = as.vector(new_cor_mat[upper.tri(new_cor_mat,diag=T)])
dist_vec = as.vector( d_ij[upper.tri(d_ij,diag=T)])
Z = 2.71^(0.89 - 0.18*((d_ij^0.29 - 1)/0.29))
Z_vec = as.vector(Z[upper.tri(Z,diag=T)])
r = (exp(2*Z_vec) - 1)/(exp(2*Z_vec) +1)

df= data.frame(dist_vec,cor_vec,Z_vec,r)
df = df %>% na.omit()
colnames(df) = c("dist","correlation","model","r")
df = rename(df,distance = `dist`,correlation = correlation,Z_model = model,r = r)
df2 = df %>% filter( correlation >0 )
plot(df2$distance,df2$correlation)
abline(lm(df2$correlation~df2$distance))


library(plotly)

p1 = plot_ly(df2,
        x = ~ distance,
        y = ~r,
        type = "scatter",
        mode = "markers",
        name = "Model") 

p1 = p1 %>% add_trace(df2,y = ~ correlation,name = "Site pairs")  
mytheme = theme(plot.title=element_text(family="sans",face="bold",size=15))
ggp = ggplot(df2,
       aes(x = distance,
           y = correlation)) +  geom_point(color = "darkblue",alpha = 0.6) + theme_grey() +
  ggtitle("Correlation of Concurrent Annual Maximum Peaks against the distance between the stations") + 
  mytheme + geom_point(aes(x = distance,y = r),color = "violet")
ggplotly(ggp)



neww = matrix(data=NA,nrow=92,ncol=92)
for(i in 1){
  for (j in 1:92){
    print(con_correlation(site_numbs[i],site_numbs[j]))
  }
}

