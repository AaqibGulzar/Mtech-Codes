library(tidyverse)
library(dataRetrieval)
library(geosphere)
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

data = readNWISpeak(site_numbs,endDate="2021-12-31")
data_coords <- readNWISsite(site_numbs)

d_ij=matrix(data = NA,nrow = 92,ncol = 92)
for(i in 1:92){
  for (j in 1:92){
    d_ij[i,j]= distm(
      c(data_coords$dec_long_va[i], data_coords$dec_lat_va[i]),
      c(data_coords$dec_long_va[j], data_coords$dec_lat_va[j])
    )
  }
}

d_ij = d_ij *  0.000621

con_correlation = function(siteA, siteB) {
  
  siteA = filter(data , site_no == siteA)
  siteA = siteA[, c(3, 5)] %>% na.omit()
  siteA$cal_year = year(ymd(siteA$peak_dt))
  siteA$month = month(ymd(siteA$peak_dt))
  siteA$wat_year = ifelse(siteA$month <= 9, siteA$cal_year, siteA$cal_year + 1)
  
  
  siteB = filter(data , site_no == siteB)
  siteB = siteB[, c(3, 5)] %>% na.omit()
  siteB$cal_year = year(ymd(siteB$peak_dt))
  siteB$month = month(ymd(siteB$peak_dt))
  siteB$wat_year = ifelse(siteB$month <= 9, siteB$cal_year, siteB$cal_year + 1)
  
  inters = intersect(siteA$wat_year, siteB$wat_year)
  
  if(length(inters) >= 50) {
    siteA = siteA %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    siteB = siteB %>% filter(between(wat_year, inters[1], inters[length(inters)]))
    correlation = ifelse(length(siteA$peak_va) == length(siteB$peak_va),cor(log10(siteA$peak_va), log10(siteB$peak_va)),0)
    return(correlation) } else { 0 }
}


con_correlation(site_numbs[1],site_numbs[3])




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





cf_ij <- function(siteA, siteB) {
  siteA = filter(data , site_no == siteA)
  yearsA = siteA[, 3] %>% na.omit() %>% format(format = "%Y")
  siteB = filter(data , site_no == siteB)
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

cf_mat =
  foreach(i = 1:92,combine = cbind) %:%
  foreach (j = 1:92)  %dopar% {
    cf_ij(site_numbs[i], site_numbs[j]) 
  } %>% unlist() %>% matrix(nrow = 92, ncol = 92)

# user  system elapsed 
# 2.03    0.21   20.46 less than 10s

stopCluster(cl)

#Now making the smoothed correlation for the Piedmont.
rho_dij <- matrix(data = NA,nrow = 92,ncol = 92)

for(i in 1:92) {
  for (j in 1:92) {
    rho_dij[i, j] =
      0.993 ^ ((2.78 * d_ij[i, j]) / (0.00989 * d_ij[i, j] + 1))  #As per the paper.
  }
}


# Finally we calculate the inter site correlation coefficient between two at site skewness
# estimators in terms of the inter site correlation coefficient.

rho_cap = matrix(data = NA,ncol = 92,nrow = 92)
for(i in 1:92) {
  for (j in 1:92) {
    rho_cap[i,j] = (cf_mat[i, j]) * (rho_dij[i, j] ^ (3))  # here we try k=3
  }
}

cor_vec = as.vector(new_cor_mat[upper.tri(new_cor_mat,diag=T)])
dist_vec = as.vector( d_ij[upper.tri(d_ij,diag=T)])

correl = data.frame(cor_vec,dist_vec)
correl = correl[cor_vec > 0 & cor_vec < 1, ]
rownames(correl) = NULL

ggplot() +
  geom_point(aes(x = d_ij, y = rho_dij)) +
               geom_point(data=correl,aes(x= dist_vec,y = cor_vec))



# create the plot with two different data sets
ggplot() +
  geom_point(aes(x = d_ij, y = rho_dij, color = "Model Pairs"),size = 2) +
  geom_point(data = correl, aes(x = dist_vec, y = cor_vec, color = "Site Pairs")) +
  
  # set contrasting colors for the two data sets
  scale_color_manual(name = "",values = c("Model Pairs" = "darkred", "Site Pairs" = "dodgerblue")) +
  
  # add plot labels and legend titles
  labs(title = "Correlation vs Distance",
       x = "Distance (miles)",
       y = "Correlation",
       color = "Data Set") + theme_minimal() +
  theme(legend.text = element_text(size = 12),
                                                    legend.key.size = unit(1.5, "lines"),legend.position=c(0.85,0.85)) +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

ggsave("Correlogram.jpeg",dpi = 800,width=10,height=8)
