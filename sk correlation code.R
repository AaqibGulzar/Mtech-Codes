library(tidyverse)
colnames(Record_Periods)="RP"
Record_Periods=Record_Periods %>% separate(RP,c("FROM","TO"))
Record_Periods$FROM=as.numeric(Record_Periods$FROM)
Record_Periods$TO=as.numeric(Record_Periods$TO)
start=Record_Periods$FROM
end=Record_Periods$TO

aqib_func<- function(stationA, stationB)
{
  inters<- intersect(stationA, stationB)
  crp <- length(inters)
  x_ob_A=length(setdiff(stationA,inters))
  x_ob_B=length(setdiff(stationB,inters))
  denom=((crp+x_ob_A)*(crp+x_ob_B))^0.5
  sk_corr=(crp/denom)*((0.26)^2.9)
  return(sk_corr)
}
                                        #station=start[i]:end[i],start=from,end=to
                                        
mmr=matrix(data=NA,44,44)               #create an empty matrix and loop the function over it
for(i in 1:nrow(Record_Periods)){
  for (j in 1:nrow(Record_Periods)){
    mmr[i,j]= aqib_func(start[i]:end[i],start[j]:end[j])
  }
}
