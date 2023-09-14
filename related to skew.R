library(moments)

ols_musk=lm(data=df,wsk~lnl+sl)
X_musk=model.matrix(ols_musk)
sig= matrix(data = NA,nrow = 44,ncol = 44)
Sampling_Cov_matrix=as.matrix(Sampling_Cov_matrix)
sig=Sampling_Cov_matrix
y=df$wsk 
sig_inv= solve(sig)
X_musk_tr=solve(t(X_musk) %*% sig_inv%*% X_musk)
betahat=X_musk_tr%*%t(X_musk)%*%sig_inv%*%y
Residuals=y-X_musk%*%betahat

aqib_func<- function(stationA, stationB)
{ 
start=Record_Periods$FROM
end=Record_Periods$TO  
  inters<- intersect(stationA, stationB)
  crp <- length(inters)
  x_ob_A=length(setdiff(stationA,inters))
  x_ob_B=length(setdiff(stationB,inters))
  denom=((crp+x_ob_A)*(crp+x_ob_B))^0.5
  sk_corr=(crp/denom)*(0.26)^2.9
  return(sk_corr)
  }
mmr=matrix(data = NA,44,44)
for(i in 1:nrow(Record_Periods)){
  for (j in 1:nrow(Record_Periods)){
    mmr[i,j]= aqib_func(start[i]:end[i],start[j]:end[j])
  }
}