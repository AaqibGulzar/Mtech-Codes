X=cbind(1,Muskvar$lnl,Muskvar$Sl)   #create X with 1s as first column
Y=Muskvar$Wsk                      #create Y
beta=solve(t(X)%*%X,)              #find inverse              
beta_hat=beta%*%t(X)%*%Y           #create beta_hat
resid=Y-X%*%beta_hat              #find residuals
e=matrix(resid(ols),nrow = 44)    #residual matrix
et=t(e)
sigma_squared=as.vector((et%*%e)/(nrow(X)-ncol(X)))        #unbiased estimator of sigma^2,,denominator is...
..................................# n - (k+1) where k is deg of freedom.
varbeta_hat<-sigma_squared*solve(t(X)%*%X)  #estimator of variance of beta,
#a matrix whose diagonals are est. variance,whose 
# sqaure root will give std.Error for each coefficient.
sqrt(diag(varbeta_hat))
#same results can be found by simply applying lm() function.


#steps for robust heteroscedasticity analysis
#beta hat and residuals will be same as for standard ols
#for variance of beta hat,robust HC1  estimator is used as below
beta=solve(t(X)%*%X)
A=solve(t(X)%*%X)
R=diag(c(resid^2))
var_bet_rob=(44/41)*beta%*%t(X)%*%R%*%X%*%A
SEStandard<-sqrt(diag(varbeta_hat))  # from previos code
SERobust=sqrt(diag(var_bet_rob))
outmat=cbind(beta_hat,SEStandard,SERobust)
colnames(outmat)=c("Beta Hat","SEStandard","SERobust")
#same robust estimation can be done by using library(estimatr),function lm_robust
#HC1=lm_robust(Wsk~lnl+Sl,data=Muskvar,se_type="HC1"),HC2=lm_robust(Wsk~lnl+Sl,data=Muskvar,se_type="HC2"),HC3=lm_robust(Wsk~lnl+Sl,data=Muskvar,se_type="HC3")
