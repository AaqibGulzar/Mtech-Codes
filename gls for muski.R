#Method 1: manual caculatiobs
lmod=lm(Fsk~csl+cl,data=Rmuskingum)
n=length(residuals(lmod))
phi=cor(residuals(lmod)[-1],residuals(lmod)[-n])
X=model.matrix(lmod)
Sigma=diag(n)
Sigma= phi^abs(row(Sigma)-col(Sigma))
y=Rmuskingum$Fsk
Sigma_inv=solve(Sigma)
XTX_inv=solve(t(X) %*% Sigma_inv%*%X)
betahat=XTX_inv%*%t(X)%*%Sigma_inv%*%y
Residuals=y-X%*%betahat
model_ev_gls=(t(Residuals)%*%Sigma_inv%*%Residuals)/41
cor(Residuals[-1],Residuals[-n])

#Method 2:cholesky decomposition

S=chol(Sigma)
S_inv=solve(t(S))
Sx=S_inv%*%X
Sy=S_inv%*%y
lm(Sy~Sx-1)           #same as betahat estimators


#Method 3:using the nlme package
library(nlme)
glmod=gls(Fsk~csl+cl,data=,correlation = corAR1())
summary(glmod)
intervals(glmod,which = "all")
