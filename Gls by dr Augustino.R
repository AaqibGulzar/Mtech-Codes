#GLS using faraway data set
#Method 1: manual calculations
library(faraway)
globwarm=na.omit(globwarm)
lmod=lm(nhtemp~wusa+jasper+westgreen,data=globwarm)
n=length(residuals(lmod))
cor(residuals(lmod)[-1],residuals(lmod)[-n])
X=model.matrix(lmod)
Sigma=diag(n)
Sigma=0.707^abs(row(Sigma)-col(Sigma))  #this sigma is omega in the paper for ols
y=globwarm$nhtemp                       # and Δ(δ²) for gls 
Sigma_inv=solve(Sigma)
XTX_inv=solve(t(X) %*% Sigma_inv%*%X)
betahat=XTX_inv%*%t(X)%*%Sigma_inv%*%y
Residuals=y-X%*%betahat7b
cor(Residuals[-1],Residuals[-n])
#Method 2:Cholesky decomposition

S=chol(Sigma)
S_inv=solve(t(S))
Sx=S_inv%*%X
Sy=S_inv%*%y
lm(Sy~Sx-1)           #same as betahat estimators


#Method 3:using the nlme package
library(nlme)
glmod=gls(nhtemp~wusa+jasper+westgreen,data=globwarm,correlation = corAR1(form = ~year))
summary(glmod)
intervals(glmod,which = "all")
