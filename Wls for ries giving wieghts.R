suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
data(BostonHousing)
str(BostonHousing)

X = model.matrix(ols_reis)
W = diag(1/diag(var_mat))
wls_ries_k = lm(formula = skew ~ ., data = regression_data, weights = diag(W))
beta_wls = solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%Reis_Et_Al_2020_Skew$skew
mev_wls_k = (t(wls_ries_k$residuals) %*% solve(W) %*% wls_ries_k$residuals) / 77
wls_ries_0 = lm(formula = skew ~ 1, data = regression_data, weights = diag(W))
mev_wls_0 = (t(wls_ries_0$residuals) %*% solve(W) %*% wls_ries_0$residuals) / 77
psuedo_R = 1 - mev_wls_k/mev_wls_0
samp_var = matrix(data = NA, nrow = 92, ncol = 1)
n = 92
e = diag(n)
A = diag(0.053,92,92) + var_mat
XTX_inv = solve(t(X) %*% solve(A) %*% X)
for (i in 1:92) {
  samp_var[i] = X[i,] %*% XTX_inv %*% t(t(X[i,]))
}

avg_Samp_Var = mean(samp_var)

avp_new = avg_Samp_Var + mev_wls




