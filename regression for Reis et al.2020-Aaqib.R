library(tidyverse)

Data = read.csv("d:/R Programming/Reis Et Al Data/Regression Data.csv")
aaqib_ols = lm(data = Data, Aaqib_skew ~ .)
aaqib_ols_0 = lm(Data,Aaqib_skew ~ 1)

y = regression_data$skew

library(WREG)

X = model.matrix(aaqib_ols)
aaq.wre = WREG.OLS(y,X,transY="none")

#Remember !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
aaqib_ols$residuals %>% var() = t(aaqib_ols$residuals)%*%aaqib_ols$residuals/91

mev_0 = (t(residuals(aaqib_ols_0)) %*% residuals(aaqib_ols_0)) / 91

mev_k = (t(residuals(aaqib_ols)) %*% residuals(aaqib_ols)) / 77  #79 = dof i,e 92-(14+1)

adj_R2 = 1 - mev_k / mev_0

PseudoR2(glm_reis, which = "all").........#for glm models.


n = length(residuals(aaqib_ols))

Sigma = diag(n)

Sigma_2 = 0.2202366 * Sigma

ln.X = log10(X)

XTX_inv = solve(t(X) %*% solve(Sigma_2) %*% X)

samp_var = matrix(data = NA, nrow = 92, ncol = 1)


for (i in 1:nrow(X)) {
  samp_var[i] = X[i, ] %*% XTX_inv %*% X[i,]
}

avg_Samp_Var = mean(samp_var)
avp_new_ols = avg_Samp_Var + mev_k

avp_old = mev_k + mean(samp_var[i,] - 2 %*% mev_k * X[i, ] %*% XTX_inv %*% t(X) %*% e[, i])

#--------------------------------------------------------------------------------

#getting the lat_long of multiple sites using dataretrieval,which takes site numbers as
#character strings and not numbers.


#FOR WLS REGRESSION METHOD.
Sigma_wls = diag(n)

Sigma_2_wls = 0.082 * Sigma_wls + var_mat

XTX_inv_wls = solve(t(X) %*% solve(Sigma_2_wls) %*% X)

samp_var_wls = matrix(data = NA, nrow = 92, ncol = 1)

e = diag(n)

for (i in 1:nrow(X)) {
  samp_var_wls[i] = X[i, ] %*% XTX_inv_wls %*% X[i, ]
}


avg_Samp_Var_wls = mean(samp_var_wls)

avp_new_wls = avg_Samp_Var_wls + 0.082


#for GLS method
Sigma_gls = diag(n)

Sigma_2_gls =0.079*Sigma_gls +  cov_mat_pied

XTX_inv_gls = solve(t(X) %*% solve(Sigma_2_gls) %*% X)

samp_var_gls = matrix(data = NA, nrow = 92, ncol = 1)

e = diag(n)

for (i in 1:nrow(X)) {
  samp_var_gls[i] = X[i, ] %*% XTX_inv_gls %*% X[i, ]
}


avg_Samp_Var_gls = mean(samp_var_gls)

avp_new_gls = avg_Samp_Var_gls + 0.082


#Constructing the CI for each Station's Skewness value.

k_upper = skews$skews[1] + 1.96*(samp_var[1]^0.5)
k_lower = skews$skews[1] - 1.96*(samp_var[1]^0.5)
























