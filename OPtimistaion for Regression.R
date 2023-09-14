Data = read.csv("d:/R Programming/Reis Et Al Data/Regression Data.csv")
aaqib_ols = lm(data=Data,Aaqib_skew ~.)
write.csv(round(aaqib_ols$coefficients,digits=3),"ols.csv")
#optimization for ries et al.(WLS)
coeffs = matrix(aaqib_ols$coefficients, nrow = 15, ncol = 1)
x_wls = c(0, coeffs)
idm = diag(1, nrow = 92, ncol = 92)
X = model.matrix(aaqib_ols)
MLE_wls = function(x) {
  error_variance = x[1]
  parameters = matrix(x[2:16], nrow = 15, ncol = 1)
  Result =
    0.5 * log(det(error_variance * idm + var_mat), base = exp(1)) +
    0.5 * t((Reis_Et_Al_2020_Skew$skew - X %*% parameters)) %*% solve((error_variance *
                                                                         idm + var_mat)) %*%
    (Reis_Et_Al_2020_Skew$skew - X %*% parameters)
  return(Result)
}
wls= optim(par = x_wls, MLE_wls, method = "Nelder-Mead")
write.csv(round(wls$par,digits=3),"wls.csv")


#GLS
coeffs = matrix(aaqib_ols$coefficients, nrow = 15, ncol = 1)
x_gls = c(0, coeffs)
idm = diag(1, nrow = 92, ncol = 92)
X = model.matrix(aaqib_ols)
MLE_gls = function(x) {
  error_variance = x[1]
  parameters = matrix(x[2:16], nrow = 15, ncol = 1)
  Result =
    0.5 * log(det(error_variance * idm + cov_mat_pied), base = exp(1)) +
    0.5 * t((Reis_Et_Al_2020_Skew$skew - X %*% parameters)) %*% solve((error_variance *
                                                                         idm + cov_mat_pied)) %*%
    (Reis_Et_Al_2020_Skew$skew - X %*% parameters)
  return(Result)
}
gls = optim(par = x_gls, MLE_gls, method = "Nelder-Mead")
write.csv(round(gls$par,digits=3),"gls.csv")