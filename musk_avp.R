ols_musk = lm(data = musk, Skewness ~ .)
ols_0m = lm(Skewness ~ 1, data = musk)
mev_0m = (t(residuals(ols_0m)) %*% residuals(ols_0m)) / 43
mev_km = (t(residuals(ols_musk)) %*% residuals(ols_musk)) / 41  #41 = dof i,e 44-(2+1)
adj_R2 = 1 - mev_km / mev_0m
regression_musk = musk[, -1]
regression_musk = as.data.frame(regression_musk)
regression_musk = as.matrix(regression_musk)
X = matrix(data = regression_musk, nrow = 44, ncol = 2)
n = length(residuals(ols_musk))
Sigma = diag(n)
Sigma = 0.2792544 * Sigma
XTX_inv = solve(t(X) %*% solve(Sigma) %*% X)
samp_var = matrix(data = NA, nrow = 44, ncol = 1)
e=diag(n)
for (i in 1:nrow(X)) {
  samp_var[i] = X[i,] %*% XTX_inv %*% t(t(X[i,]))
}
avp_old = 0.2792544 + mean(samp_var[i, ] - 2 %*% 0.2792544 * X[i,] %*% XTX_inv %*% t(X) %*% e[, 1])

avg_Samp_Var = mean(samp_var)
avp_new = avg_Samp_Var + 0.2792544
