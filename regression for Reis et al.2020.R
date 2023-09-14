library(tidyverse)
library(readr)
Reis_Et_Al_2020_Skew <-
  read_csv(
    "https://ecommons.cornell.edu/bitstream/handle/1813/56521.2/Reis_Et_Al_2020_Skew.txt?sequence=29&isAllowed=y",
    col_names = FALSE
  )
View(Reis_Et_Al_2020_Skew)

variables = read.csv("c:/Users/Hp/Desktop/R Programming/ecornell/reis_etal.csv")

Reis_Et_Al_2020_Skew = as.data.frame(Reis_Et_Al_2020_Skew)

colnames(Reis_Et_Al_2020_Skew) = "skew"

regression_data = variables[3:14]

regression_data$skew = Reis_Et_Al_2020_Skew$skew

regression_data = regression_data %>% select(skew, everything())

ols_reis = lm(data = regression_data, skew ~ .)

ols_0 = lm(skew ~ 1, data = regression_data)

mev_0 = (t(residuals(ols_0)) %*% residuals(ols_0)) / 91

mev_k = (t(residuals(ols_reis)) %*% residuals(ols_reis)) / 79  #79 = dof i,e 92-(12+1)

adj_R2 = 1 - mev_k / mev_0

PseudoR2(glm_reis, which = "all").........#for glm models.

regression_data2 = regression_data[, -1]

regression_data2 = as.data.frame(regression_data2)

regression_data2 = as.matrix(regression_data2)

X = matrix(data = regression_data2, nrow = 92, ncol = 12)

y = regression_data$skew

n = length(residuals(ols_reis))

Sigma = diag(n)

Sigma_2 = 0.2792544 * Sigma

XTX_inv = solve(t(X) %*% solve(Sigma_2) %*% X)

samp_var = matrix(data = NA, nrow = 92, ncol = 1)

e = diag(n)

for (i in 1:nrow(X)) {
  samp_var[i] = X[i, ] %*% XTX_inv %*% t(t(X[i, ]))
}

avp_old = mev_k + mean(samp_var[i,] - 2 %*% mev_k * X[i, ] %*% XTX_inv %*% t(X) %*% e[, i])

avg_Samp_Var = mean(samp_var)

avp_new = avg_Samp_Var + mev_k
#--------------------------------------------------------------------------------

#getting the lat_long of multiple sites using dataretrieval,which takes site numbers as
#character strings and not numbers.

