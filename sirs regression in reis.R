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
mev_matrix = matrix(data = NA, nrow = 91, ncol = 1)

for (i in 1:nrow(regression_data)) {
  mev_matrix[i] = (sigma(lm(data = regression_data[-i,], skew ~ .)))^2
  mev_matrix=as.matrix(mev_matrix)
  }
#.........................................................

Sigma=diag(92)
Sigma_2= mev_matrix[i] * Sigma


  
 
for (i in 1:nrow(X)) {
  samp_var[i] = X[i,] %*% solve(t(X) %*% solve(mev_matrix[i] * Sigma) %*% X)  %*% t(t(X[i,]))
  
}



XTX_inv[i] = solve(t(X) %*% solve(mev_matrix[i] * Sigma) %*% X)

samp_var = matrix(data = NA, nrow = 92, ncol = 1)

e = diag(n)

for (i in 1:nrow(X)) {
  samp_var[i] = X[i, ] %*% XTX_inv %*% t(t(X[i, ]))
}

