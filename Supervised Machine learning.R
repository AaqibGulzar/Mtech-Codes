

compute_cost_linear_reg <- function(X, y, w, b, lambda_ = 1) {
  # Computes the cost over all examples
  # Args:
  # X (matrix): Data, m examples with n features
  # y (vector): target values
  # w (vector): model parameters
  # b (scalar): model parameter
  # lambda_ (scalar): Controls amount of regularization
  # Returns:
  # total_cost (scalar): cost
  
  m <- nrow(X)
  n <- length(w)
  cost <- 0
  
  for (i in 1:m) {
    f_wb_i <- sum(X[i,] * w) + b
    cost <- cost + (f_wb_i - y[i])^2
  }
  
  cost <- cost / (2 * m)
  
  reg_cost <- sum(w^2)
  reg_cost <- (lambda_ / (2 * m)) * reg_cost
  
  total_cost <- cost + reg_cost
  
  return(total_cost)
}

set.seed(1)

X_tmp <- matrix(runif(5*6), nrow = 5, ncol = 6)
y_tmp <- c(0, 1, 0, 1, 0)
w_tmp <- runif(ncol(X_tmp)) - 0.5
b_tmp <- 0.5
lambda_tmp <- 0.7

cost_tmp <- compute_cost_linear_reg(X_tmp, y_tmp, w_tmp, b_tmp, lambda_tmp)

cat("Regularized cost:", cost_tmp, "\n")




sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

compute_cost_logistic_reg <- function(X, y, w, b, lambda_ = 1) {
  m <- nrow(X)
  n <- ncol(X)
  cost <- 0
  
  sigmoid <- function(z) {
    1 / (1 + exp(-z))
  }
  
  for (i in 1:m) {
    z_i <- sum(X[i,] * w) + b
    f_wb_i <- sigmoid(z_i)
    cost <- cost - y[i]*log(f_wb_i) - (1-y[i])*log(1-f_wb_i)
  }
  cost <- cost / m
  
  reg_cost <- sum(w^2)
  reg_cost <- (lambda_/(2*m)) * reg_cost
  
  total_cost <- cost + reg_cost
  return(total_cost)
}


set.seed(1)
X_tmp <- matrix(runif(5*6), nrow=5, ncol=6)
y_tmp <- c(0, 1, 0, 1, 0)
w_tmp <- runif(ncol(X_tmp)) - 0.5
b_tmp <- 0.5
lambda_tmp <- 0.7
cost_tmp <- compute_cost_logistic_reg(X_tmp, y_tmp, w_tmp, b_tmp, lambda_tmp)

cat("Regularized cost:", cost_tmp)


compute_gradient_linear_reg <- function(X, y, w, b, lambda_ = 1) {
  # X: m x n matrix (m examples with n features)
  # y: m x 1 vector (target values)
  # w: n x 1 vector (model parameters)
  # b: scalar (model parameter)
  # lambda_: scalar (controls amount of regularization)
  
  m <- nrow(X)
  n <- ncol(X)
  dj_dw <- matrix(0, nrow = n, ncol = 1)
  dj_db <- 0
  
  for (i in 1:m) {
    err <- (t(X[i,]) %*% w + b) - y[i]
    for (j in 1:n) {
      dj_dw[j] <- dj_dw[j] + err * X[i, j]
    }
    dj_db <- dj_db + err
  }
  
  dj_dw <- dj_dw / m
  dj_db <- dj_db / m
  
  for (j in 1:n) {
    dj_dw[j] <- dj_dw[j] + (lambda_ / m) * w[j]
  }
  
  return(list(dj_db = dj_db, dj_dw = dj_dw))
}

set.seed(1)
X_tmp <- matrix(runif(5*3), ncol = 3)
y_tmp <- c(0, 1, 0, 1, 0)
w_tmp <- runif(ncol(X_tmp))
b_tmp <- 0.5
lambda_tmp <- 0.7

grad <- compute_gradient_linear_reg(X_tmp, y_tmp, w_tmp, b_tmp, lambda_tmp)

cat(sprintf("dj_db: %f\n", grad[[1]]))
cat(sprintf("Regularized dj_dw:\n%s\n", toString(grad[[2]])))



fig = plt.figure(figsize = (12,8))
ax = fig.gca(projection = "3d")
surf = ax.plot_surface(theta_0,theta_1,cost_values,cmap = "viridis")
fig.colorbar(surf,shrink = 0.5,aspect = 5)
plt.xlabel("$\Theta_0$")
plt.ylabel("$\Theta_1$")
ax.set_zlabel("$J(\Theta)$")
ax.view_init(30,330)
plt.show()




compute_gradient_logistic_reg <- function(X, y, w, b, lambda_) {
  # Computes the gradient for logistic regression
  
  # Args:
  #   X (matrix): Data, m examples with n features
  #   y (vector): Target values
  #   w (vector): Model parameters
  #   b (scalar): Model parameter
  #   lambda_ (scalar): Controls amount of regularization
  
  # Returns:
  #   dj_dw (vector): The gradient of the cost w.r.t. the parameters w
  #   dj_db (scalar): The gradient of the cost w.r.t. the parameter b
  
  m <- dim(X)[1]
  n <- dim(X)[2]
  dj_dw <- rep(0, n)
  dj_db <- 0
  
  for (i in 1:m) {
    f_wb_i <- sigmoid(sum(X[i, ] * w) + b)
    err_i <- f_wb_i - y[i]
    
    for (j in 1:n) {
      dj_dw[j] <- dj_dw[j] + err_i * X[i, j]
    }
    dj_db <- dj_db + err_i
  }
  dj_dw <- dj_dw / m
  dj_db <- dj_db / m
  
  for (j in 1:n) {
    dj_dw[j] <- dj_dw[j] + (lambda_ / m) * w[j]
  }
  
  return(list(dj_db = dj_db, dj_dw = dj_dw))
}


set.seed(1)
X_tmp <- matrix(runif(15), nrow = 5, ncol = 3)
y_tmp <- c(0, 1, 0, 1, 0)
w_tmp <- runif(ncol(X_tmp))
b_tmp <- 0.5
lambda_tmp <- 0.7
out <- compute_gradient_logistic_reg(X_tmp, y_tmp, w_tmp, b_tmp, lambda_tmp)
dj_db_tmp <- out[1]
dj_dw_tmp <- out[2]

cat(paste0("dj_db: ", dj_db_tmp, "\n"))






















