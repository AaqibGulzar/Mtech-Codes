compute_cost = function(x, y, w, b) {
  m = length(x)
  f.x = numeric()
  cost = numeric()
  for (i in 1:length(x)) {
    f.x[i] = w*x[i] + b
    cost[i] = (f.x[i] - y[i]) ^ 2
    J = (0.5 * m) * sum(cost)
  }
  
  total_cost = J
  
  
  return (total_cost)
}
x = runif(100,1,20)
y = runif(100,2,30)
compute_cost(x,y,2,1)

compute_gradient = function(x, y, w, b) {
  m = length(x)
  f.x = numeric()
  djw = numeric()
  djb = numeric()
  for (i in 1:length(x)) {
    f.x[i] = w * x[i] + b
    djw[i] = f.x[i] - y[i]
    djb[i] =  (f.x[i] - y[i]) * x[i]
  }
    dj_dw = sum(djw) / m
    dj_db = sum(djb) / m
    return(c(dj_dw, dj_db))
  }
compute_gradient(X_train,y_train,w_init,b_init) 


X_train <- matrix(c(2104, 5, 1, 45, 1416, 3, 2, 40, 852, 2, 1, 35), nrow = 3, byrow = TRUE)
y_train <- c(460, 232, 178)
b_init <- 785.1811367994083
w_init <- c(0.39133535, 18.75376741, -53.36032453, -26.42131618)


compute_gradient <- function(X, y, w, b) {
  # Computes the gradient for linear regression 
  # Args:
  #   X (matrix): Data, m examples with n features
  #   y (vector): target values
  #   w (vector): model parameters
  #   b (scalar): model parameter
  # Returns:
  #   dj_dw (vector): The gradient of the cost w.r.t. the parameters w.
  #   dj_db (scalar): The gradient of the cost w.r.t. the parameter b.
  
  m <- nrow(X)
  n <- ncol(X)
  
  dj_dw <- rep(0, n)
  dj_db <- 0
  
  for (i in 1:m) {
    err <- (t(X[i, ]) %*% w) + b - y[i]
    for (j in 1:n) {
      dj_dw[j] <- dj_dw[j] + err * X[i, j]
    }
    dj_db <- dj_db + err
  }
  
  dj_dw <- dj_dw / m
  dj_db <- dj_db / m
  
  return(list("dj_db" = dj_db, "dj_dw" = dj_dw))
}



 

gradient_descent <- function(X, y, w_in, b_in, cost_function, gradient_function, alpha, num_iters) {
  # An array to store cost J and w's at each iteration primarily for graphing later
  J_history <- numeric()
  w <- w_in
  b <- b_in
  
  for (i in 1:num_iters) {
    # Calculate the gradient and update the parameters
    grad <- gradient_function(X, y, w, b)
    dj_dw <- grad[[2]]
    dj_db <- grad[[1]]
    
    # Update Parameters using w, b, alpha and gradient
    w <- w - alpha * dj_dw
    b <- b - alpha * dj_db
    
    # Save cost J at each iteration
    J_history[i] <- cost_function(X, y, w, b)
    
    # Print cost every at intervals 10 times or as many iterations if < 10
    if (i %% max(10, round(num_iters/10)) == 0) {
      cat(sprintf("Iteration %4d: Cost %8.2f   \n", i, J_history[i]))
    }
  }
  
  return(list(w=w, b=b, J_history=J_history))
}

# initialize parameters
initial_w <- rep(0, length(w_init))
initial_b <- 0
# some gradient descent settings
iterations <- 1000
alpha <- 5.0e-7
# run gradient descent 
result <- gradient_descent(X_train, y_train, initial_w, initial_b,
                           compute_cost, compute_gradient, 
                           alpha, iterations)
w_final <- result$w
b_final <- result$b
J_hist <- result$J_history
cat(sprintf("b, w found by gradient descent: %.2f, %s\n", b_final, toString(w_final)))

m <- nrow(X_train)
for (i in 1:m) {
  cat(sprintf("prediction: %.2f, target value: %d\n", 
              crossprod(X_train[i,], w_final) + b_final, y_train[i]))
}

# plot cost versus iteration
par(mfrow=c(1,2),mar=c(5,5,2,2),oma=c(0,0,2,0))
plot(J_hist,type="l", main="Cost vs. iteration", ylab="Cost", xlab="iteration step")
plot(J_hist[100:length(J_hist)],type="l", main="Cost vs. iteration (tail)", ylab="Cost", xlab="iteration step")
