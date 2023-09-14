# # # Introduction to Optimization in R.
 
# THE PROBLEM 
# A company wants to maximise the profits for two products A and B which are sold at $25 and $20.
# There are 1800 resource units available every day and the product A requires 20 units while the
# product B requires 12 units. Both of these require a production time of 4 minutes and total available
# working hours are 8 in a day.
# What should be the production quantity for each of the products to maximise profits?

# The objective function in the above problem will be:
#     max(sales) = max(25*x1 + 20*x2)
#     where x1 is the units of A produced.
#     x2 is the units of B produced.
# Both x1 and x2 are called as "Decision Variables"    


#   The constraints (Time and Resources) in the problem are:
#   20*x1 + 12*x2 <= 1800 (resource constraint) >>>>>>> 20 units are required by A and 12 units by B.
#   4*x1 + 4*x2 <= 8*60(time constraint) >>>>>>>>>>>>>> 4-minutes reqiuired by each.
   
library(lpSolve)  
##Set the coefficients of the decision variable
  
  objective.in = c(25,20)

## Create a constraint matrix
  
  const.mat = matrix(c(20,12,4,4),nrow=2,byrow=T)
  
## Define the Constraints
  
  time.const = 8*60
  resou_const = 1800
  
# RHS for constraints
  
  const.rhs = c(resou_const,time.const)
  
#Constraint direction
  
  const.dir = c("<=","<=")
  
#FIND THE OPTIMAL SOLUTION
  optimum = lp(direction="max",objective.in,const.mat,const.dir,const.rhs)
  
#    > optimum$solution
#   [1] 45 75
#    > optimum$objval
#   [1] 2625 
  
  
  ## SO FROM THE ABOVE WE CONCLUDE THAT:
# The company shoud produce 45 units of A and 75 units of B to get the max sales of $2625
# given the constraints.
  

   
###EXAMPLE 2
  
  my_fun = function(x){     # create a mathematical function which depends on x.
    x^3 + x^2 -10*x
  }

  plot(my_fun,xlim=c(-1,3),type = "o",col = "red") # plot to visualise.
  
#As we can see the function has its  minimum value in between 1 and 2 somewhere within xlim = c(-1,3)
  
  optimise(my_fun,interval=c(-1,3))

#   $minimum
#    1.522606
   
#   $objective
#    -9.377829 
   
  

#EXAMPLE 3
  ##Poisson Log-likelihood   
 
#We will create a loglikelihood for the poisson didtribution which is mathematically written as:(check google)
      
library(knitr)
  poisson.like = function(mu,y){
    n = nrow(y)
    logl = sum(y)*log10(mu) - n*mu
    return(-logl)
  }
 
#Now we will invoke the use of the optim function. For that,we need starting values,log-likelihood and data.
#Method can be "CG" or "BFGS". Check help for more.
  
##DATA Simulation,Log-likeliho0d for a linear model.
 
   
  X = cbind(1,runif(100)) # a 100x2 matrix,1 for the intercept.
  theta = c(2,3,1) # estimator coefficients
  y = X%*%theta[1:2] + rnorm(100) # linear model
  
ols.lf = function(theta,y,X){
  n = nrow(X)
  k = ncol(X)
  bet = theta[1:k]
  sigma2 = theta[k+1]
  errors = y - X%*%bet
  logl = 0.5*n*log10(2*pi) - 0.5*n*log10(sigma2) - ((t(errors)%*%errors)/(2*sigma2))
  return(-logl)
  }  

p = optim(c(1,1,1),ols.lf,method="BFGS",hessian=T,y=y,X=X)  
  
  
#EXample 4
library(mosaic)
f = makeFun(x^2 + y ~ x&y) #obj fun
g = makeFun(x^2 - y^2 ~ x&y)  #constr fun,=1
plotFun(f(x,y)~x&y,xlim=range(-2,2),ylim=range(-2,2),filled=F)   
plotFun(g(x,y)~x&y,levels=1, xlim=range(-2,2),ylim=range(-2,2),filled=FALSE,add=TRUE,col="blue")   
plotFun(f(x,y)~x&y,xlim=range(-2,2),ylim=range(-2,2),filled=F,add=T,levels=0.8)   
#now u can see where the two functions are minimum,at x = 0.7 or y = -0.6 with obj.fun =0.8



 #Example 5

#the profit maximization problem is?:
# 10x -x^2 - 20
# where x is the quantity prosuced by the firm.

guess = c(1) #step 1,take an initial guess in the feasible region.

profit  = function(x){
  return(10*x -x^2 - 20)    #define the function
}

optim(guess,
      profit, 
      control=list(fnscale = -1),    #maximise and not minimise 
      lower=0,                       #excludes negative numbers by setting a lower bound.
      method="L-BFGS-B")             #method used  


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
####Example 6

library(lpSolve) 
#Our business problem is like this
#Max.Z = 20x1 + 60x2 #profit of two products per each unit.
objective.in = c(20,60) # our obj fun based on the profit of each unit.
time.const.work = 2700 #max working hours.(30 and 20 for each product)
time.const.mac = 850   # max machine availability hours. (5 and 10 for each product)
prod.const = 95  # combined minimum number of units to be sold.(x1 + x2 = 95)
const.mat = matrix(c(30,20,5,10,1,1),nrow=3,byrow=T)
const.rhs = c(time.const.work,time.const.mac,prod.const)
const.dir = c("<=","<=",">=")  #not exceeding work time and machine time but not going below production = 95.
#FIND THE OPTIMAL SOLUTION
optimum = lp(direction="max",objective.in,const.mat,const.dir,const.rhs)
print(c(Value = optimum$objval,  Units = optimum$solution))


ok = function(x){
  x^3 + x^2 - 10*x
}

optimise(ok ,interval=c(-4,4))
         

plot(ok,x.lim=range(-4,4),col = "red")


#><><>><><><><><><><><><><><><><><><><><><><><><><><><><><>><><><><><><><><>><><><><><><><>
#?/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\\/\//\
f <- function(x,y) {
  2*x*(y^2)+(2*(x^2)*y) + x*y   #Create a function with 2 variables.
}

x<- seq(-0.5,0.5, len=400)  #Define x variable
y<- seq(-0.5,0.5, len=400)  #Define y variable
z <- outer(x,y,f)           #define the variable z= 𝒇(𝒙,𝒚) which corresponds
                            #to the values of the function for each pair of
                            #𝒙 and 𝒚 that satisfies 𝒇(𝒙,𝒚).
#So basically we created a matrix with 200x200 values. 
str(z)

persp(x,y,z,ticktype="detailed",phi=15,theta=-30)
image(x,y,z)

fbb<-function(x) {
  f(x[1],x[2])
} 
optim(c(0.5,0.5),fbb,control=list(fnscale=-1))

f2 = function(x,y){
  x^2 - y
}
x = seq(-1,1,len = 10)
y = seq(-1,1,len = 10)
z = outer(x,y,f2)
persp(x,y,z,ticktype="detailed",phi = 15,theta=-30)
fbb = function(x){
  f2(x[1],x[2])
}


# Example 7
library(stats)
f1  = function(x) {
  #x is a vector of length 3.
  F = matrix(c(x[1] + x[2] - x[3],
               x[1] ^ 2 + x[2] ^ 2 - x[3] ^ 2,
               x[1] ^ 3 + x[2] ^ 3 - x[3] ^ 3))
  return(sum(F ^ 2))
}

#Initial values for the parameters.This is just a guess.
x0 = c(1,1,1)

#optimize the equation using nelder mead method
result = optim(x0,f1,method="Nelder-Mead")

#optimized parameter values.
result$par

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#QUESTION
#How we can run the above optimization in a loop until we get an
#optimal solution for a particular set of parameters? Can we do that?


#ANSWER
#Yes, you can use a loop to run the optimization multiple
# times until you obtain an optimal solution for a particular set of parameters.
# This can be useful if you want to ensure that the optimization has converged to
# the optimal solution, or if you want to perform the optimization multiple times 
# with different initial parameter values.
# 
# To run the optimization in a loop, you can use a while loop or a for loop in R.
# The loop should continue until some condition is met, such as the optimization
# reaching a certain level of precision or the number of iterations reaching a 
# maximum value. Inside the loop, you can call the optim function to perform the
# optimization, and check the result to determine whether the condition for ending 
# the loop has been met.
# Here is an example of using a while loop to run the optimization in a loop:

# Define the function to be optimized
myfunc <- function(x) {
  x^3 + x^2 -10*x
}

# Specify the parameters to be optimized
params <- c(1, 2, 3)

# Set the maximum number of iterations
max_iterations <- 100

# Set the precision level for the optimization
precision <- 1e-6

# Initialize the iteration counter
iteration <- 0

# Run the optimization in a loop
while (iteration < max_iterations) {
  # Use the Nelder-Mead method for optimization
 result <- optim(par = 4, fn = myfunc, method = "Nelder-Mead")
  
  # Check if the optimization has reached the desired precision level
  if (abs(result$value - myfunc(result$par)) < precision) {
    # If the precision level has been reached, break out of the loop
    break
  }
  
  # Update the iteration counter
  iteration <- iteration + 1
}

# Print the optimized parameters
print(result$par)
# In this example, the while loop continues until the number of iterations reaches the maximum value of 100 or the optimization reaches the desired precision level of 1e-6. Inside the loop, the optim function is called to perform the optimization, and the result is checked to see if the precision level has been reached. If the precision level has been reached, the break keyword is used to exit the loop.
# 
# Alternatively, you could use a for loop instead of a while loop. For example:
# Define the function to be optimized
myfunc <- function(x) {
  # The function returns the sum of the squares of the parameters
  return(sum(x^2))
}

# Specify the parameters to be optimized
params <- c(1, 2, 3)

# Set the maximum number of iterations
max_iterations <- 100

# Set the precision level for the optimization
precision <- 1e-6

# Run the optimization in a loop
for (iteration in 1:max_iterations) {
  # Use the Nelder-Mead method for optimization
  result <- optim(par = params, fn = myfunc, method = "Nelder-Mead")
  
  # Check if the optimization has reached the desired precision level
  if (abs(result$value - myfunc(result$par)) < precision) {
    # If the precision  level has been reached, break out of the loop
    break
  }
}  



trig  = function(x) {
  #x is a vector of length n
  F = matrix(c(sin(x) + cos(x),
               sin(x) ^ 2 + cos(x) ^ 2 ,
               sin(x) ^ 3 + cos(x) ^ 3 ))
  return(sum(F^2))
  #the matrix F will be a matrix with nrow = 3*n and ncol = 1
}

#Initial values for the parameters.This is just a guess.
x0 = c(1,1,1)

#optimize the equation using nelder mead method
result = optim(x0,trig,method="Nelder-Mead")



aaqib_ols = lm(data=regression_data.3,Aaqib_skew ~.)
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

#GLS using Ries covariance matrix
coeffs = matrix(aaqib_ols$coefficients, nrow = 15, ncol = 1)
x_ries = c(0, coeffs)
idm = diag(1, nrow = 92, ncol = 92)
X = model.matrix(aaqib_ols)
MLE_ries = function(x) {
  error_variance = x[1]
  parameters = matrix(x[2:16], nrow = 15, ncol = 1)
  Result =
    0.5 * log(det(error_variance * idm + cov_mat_ries), base = exp(1)) +
    0.5 * t((Reis_Et_Al_2020_Skew$skew - X %*% parameters)) %*% solve((error_variance *
                                                                         idm + cov_mat_ries)) %*%
    (Reis_Et_Al_2020_Skew$skew - X %*% parameters)
  return(Result)
}
optim(par = x_ries, MLE_ries, method = "Nelder-Mead")












max_iterations <- 100

# Set the precision level for the optimization
precision <- 1e-6

# Initialize the iteration counter
iteration <- 0

# Run the optimization in a loop
while (iteration < max_iterations) {
  # Use the Nelder-Mead method for optimization
  result <- optim(par = x0, fn = MLE, method = "Nelder-Mead")
  
  # Check if the optimization has reached the desired precision level
  if (abs(result$value - MLE(x0)) < precision) {
    # If the precision level has been reached, break out of the loop
    break
  }
  
  # Update the iteration counter
  iteration <- iteration + 1
}