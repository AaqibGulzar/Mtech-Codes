library(Rcpp)
# Way to run a loop in a faster way using C++ functions
# We are in an R script and define a C++ function with cppFunction(' ')

cppFunction(' 
 
 // Define function "this_is_a_cpp_function" which 
 // returns a string and takes no input
 std::string this_is_a_cpp_function() {
 
   // Define a string variable called "output" 
   // and set its value to "Statistics Globe shows me Rcpp"
   std::string output = "Statistics Globe shows me Rcpp";
 
   // Return variable output.
   // We defined function "this_is_a_cpp_function" to 
   // return a string. Therefore, we can only 
   // return a string variable here.
   return output;
 }
 
')
# Semicolons! We have to end every code line with a semicolon 
# (which – frankly – is easily forgotten by R users).
# In the above code, we only use datatype string which is part of
# the std namespace and therefore called via std::string. A string object
# in C++ is like a character object in R. Other data types include bool (boolean),
# int (integer) or double (double floating numbers).


#===============================================================================
# When you write C++ code, the above used Rcout commands are very useful. 
# You can use them to print certain information when conducting a function, 
# similar to base R functions cat() and print().
cppFunction(' 
 std::string this_is_a_second_cpp_function() {
   std::string output = "Test";
   Rcout << "Indices start with 0!" << std::endl;
   Rcout << "output = " << output << std::endl;
   Rcout << "output[0] = " << output[0] << std::endl;
   return output;
 }
')


# Define a C++ function for the loop
cppFunction(' 
 double loop_fun_cpp(int n) {
   double output = 0;
   for (int i=1; i<=n; i++) {
     output = sin(i + output);
   }
   return output;
 }
')

loop_fun_r(100000000) %>% system.time()
# user  system elapsed 
# 11.50    0.02   20.19 

# Define an R function for the loop
loop_fun_r = function (n) {
  output = 0
  for (i in 1:n) {
    output = sin(i + output)
  }
  return(output)
}

loop_fun_cpp(100000000) %>% system.time()
# user  system elapsed 
# 4.62    0.03   14.57 


if (!require('rbenchmark', quietly = TRUE)) { install.packages('rbenchmark') } 
library('rbenchmark') # Load package 'rbenchmark'

# Compare both functions for 100 evaluations
n = 10^6

benchmark(loop_fun_cpp(n),
          loop_fun_r(n),
          replications = 100)[,1:4]

#===============================================================================
# Define an R function for the loop
r_loop2 <- function (I) {
  result <- vector(length = I)
  result[1] <- 1
  for (i in 2:I) {
    result[i] <- cos(i - result[i-1])
  }
  return(result)
}

# Install R package RcppArmadillo if not already installed.
if (!require('RcppArmadillo',
             quietly = TRUE)) { install.packages('RcppArmadillo') } 

# We make some comments on the C++ code. For writing C++ code, we often find it 
# convenient to use the RcppArmadillo package. Armadillo is a C++ linear algebra 
# library (by Conrad Sanderson) that strives for a good balance between speed and
# ease of use.
# 
# With arma::vec cpp_loop2(int I), we define a function called ‘cpp_loop2’ which 
# takes as input an integer I and returns an Armadillo vector (we state that by the
# arma::vec before the function name). With Armadillo, we simply initiate a vector 
# called ‘result’ of length I as arma::vec result(I).

# Define a C++ function for the loop
cppFunction(' 
 arma::vec cpp_loop2(int I) {
  arma::vec result(I);
  result[0] = 1;
    for (int i=1; i<=I; i++) {
     result[i] = cos((i+1) - result[i-1]);
   }
   return result;
 }
', 
depends = c("RcppArmadillo"))

#===============================================================================
# As another example, we implement a loop with a stopping condition. For iterative
# algorithms, usually we set a maximum number of iterations and use conditions
# indicating how much the parameters of interest change between the iterations. 
# As an example, we take a look at the Geometric series. 
httr::BROWSE(url = "https://en.wikipedia.org/wiki/Geometric_series")
# Define an R function for the loop
r_loop3 <- function (maxiter = 500, eps = 10^(-10), q = 0.5) {
  
  # initialize values
  iter       <- 0
  result_old <- 10
  result_new <- 0
  
  # start while loop
  while (iter <= maxiter & abs(result_old - result_new) > eps ) {
    result_old <- result_new 
    result_new <- result_old + q^iter
    iter       <- iter + 1
  }
  
  cat(paste0("Stopped at iter = ", iter, " iterations\n"))
  
  # return results
  return(result_new)
}


# Define a C++ funtion for the loop
cppFunction(' 
 double cpp_loop3(int maxiter = 500, double eps = 10^(-10), double q = 0.5) {
 
  // initialize values
  int iter          = 0;
  double result_old = 10;
  double result_new = 0;
 
  // start while loop
  while (iter <= maxiter && std::abs(result_old - result_new) > eps) {
      result_old = result_new; 
      result_new = result_old + std::pow(q, iter);
      iter++;
  }
 
   Rcout << "Stopped at iter = " << iter << " iterations" << std::endl;
 
   // return output
   return result_new;
 }
')

# Check whether our loops indicate that the statement that the series converges to
# 1/(1−q) holds for q=0.5.

q = 0.5

r_loop3(maxiter = 1000, eps = 10^(-10), q = q)
# Stopped at iter = 35 iterations
# [1] 2

cpp_loop3(maxiter = 1000, eps = 10^(-10), q = q)
# Stopped at iter = 35 iterations
# [1] 2

1 / (1-q)
# [1] 2



























