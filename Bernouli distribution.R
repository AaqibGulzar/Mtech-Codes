library(stats4)
library(EnvStats)
library(Rlab)
x = c(1,0,1,0,0,0,0,0,0,1) # this represents a sample of bernouli trails where the
# 1 represents success and 0 fails. So in this random sample out of 10 trials, first
#third and tje tenth event are successes.

dbern(x,prob=0.25) # now we create a bernouli distribution out of x.
# After this we create a maximum likelihood of the function. This example is on 
#the page 267 of jl devore.
mle_ber = function(p) {
  pmf = (p ^ 3) * (1 - p) ^ 7
  return(pmf)
}

p=runif(100) # generate random numbers between 0 and 1 to mimic the probabilities.
plot(p, mle_ber(p)) # plot the likelihood.
plot(p,log10(mle_ber(p))) # plot the log-likelihood
