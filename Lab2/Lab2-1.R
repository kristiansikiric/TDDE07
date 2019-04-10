data =read.delim("/home/krisi211/Desktop/TDDE07/Lab2/TempLinkoping.txt")

set.seed(123)

Ndraws = 10000
mu_0 = t(data.frame(-10,100,-100))
Omega_0 = 0.01*diag(3)
nu_0 = 4
var_0 = 1 


var<-(nu_0*var_0)/rchisq(Ndraws,nu_0)
hist(var,freq = FALSE)

beta = matrix(0,Ndraws,3)
library(mvtnorm)
for(i in 1:Ndraws){
  beta[i,] = rmvnorm(n=1,mean=mu_0,sigma=var[i]*solve(Omega_0))
}

