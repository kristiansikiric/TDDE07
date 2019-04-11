data =read.delim("/home/krisi211/Desktop/TDDE07/Lab2/TempLinkoping.txt")

set.seed(235)

## 1a
Ndraws = 10000
mu_0 = t(data.frame(-15,150,-150))
Omega_0 = 1*diag(3)
nu_0 = 40
var_0 = 1


var<-(nu_0*var_0)/rchisq(Ndraws,nu_0)
hist(var,freq = FALSE)

beta = matrix(0,Ndraws,3)
library(mvtnorm)

sim.beta = function(sigma2){
  rmvnorm(n=1,mean=mu_0,sigma=sigma2*solve(Omega_0))
}

betas = sapply(var,sim.beta)
betas = t(betas)

prior =function(betas,time){
  #FRÅGA!!!!!: Hur får man in epsilon i ekvationen?
  temp = betas[1] + betas[2]*time + betas[3]*time^2
  return(temp)
}

prior.temp = apply(betas[seq(1,25,1),],1,prior, time = data$time)
x = dim(prior.temp)[1]
x.axis = (1:x) / x
#FRÅGA!!!!: Hur tätt ska kurvorna ligga varandra?
plot(x.axis,prior.temp[,1],type = "l", xlab = "Time", ylab = "Temperature",ylim=c(-20,30))
for (i in 2:25) {
  lines(x.axis,prior.temp[,i])
}

## 1b
X = cbind(rep(1,length(data$time)),data$time,data$time^2)
beta.hat = solve(t(X)%*%X)%*%t(X)%*%data$temp
mu_n = solve((t(X)%*%X + Omega_0))%*%(t(X)%*%X%*%beta.hat + Omega_0%*%mu_0)
Omega_n
