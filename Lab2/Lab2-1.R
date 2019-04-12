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

sim.beta = function(sigma2, mu, omega){
  rmvnorm(n=1,mean=mu,sigma=sigma2*solve(omega))
}

betas = sapply(var,sim.beta,mu = mu_0, omega = Omega_0)
betas = t(betas)

reg_fun =function(betas,time){
  #FRÅGA!!!!!: Hur får man in epsilon i ekvationen?
  temp = betas[1] + betas[2]*time + betas[3]*time^2
  return(temp)
}

prior.temp = apply(betas[seq(1,25,1),],1,reg_fun, time = data$time)
x = dim(prior.temp)[1]
x.axis = (1:x) / x
#FRÅGA!!!!: Hur tätt ska kurvorna ligga varandra?
plot(x.axis,prior.temp[,1],type = "l", xlab = "Time", ylab = "Temperature",ylim=c(-20,30))
for (i in 2:25) {
  lines(x.axis,prior.temp[,i])
}

## 1b
X = cbind(rep(1,length(data$time)),data$time,data$time^2)
X.prim.X = t(X)%*%X
beta.hat = solve(t(X)%*%X)%*%t(X)%*%data$temp
mu_n = solve((t(X)%*%X + Omega_0))%*%(t(X)%*%X%*%beta.hat + Omega_0%*%mu_0)
Omega_n = X.prim.X + Omega_0
nu_n = nu_0 + dim(X)[1]
nuvar_n = nu_0%*%var_0 + (t(data$temp)%*%data$temp + t(mu_0)%*%Omega_0%*%mu_0 - t(mu_n)%*%Omega_n%*%mu_n)
var_n = as.numeric(nuvar_n/nu_n)

var.post = (nu_n*var_n)/rchisq(Ndraws,nu_n)
hist(var.post) #DOES IT LOOK GOOD??

betas.post = sapply(var.post,sim.beta, mu = mu_n, omega = Omega_n)
betas.post = t(betas.post)

hist(betas[,1])
hist(betas[,2])
hist(betas[,3])

plot(data$temp)
post.temp = apply(betas.post[1:1000,],1,reg_fun,time = data$time)
post.temp.median = apply(post.temp,1,median)
lines(post.temp.median)

post.temp.q = apply(post.temp,1,quantile,probs = c(0.025,0.975),na.rm=TRUE) #How does this work??
lines(post.temp.q[1,],col = "red")
lines(post.temp.q[2,],col = "blue")

## 1c
time = -betas[,2]/(2*betas[,3])
plot(density(time)$x*366,density(time)$y,type="l",xlab = "Days", ylab = "Max Temp dist")

## 1d
# mu_0: vector of length 7, set the last terms to zero so that the higher 
#order terms have no effect.

# omega_0: matrix of dim 7x7, set the last terms in the diagonal to a very high value
# so that the variance of the higher order terms becomes small.