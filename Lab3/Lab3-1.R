data =read.delim("/home/krisi211/Desktop/TDDE07/Lab3/rainfall.dat")
set.seed(123)
## a)
# Init values
x = mean(data[,1])
n = length(data[,1])

###### Random values #######
mu_0 = x
tau_0 = 10
nu_0 = 3
sigma_0 = 1
sigma = 1 #Init sigma to some value not zero
############################

nu_n = nu_0 + n
NDraws = 10000

#Gibbs sampling
gibbsDraws = matrix(0,NDraws,2)
for( i in 1:NDraws){
  
  #####FROM LECTURE 2#######
  w = (n/sigma) / (n/sigma + 1/tau_0)
  mu_n = w*x + (1-w)*mu_0
  tau_n = 1/((n/sigma) + (1/tau_0))
  ##########################
  
  mu_gibbs = rnorm(1,mu_n,sqrt(tau_n))
  gibbsDraws[i,1] = mu_gibbs
  
  tau = (nu_0*sigma_0 + sum((data[,1]-mu_gibbs)^2))/(n+nu_0)
  sigma = ((nu_n-1)*tau)/rchisq(1,nu_n-1)
  gibbsDraws[i,2] = sigma
}

hist(gibbsDraws[,1])
hist(gibbsDraws[,2]) 

plot(gibbsDraws[,1],type = 'l')
plot(gibbsDraws[,2],type = 'l')

cusumData =  cumsum(gibbsDraws[,1])/seq(1,NDraws)
plot(1:NDraws, cusumData, type = "l", ylab='Cumulative estimate', xlab = 'MCMC iteration', xlim = c(0,NDraws), 
     main = 'Cusum - Gibbs')
abline(h = mean(gibbsDraws[,1]))

cusumData =  cumsum(gibbsDraws[,2])/seq(1,NDraws)
plot(1:NDraws, cusumData, type = "l", ylab='Cumulative estimate', xlab = 'MCMC iteration', xlim = c(0,NDraws), 
     main = 'Cusum - Gibbs')
abline(h = mean(gibbsDraws[,2]))

## b)
source("/home/krisi211/Desktop/TDDE07/Lab3/NormalMixtureGibbs.R")

## c)
plot(density(data[,1]),col='red',xlim = c(xGridMin,xGridMax))
mu_hat = mean(gibbsDraws[,1])
sigma_hat = mean(gibbsDraws[,2])
lines(xGrid,dnorm(xGrid,mu_hat,sqrt(sigma_hat)), col = 'blue')
lines(xGrid,mixDensMean, type = "l", lwd = 2, lty = 4, col = "black")
      