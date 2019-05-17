set.seed(123)
## a)

T=200
mu = 10
sigma2 = 2

t = seq(2,T)
x1 = mu
phi = seq(-1,1,by=0.1)


ar.sim = function(phi){
  x_sample = rep(0,length=length(t))
  x_sample[1] = x1
  for(time in t){
    epsilon_t = rnorm(1,0,sqrt(sigma2))
    x_sample[time] = mu + phi*(x_sample[time-1] - mu) + epsilon_t
  }
  return (x_sample)
}

ar_sims = matrix(ncol = length(t)+1, nrow = length(phi),data = 0)

for (i in 1:length(phi)){
  ar_sims[i,] = ar.sim(phi[i])
}
#Hur ska man tolka det här?
plot(ar_sims[1,],type = 'l', main = paste("phi = ",phi[1]))
plot(ar_sims[10,],type = 'l',main = paste("phi = ",phi[11]))
plot(ar_sims[20,],type = 'l', main = paste("phi = ",phi[21]))

## b)
library('rstan')
