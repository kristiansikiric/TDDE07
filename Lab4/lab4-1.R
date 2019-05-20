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
plot(ar_sims[1,],type = 'l', main = paste("phi = ",phi[1]), ylab = "xt")
plot(ar_sims[11,],type = 'l',main = paste("phi = ",phi[11]), ylab = "xt")
plot(ar_sims[21,],type = 'l', main = paste("phi = ",phi[21]), ylab = "xt")

## b)
suppressMessages(library('rstan'))

x = ar.sim(0.3)
y = ar.sim(0.95)

StanModel = '
data{
  int<lower=1> N;
  real x[N];
}
parameters{
  real mu;
  real phi;
  real sigma2;
}
transformed parameters {
  real sigma;
  sigma = sqrt(sigma2);
}
model{
  for(n in 2:N){
    x[n] ~ normal(mu + phi*(x[n-1]-mu), sigma);
  }
}
'
x.fit = stan(model_code = StanModel, data = list(N = T, x=x),refresh = 0)
y.fit = stan(model_code = StanModel, data = list(N = T, x=y),refresh = 0)

print("Parameters for x")
print(x.fit, digits_summary = 3,pars = c('mu','sigma2','phi'),probs = c(0.025,0.975))
cat("\nParameters for y\n")
print(y.fit,digits_summary = 3,pars = c('mu','sigma2','phi'),probs = c(0.025,0.975))

plot(extract(x.fit)$mu,type = 'l',ylab = c("mu"), main = paste("phi = 0.3"))
plot(extract(y.fit)$mu, type = 'l',ylab="mu", main = paste("phi = 0.95"))
plot(extract(x.fit)$mu,extract(x.fit)$phi)
plot(extract(y.fit)$mu,extract(y.fit)$phi)

## c)
data = read.delim("~/Documents/TDDE07/Lab4/campy.dat")

#xt = ar.sim(0.3)

StanModel2 = '
data{
  int<lower=1> N;
  int c[N];
}
parameters{
  real x[N];
  real mu;
  real phi;
  real sigma2;
}
transformed parameters {
  real sigma;
  sigma = sqrt(sigma2);
}
model{
  phi ~ uniform(-1, 1);
  for(n in 2:N){
    x[n] ~ normal(mu + phi*(x[n-1]-mu), sigma);
    c[n-1] ~ poisson(exp(x[n-1]));
  }
  c[N] ~ poisson(exp(x[N]));
}
'

fit.stan = function(stanModel, data){
  c.fit = stan(model_code = stanModel,data = list(N = length(data$c),c=data$c))
}

plot.stan = function(model, data){
  #Last five elements were not wanted.
  mean = exp(summary(model)$summary[1:140,'mean'])
  cred_97.5 = exp(summary(model)$summary[1:140,'97.5%'])
  cred_2.5 = exp(summary(model)$summary[1:140,'2.5%'])
  plot(data$c)
  lines(mean,col="green")
  lines(cred_2.5,col="red")
  lines(cred_97.5,col="blue")
  legend("topleft", legend = c("Mean", "2.5% cred. interval", "97.5% cred. interval"), col = c("green", "red", "blue"), lty = 1, cex = 0.8)
}

c.fit = fit.stan(StanModel2, data)
plot.stan(c.fit, data)

## d)

StanModel3 = '
data{
  int<lower=1> N;
  int c[N];
}
parameters{
  real x[N];
  real mu;
  real phi;
  real<lower=0> sigma2;
}
transformed parameters {
  real sigma;
  sigma = sqrt(sigma2);
}
model{
  phi ~ uniform(-1, 1);
  sigma2 ~ scaled_inv_chi_square(N,0.05);
  for(n in 2:N){
    x[n] ~ normal(mu + phi*(x[n-1]-mu), sigma);
    c[n-1] ~ poisson(exp(x[n-1]));
  }
  c[N] ~ poisson(exp(x[N]));
}
'

c.fit = fit.stan(StanModel3, data)
plot.stan(c.fit, data)






