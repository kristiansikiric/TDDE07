### 1 a)
set.seed(235)

# Variables
s = 14
n = 20
a0 = b0 = 2
f = n - s

rand.samples = rbeta(n*100, a0 + s, b0 + f)

# True values
mean = (a0 + s) / (a0 + s + b0 + f)
variance = ((a0 + s) * (b0 + f)) / (((a0 + s) + (b0 + f))^2 * ((a0 + s) + (b0 + f) + 1))
stdev = variance^0.5

# Plots
hist(rand.samples,freq=FALSE)
x = seq(0,1,length=100)
curve(dbeta(x,a0+s,b0+f),add=TRUE,col="red")

### 1 b)
set.seed(235)

nDraws = 10000
rand.samples = rbeta(nDraws, a0 + s, b0 + f)
p_calc = 100*sum(rand.samples<0.4)/length(rand.samples)
p_true = 100*pbeta(0.4, a0+s,b0+f)

### 1 c)

prior = beta(a0, b0)
hist(log(rand.samples/(1-rand.samples)), freq=FALSE)
lines(density(log(rand.samples/(1-rand.samples))))
x = seq(-1,2,length=100)
curve(dnorm(x,0.8566,1))
