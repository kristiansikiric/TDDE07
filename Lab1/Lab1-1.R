### 1 a)
set.seed(235)

# Variables
s = 14
n = 20
a0 = b0 = 2
f = n - s
a_n = a0 + s
b_n = b0 + f

rand.samples = rbeta(n*100, a_n, b_n)

# True values
mean = (a_n) / (a_n+b_n)
variance = ((a_n) * (b_n)) / (((a_n) + (b_n))^2 * ((a_n) + (b_n) + 1))
stdev = variance^0.5

# Plots
hist(rand.samples,freq=FALSE,main = "Historgram of beta distribution")
abline(v=mean, col = "blue")
abline(v=mean-stdev, col = "green")
abline(v=mean+stdev, col = "green")
x = seq(0,1,length=100)
curve(dbeta(x,a0+s,b0+f),add=TRUE,col="red")

### 1 b)
set.seed(235)

nDraws = 10000
rand.samples = rbeta(nDraws, a_n, b_n)
p_calc = 100*sum(rand.samples<0.4)/length(rand.samples)
p_true = 100*pbeta(0.4, a0+s,b0+f)

### 1 c)

prior = beta(a0, b0)
hist(log(rand.samples/(1-rand.samples)), freq=FALSE)
lines(density(log(rand.samples/(1-rand.samples))))