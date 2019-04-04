### 2 a)
set.seed(235)

draws = 10000
income = c(14, 25, 45, 25, 30, 33, 19, 50, 34, 67)
n = 10
v = 9

y = log(income)
tau2=sum((y-3.5)^2)/10
tau = tau2**(0.5)

var<-((n-1)*tau2)/rchisq(draws,n-1)
hist(var,breaks = seq(0,3,length=100),freq=FALSE, xlab="Variance", main="Histogram of variance") 
theta = seq(0,3,length=100)
p = ((v/2)^(v/2))*(tau^v)*(theta^-((v/2)+1))*exp((-v*tau2)/(2*theta))
p = p/gamma(v/2)
lines(theta,p,col="red", lwd=2)
legend("topright", col = c("red"), legend=c("PDF of the Scaled inverse chi-squared"),lty=1)

### 2 b)
set.seed(235)

G = 2*pnorm(var**0.5/2**0.5)-1
hist(G)

### 2 c)
set.seed(235)

quantile(G,probs = seq(0,1,0.025))

newG = G[which(G > 0.1714136)]
newG = newG[which(newG < 0.4396433)]
plot(density(G))
library(coda)
HPDinterval(as.mcmc(G),prob = 0.95)

