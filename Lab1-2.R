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

q = quantile(G,probs=c(0.025,0.975))
print(paste("Lower threshold of the credible interval: ", q[1]))
print(paste("Upper threshold of the credible interval: ", q[2]))
newG = G[which(G > q[1])]
newG = newG[which(newG < q[2])]
plot(density(G), main="PDF of G", xlab="")
legend("topright", col = c("red", "dodgerblue4"), legend=c("Credible interval", "HPD interval"),lty=1)
abline(v=q[1],col="red")
abline(v=q[2],col="red")


dx <- density(G)
dn <- cumsum(dx$y)/sum(dx$y)
li <- which(dn>=0.025)[1]
ui <- which(dn>=0.975)[1]
dx$x[c(li,ui)]
print(paste("Lower threshold of the highest posterior density interval: ", dx$x[c(li,ui)][1]))
print(paste("Upper threshold of the highest posterior density interval: ", dx$x[c(li,ui)][2]))
abline(v = 0.1690950,col="dodgerblue4")
abline(v= 0.4408587,col="dodgerblue4")
abline(h=0)

