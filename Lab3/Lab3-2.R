data = read.delim("/home/ponsv690/Documents/TDDE07/Lab3/eBayNumberOfBidderData.dat",sep = "")
X = as.matrix(data[,-1])
y = data[1]
## a)
glm.model= glm(nBids ~0+.,data = data, family = poisson)

#Significant covariates: Sealed, VerifyId, MajBlem(Semi), MinBidShare 

## b)

mu = matrix(0,dim(X)[2],1)
sigma = 100 * solve(t(X)%*%X)
initVal = c(rep(0,dim(X)[2]))
library("mvtnorm")

logPoisson = function(betas, y,X,mu,sigma){
  logPos = (sum(y*betas%*%t(X) - exp(betas%*%t(X)) - log(factorial(y))))
  
  if (abs(logPos) == Inf) logPos = -20001
  logPrior = dmvnorm(betas, mu, sigma)
  return(logPos + logPrior)
}

OptimResults = optim(initVal,logPoisson,gr=NULL,y,X,mu,sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)
beta.tilde = OptimResults$par
inv.hessian = -solve(OptimResults$hessian)

beta = rmvnorm(10000, beta.tilde, inv.hessian) 
(colMeans(beta))
coef(glm.model)

## c)
