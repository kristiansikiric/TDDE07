data = read.delim("//Users/kristiansikiric/Desktop/TDDE07/Lab3/eBayNumberOfBidderData.dat", sep = "")
#data = read.delim("/home/ponsv690/Documents/TDDE07/Lab3/eBayNumberOfBidderData.dat",sep = "")
set.seed(123)
X = as.matrix(data[,-1])
y = data[1]
## a)
glm.model= glm(nBids ~0+.,data = data, family = poisson)

#Significant covariates: Sealed, VerifyId, MajBlem(Semi), MinBidShare 

## b)

mu = matrix(0,dim(X)[2],1)
sigma = 100 * solve(t(X)%*%X)
initVal = rep(0,dim(X)[2])
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
rwm = function(var, LogPost, theta_prev, ...){
  # Step 1
  theta_p = rmvnorm(1,theta_prev,var)
  
  # Step 2
  post.theta_prev = LogPost(theta_prev,...)
  post.theta_p = LogPost(theta_p,...)
  alpha = min(1, exp(post.theta_p-post.theta_prev))
  
  # Step 3
  accepted = runif(1,0,1) < alpha
  
  if(accepted){
    return(list(theta=theta_p, accepted=accepted))
  }
  else {
    return(list(theta=theta_prev, accepted=accepted))
  }
}

mcmc = function(LogPost, ndraws, ncov) {
  c = 5
  var = c * inv.hessian
  beta = matrix(rep(rep(0, ncov), ndraws), ncol = ncov)
  accepted = rep(0, ndraws-1)
  
  for(i in 2:ndraws) {
    sample = rwm(var, LogPost, beta[i-1,], y, X, mu, sigma)
    beta[i,] = sample$theta
    accepted[i-1] = sample$accepted
  }
  
  sum(accepted) / (ndraws-1)
  #plot(beta[,1], type = 'l')
  plot(beta[,2], type = 'l')
  plot(beta[,3], type = 'l')
  plot(beta[,4], type = 'l')
  plot(beta[,5], type = 'l')
  plot(beta[,6], type = 'l')
  plot(beta[,7], type = 'l')
  plot(beta[,8], type = 'l')
  plot(beta[,9], type = 'l')
  
  
  return(beta)
}
betas = mcmc(logPoisson, 10000, dim(X)[2])
colMeans(betas)
colMeans(beta) #From b)
