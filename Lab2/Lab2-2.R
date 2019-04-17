data = read.delim("/home/krisi211/Desktop/TDDE07/Lab2/WomenWork.dat",sep="")
#data = read.delim("/home/ponsv690/Documents/TDDE07/Lab2/WomenWork.dat",sep="")
set.seed(235)

## a
glm.model = glm(Work~0 +., data = data, family = binomial)

## b
tau = 10
library("mvtnorm")
y = as.vector(data[,1])
X = as.matrix(data[,2:9])
params = dim(X)[2]

mu = matrix(0,params,1)
Sigma = tau^2*diag(params)

LogPostLogistic = function(betas, y, X, mu, Sigma) {
  params = length(betas)
  linPred = X%*%betas
  
  logLik = sum(linPred*y -log(1+exp(linPred)))
  if (abs(logLik) == Inf) logLik = -20001
  
  logPrior = dmvnorm(betas, mu, Sigma, log=TRUE)
  
  return(logLik + logPrior)
}

initVal = c(rep(0,dim(X)[2]))

OptimResults = optim(initVal,LogPostLogistic,gr=NULL,y,X,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

beta.tilde = OptimResults$par
inv.hessian = -solve(OptimResults$hessian)

beta = rmvnorm(10000,beta.tilde,inv.hessian)

quantile(beta[,7],probs = c(0.025,0.975))
CI.lo = beta.tilde[7] + 1.96*(inv.hessian[7,7])^0.5
CI.hi = beta.tilde[7] - 1.96*(inv.hessian[7,7])^0.5

## c
new.X = c(1, 10, 8,10,1,40,1,1)

mul = function(row){
  return(t(new.X)%*%row)
}

pred = apply(beta,1,mul)

Pr = exp(pred)/(1+exp(pred))

hist(Pr)
