#data = read.delim("/home/krisi211/Desktop/TDDE07/Lab2/WomenWork.dat",sep="")
data = read.delim("/home/ponsv690/Documents/TDDE07/Lab2/WomenWork.dat",sep="")


## a
glm.model = glm(Work~0 +., data = data, family = binomial)

## b
tau = 10
library("mvtnorm")
y = as.vector(data[,1]) # Data from the read.table function is a data frame. Let's convert y and X to vector and matrix.
X = as.matrix(data[,2:9])
params = dim(X)[2]

mu = matrix(0,params,1)
Sigma = tau^2*diag(params)

LogPostLogistic = function(betas, y, X, mu, Sigma) { # Kan ni kugga oss för det här?
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

numerical.beta = rmvnorm(10000,beta.tilde,inv.hessian) #Ser ut som skit
hist(numerical.beta)
