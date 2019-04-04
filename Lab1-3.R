set.seed(235)

data = c(-2.44, 2.14, 2.54, 1.83, 2.02, 2.33, -2.79, 2.23, 2.07, 2.02)

mu = 2.39
k = seq(0.001, 10, 0.01)

calc_posterior = function(k){
  prior = dexp(k)
  likelihood = exp(k * cos(data - mu))/(2 * pi * besselI(k, 0))
  post = prod(likelihood) * prior

  return(post)  
}

post = sapply(k, calc_posterior)

plot(k, post, type = "l")
grid()
