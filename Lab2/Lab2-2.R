data = read.delim("/home/krisi211/Desktop/TDDE07/Lab2/WomenWork.dat",sep="")

## a
glm.model = glm(Work~0 +., data = data, family = binomial)

## b
tau = 10
library("mvtnorm")
y <- as.vector(data[,1]) # Data from the read.table function is a data frame. Let's convert y and X to vector and matrix.
X <- as.matrix(data[,2:9])
params = dim(X)[2]
