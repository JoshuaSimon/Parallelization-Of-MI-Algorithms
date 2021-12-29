#-> Simulate some data and conduct a simulation study to compare different imputation techniques
library(tidyverse)
library(mice)

R <- 30
n <- 2000

  ### Data generation ###
  
  # one normally and one uniformly distributed variable:
  x1 <- rnorm(n, 0, 1)
  x2 <- runif(n, 0, 2)
  
  # continuous outcome variable with non-linear terms and non-linear error term:
  eps <- rchisq(n, 107 / 96)
  y <- -3 + 3.5 * x1 ^ 2 + 2.75 * x2 ^ 3 + (eps - 107 / 96)
  
  data <- as.data.frame(cbind(x1, x2, y))
  data_bd <- data
  
  
  ### Generation of missing data ###
  
  # MAR in y:
  z1 <- rnorm(n, 0, sqrt(16)) * x2
  lin_y <- 1.75 - 1.5 * z1
  prob_y <- pnorm(lin_y)    # yields around 35% missing values
  res_y <- rbinom(n, 1, prob_y)
  data$y[which(res_y == 0)] <- NA
  
  

  ### Multiple imputation and Analysis ###

  # use all variables as predictors, but just consider linear relationships..
  # initialization:
  ini <- mice(data, m = 1, maxit = 0)
  pred <- ini$pred


##Parallelization

#Packages
library(foreach)
library(parallel)
library(doParallel)
library(testit)

#Core Idenitification and Clustering
num_cores <- 7
cl <- makeCluster(num_cores)
clusterSetRNGStream(cl, 9956)
registerDoParallel(cl)

#Parallelization of simple MI Imputation with foreach
result <- list()
resultp <- list()
resultparl <- list()
systp <- vector(length=R)
syst <- vector(length=R)
systparl <- vector(length=R)

for(i in 1:R){
  
systp[i] <- system.time(resultp[[i]] <- foreach(no=1:i, .combine=ibind, .export = c("data", "i"), .packages="mice") %dopar% {
  mice(data,m = 1, predictorMatrix = pred, print = F)
})[3] #run time of parallelized MI

syst[i] <- system.time(result[[i]] <- mice(data,m = i, predictorMatrix = pred, print = F))[3]
#run time of sequential MI

systparl[i] <- system.time(resultparl[[i]] <- parlmice(data=data, m=i, predictorMatrix = pred, print = F, cluster.seed = 123))[[3]]
#run time parlmice
}

#Runtime Visualization
X <- cbind(1:R, systp, syst, systparl)
plot(X[,1], X[,2], type="l", ylim=c(0, 3), ylab="Runtime", xlab="Number of Multiple Imputations", col="blue")
lines(X[,1], X[,3], col="red")
lines(X[,1], X[,4], col="green")
stopCluster(cl)


#Create Larger Dataset to improve performance of parlmice
n <- 100000
x1 <- rnorm(n, 0, 1)
x2 <- runif(n, 0, 2)

# continuous outcome variable with non-linear terms and non-linear error term:
eps <- rchisq(n, 107 / 96)
y <- -3 + 3.5 * x1 ^ 2 + 2.75 * x2 ^ 3 + (eps - 107 / 96)

data1 <- as.data.frame(cbind(x1, x2, y))
data_bd1 <- data1

### Generation of missing data ###

# MAR in y:
z1 <- rnorm(n, 0, sqrt(16)) * x2
lin_y <- 1.75 - 1.5 * z1
prob_y <- pnorm(lin_y)    # yields around 35% missing values
res_y <- rbinom(n, 1, prob_y)
data1$y[which(res_y == 0)] <- NA



### Multiple imputation and Analysis ###

# use all variables as predictors, but just consider linear relationships..
# initialization:
ini <- mice(data, m = 1, maxit = 0)
pred <- ini$pred


##Parallelization

#Core Idenitification and Clustering
cl <- makeCluster(num_cores)
clusterSetRNGStream(cl, 9956)
registerDoParallel(cl)

#Parallelization of simple MI Imputation with foreach
result <- list()
resultp <- list()
resultparl <- list()
systp <- vector(length=R)
syst <- vector(length=R)
systparl <- vector(length=R)

for(i in 1:R){
  
  systp[i] <- system.time(resultp[[i]] <- foreach(no=1:i, .combine=ibind, .export = c("data1", "i"), .packages="mice") %dopar% {
    mice(data=data1,m = 1, predictorMatrix = pred, print = F)
  })[3] #run time of parallelized MI
  
  syst[i] <- system.time(result[[i]] <- mice(data=data1,m = i, predictorMatrix = pred, print = F))[3]
  #run time of sequential MI
  
  systparl[i] <- system.time(resultparl[[i]] <- parlmice(data=data1, m=i, predictorMatrix = pred, print = F, cluster.seed = 123))[[3]]
  #run time parlmice
}

#Runtime Visualization
X <- cbind(1:R, systp, syst, systparl)
plot(X[,1], X[,2], type="l", ylab="Runtime", xlab="Number of Multiple Imputations", col="blue")
lines(X[,1], X[,3], col="red")
lines(X[,1], X[,4], col="green")
stopCluster(cl)
