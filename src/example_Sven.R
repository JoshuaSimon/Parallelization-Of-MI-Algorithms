library(mice)
library(parmice)
library(parallel)
library(doParallel)
library(tidyverse)
library(foreach)


# Generate random data (see Session 2)
seed <- 42
set.seed(seed)

# 10000 Observations:
n1 <- 10000

# 3 variables
x_1 <- rnorm(n1, 5, 2)
# x_2 depending on the values of x_1
x_2 <- 5 - 0.7 * x_1 + rnorm(n1, 0, 4)
# x_3 depending on the values of x_1 und x_2
x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n1, 0, 2)

# Proportion of missing data for x_3
p_mis <- 0.25

dat <- data.frame(x_1, x_2, x_3)

# generating NAs
mis <- sample(1:n1, p_mis * n1, replace = FALSE)
dat[mis, 3] <- NA

sapply(dat, function(x) sum(is.na(x))) # 2500 NAs in x3; MCAR

chains <- c(1, seq(10, 100, 10))

seqTimes <- numeric()
parlmTimes <- numeric()
parTimes <- numeric()

for (i in chains){
  
  # print current iteration
  print(paste0("Current number of imputations m: ", i))
  
  # set seed and number of cores
  set.seed(seed)
  nCores <- detectCores()
  
  # sequential run
  startTime <- Sys.time()
  imp <- mice(data = dat, m = i, maxit = 5, printFlag = FALSE, seed = seed)
  endTime <- Sys.time()
  seqTimes <- c(seqTimes, as.numeric(endTime - startTime))
  
  # parlmice run
  startTime <- Sys.time()
  imp <- parlmice(data = dat, m = i, maxit = 5, printFlag = FALSE,
                  cluster.seed = seed, n.core = nCores, 
                  n.imp.core = ifelse(i==1, 1, i%/%nCores))
  endTime <- Sys.time()
  parlmTimes <- c(parlmTimes, as.numeric(endTime - startTime))
  
  # parallel run using foreach
  cluster <- makeCluster(nCores)
  clusterSetRNGStream(cluster, seed)
  
  startTime <- Sys.time()
  registerDoParallel(cluster)
  imp <- foreach(1:i) %dopar% {
    library(mice)
    mice(data = dat, m = 1, maxit = 5, printFlag = FALSE, seed = seed)
  }
  stopCluster(cluster)
  endTime <- Sys.time()
  parTimes <- c(parTimes, as.numeric(endTime - startTime))
  
}

ggplot() + geom_point(aes(x=chains, y=seqTimes, colour="sequential")) +
  geom_line(aes(x=chains, y=seqTimes, colour="sequential")) + 
  geom_point(aes(x=chains, y=parlmTimes, colour="parlmice")) +
  geom_line(aes(x=chains, y=parlmTimes, colour="parlmice")) + 
  geom_point(aes(x=chains, y=parTimes, colour="foreach")) +
  geom_line(aes(x=chains, y=parTimes, colour="foreach")) +
  xlab("Number of chains") + ylab("Time in seconds")