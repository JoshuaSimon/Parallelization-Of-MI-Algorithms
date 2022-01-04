library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(tidyverse)
library(ggpubr)
library(foreach)

dataGenerator <- function(n = 10000, p_mis = 0.25){
  seed <- 42
  set.seed(seed)
  
  # 3 variables
  x_1 <- rnorm(n, 5, 2)
  # x_2 depending on the values of x_1
  x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
  # x_3 depending on the values of x_1 und x_2
  x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
  # x_4 depending on values of x_1, x_2, x_3
  x_4 <- 5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
  
  dat <- data.frame(x_1, x_2, x_3)
  
  # generating NAs
  mis <- sample(1:n, p_mis * n, replace = FALSE)
  mis2 <- sample(1:n, 0.5 * n, replace = FALSE)
  dat[mis, 3] <- NA
  dat[mis2, 4] <- NA
  
  return(dat)
}

impTimes <- function(x, chains = c(1, seq(10,100,10))){
  seqTimes <- numeric()
  miceparTimes <- numeric()
  parTimes <- numeric()
  
  
  # set seed and number of cores
  seed <- 42
  set.seed(seed)
  nCores <- detectCores()
  
  for (i in chains){
    # print current iteration
    print(paste0("Current number of imputations m: ", i))
    
    # sequential run
    startTime <- Sys.time()
    imp <- mice(data = dat, m = i, maxit = 5, 
                printFlag = FALSE, seed = seed)
    endTime <- Sys.time()
    seqTimes <- c(seqTimes, 
                  as.numeric(endTime - startTime))
    print(paste0("Time for ", i, 
                 " imputations (sequential): ",
                 round(as.numeric(endTime - startTime), 4),
                 " seconds"))
    
    # mice.par run
    startTime <- Sys.time()
    imp <- mice.par(don.na = dat, m = i, maxit = 5,
                    printFlag = FALSE, seed = seed)
    endTime <- Sys.time()
    miceparTimes<- c(miceparTimes, 
                     as.numeric(endTime - startTime))
    print(paste0("Time for ", i, 
                 " imputations (mice.par): ",
                 round(as.numeric(endTime - startTime), 4),
                 " seconds"))
    
    # foreach run
    cluster <- makeCluster(nCores)
    clusterSetRNGStream(cluster, seed)
    
    startTime <- Sys.time()
    registerDoParallel(cluster)
    imp <- foreach(1:i, .combine = ibind) %dopar% {
      library(mice)
      mice(data = x, m = 1, maxit = 5, 
           printFlag = FALSE, seed = seed)
    }
    stopCluster(cluster)
    endTime <- Sys.time()
    parTimes <- c(parTimes, 
                  as.numeric(endTime - startTime))
    print(paste0("Time for ", i, 
                 " imputations (foreach): ",
                 round(as.numeric(endTime - startTime), 4),
                 " seconds"))
  }
  
  return(cbind.data.frame(sequential = seqTimes,
                          micePar = miceparTimes,
                          fePar = parTimes,
                          imputations = chains,
                          dataSize = rep(nrow(x), 
                                         length(seqTimes))))
}


sizes <- c(10000, seq(25000, 100000, 25000))
chains <- c(seq(16,96,16))
nCores <- detectCores()

dfList <- list()

idx <- 1

for (size in sizes){
  # print current data size
  print(paste0("Data size: ", size))
  
  # generate data with different sizes 
  dat <- dataGenerator(n = size, p_mis = 0.25)
  imputationTimes <- impTimes(x = dat, chains = chains)
  
  # add parlmice, because it doesn't work inside
  # of functions for whatever reason
  parlmTimes <- numeric()
  
  for (i in chains){
    seed <- 42
    set.seed(42)
    
    startTime <- Sys.time()
    imp <- parlmice(data = dat, m = i, maxit = 5, 
                    printFlag = FALSE,
                    cluster.seed = seed, n.core = nCores, 
                    n.imp.core = ifelse(i==1, 1, i%/%nCores))
    endTime <- Sys.time()
    parlmTimes <- c(parlmTimes, 
                    as.numeric(endTime - startTime))
    print(paste0("Time for ", i, 
                 " imputations (parlmice): ",
                 round(as.numeric(endTime - startTime), 4),
                 " seconds"))
  }
  
  imputationTimes <- cbind.data.frame(imputationTimes,
                                      parlm = parlmTimes)
  
  dfList[[idx]] <- imputationTimes
  
  idx <- idx + 1
  
}

imputationTimes <- do.call("rbind", dfList)
#imputationTimes$dataSize <- as.numeric(format(imputationTimes$dataSize,
#                                   scientific = F))

plotFunction <- function(size=10000){
  imputationTimes %>% filter(dataSize == size) %>% 
    ggplot() + 
    geom_point(aes(x=imputations, y=sequential, color="sequential")) +
    geom_line(aes(x=imputations, y=sequential, color="sequential")) +
    geom_point(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_line(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_point(aes(x=imputations, y=fePar, color="foreach")) +
    geom_line(aes(x=imputations, y=fePar, color="foreach")) +
    geom_point(aes(x=imputations, y=parlm, color="parlmice")) +
    geom_line(aes(x=imputations, y=parlm, color="parlmice")) +
    xlab("Number of Imputations") + ylab("Time in seconds") +
    ggtitle(paste0("Data size: ", size)) + 
    scale_x_continuous(breaks=seq(16,96,16))
}

plot10k <- plotFunction(10000)
plot25k <- plotFunction(25000)
plot50k <- plotFunction(50000)
plot75k <- plotFunction(75000)
plot100k <- plotFunction(100000)


ggarrange(plot10k,
          plot25k,
          plot50k,
          plot75k,
          plot100k,
          ncol = 3,
          nrow = 2)
