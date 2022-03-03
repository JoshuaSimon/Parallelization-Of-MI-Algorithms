library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(tidyverse)
library(ggpubr)
library(foreach)



# Data generator that can generate between 
# n=2 and n=5 variables with 1 to n-1 variables
# containing p_mis missing values

dataGenerator <- function(n = 10000, variables = 2, mis = 1, p_mis = 0.3){
  seed <- 42
  set.seed(seed)
  
  if (variables == 2 & mis == 1){
    
    # 2 Variables
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    
    dat <- data.frame(x_1, x_2)
    mis_x2 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x2, 2] <- NA
    return(dat)
    
  } else if (variables == 3 & mis == 1){
    
    # 3 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    
    dat <- data.frame(x_1, x_2, x_3)
    mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x3, 3] <- NA
    return(dat)
    
  } else if (variables == 3 & mis == 2){
    
    # 3 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    
    dat <- data.frame(x_1, x_2, x_3)
    mis_x2 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x2, 2] <- NA
    dat[mis_x3, 3] <- NA
    return(dat)
    
  } else if (variables == 4 & mis == 1){
    
    # 4 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x4, 4] <- NA
    return(dat)
    
  } else if (variables == 4 & mis == 2){
    
    # 4 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x3, 3] <- NA
    dat[mis_x4, 4] <- NA
    return(dat)
    
  } else if (variables == 4 & mis == 3){
    
    # 4 variables
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x2 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x2, 2] <- NA
    dat[mis_x3, 3] <- NA
    dat[mis_x4, 4] <- NA
    return(dat)
    
  } else if (variables == 5 & mis == 1){
    
    # 5 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    x_5 <- 5 + 0.3 * x_1 + 0.4 * x_2 - 0.1 * x_3 - 0.2 * x_4 + rnorm(n, 2, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x5 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x5, 5] <- NA
    return(dat)
    
  } else if (variables == 5 & mis == 2){
    
    # 5 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    x_5 <- 5 + 0.3 * x_1 + 0.4 * x_2 - 0.1 * x_3 - 0.2 * x_4 + rnorm(n, 2, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x5 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x4, 4] <- NA
    dat[mis_x5, 5] <- NA
    return(dat)
    
  } else if (variables == 5 & mis == 3){
    
    # 5 Variables 
    x_1 <- rnorm(n, 5, 2)
    x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
    x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
    x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
    x_5 <- 5 + 0.3 * x_1 + 0.4 * x_2 - 0.1 * x_3 - 0.2 * x_4 + rnorm(n, 2, 5)
    
    dat <- data.frame(x_1, x_2, x_3, x_4)
    mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
    mis_x5 <- sample(1:n, p_mis * n, replace = FALSE)
    
    dat[mis_x3, 3] <- NA
    dat[mis_x4, 4] <- NA
    dat[mis_x5, 5] <- NA
    return(dat)
    
  } else if (variables == 5 & mis == 4){
  
    # 5 Variables 
    x_1 <- rnorm(n, 5, 2)
  x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
  x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
  x_4 <-  5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
  x_5 <- 5 + 0.3 * x_1 + 0.4 * x_2 - 0.1 * x_3 - 0.2 * x_4 + rnorm(n, 2, 5)
  
  dat <- data.frame(x_1, x_2, x_3, x_4)
  mis_x2 <- sample(1:n, p_mis * n, replace = FALSE)
  mis_x3 <- sample(1:n, p_mis * n, replace = FALSE)
  mis_x4 <- sample(1:n, p_mis * n, replace = FALSE)
  mis_x5 <- sample(1:n, p_mis * n, replace = FALSE)
  
  dat[mis_x2, 2] <- NA
  dat[mis_x3, 3] <- NA
  dat[mis_x4, 4] <- NA
  dat[mis_x5, 5] <- NA
  return(dat)
  
  } else {
    stop("argument 'size' can only take on values between n=2 and n=5;
         argument 'mis' must be at least 1, at most n-1")
  }
  
  return(dat)
}

# function to measure runtime of different MI implementations
impTimes <- function(x, chains = c(1, seq(16,96,16))){
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
    imp <- mice(data = x, m = i, maxit = 5, 
                printFlag = FALSE, seed = seed)
    endTime <- Sys.time()
    seqTimes <- c(seqTimes, 
                  as.numeric(difftime(endTime, startTime, 
                                      units="secs")))
    print(paste0("Time for ", i, 
                 " imputations (sequential): ",
                 round(as.numeric(difftime(endTime, 
                                           startTime, 
                                           units="secs")),4),
                 " seconds"))
    
    # mice.par run
    startTime <- Sys.time()
    imp <- mice.par(don.na = x, m = i, maxit = 5,
                    printFlag = FALSE, seed = seed)
    endTime <- Sys.time()
    miceparTimes<- c(miceparTimes, 
                     as.numeric(difftime(endTime, startTime, 
                                         units="secs")))
    print(paste0("Time for ", i, 
                 " imputations (mice.par): ",
                 round(as.numeric(difftime(endTime, 
                                           startTime, 
                                           units="secs")),4),
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
                  as.numeric(difftime(endTime, startTime, 
                                      units="secs")))
    print(paste0("Time for ", i, 
                 " imputations (foreach): ",
                 round(as.numeric(difftime(endTime, 
                                           startTime, 
                                           units="secs")),4),
                 " seconds"))
  }
  
  return(cbind.data.frame(sequential = seqTimes,
                          micePar = miceparTimes,
                          fePar = parTimes,
                          imputations = chains,
                          variables = rep(ncol(x), 
                                         length(seqTimes))))
}


# test runtimes for 2:5 variables with n-1 missings
dfList <- list()
dfList1 <- list()
idx <- 1

for (n in c(2:5)){
  # print current amount of variables
  print(paste0("Vars: ", n, " Vars with missings: ", n-1))
  
  # run different methods and return df with times
  imputations <- impTimes(dataGenerator(variables = n,
                                        mis = n-1))
  imputationsMiss1 <- impTimes(dataGenerator(variables = n,
                                             mis = 1))
  # add to dfList and continue indexing
  dfList[[idx]] <- imputations
  dfList1[[idx]] <- imputationsMiss1
  
  idx <- idx + 1
}


# plot for n-1 variables containing missings
imputationTimes <- do.call("rbind", dfList)

plotFunction <- function(vars=2){
  imputationTimes %>% filter(variables == vars) %>% 
    ggplot() + 
    geom_point(aes(x=imputations, y=sequential, color="sequential")) +
    geom_line(aes(x=imputations, y=sequential, color="sequential")) +
    geom_point(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_line(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_point(aes(x=imputations, y=fePar, color="foreach")) +
    geom_line(aes(x=imputations, y=fePar, color="foreach")) +
    xlab("Number of Imputations") + ylab("Time in seconds") +
    ggtitle(paste0("Variables: ", vars, " - Variables containing missing: ", vars-1)) + 
    scale_x_continuous(breaks=seq(16,96,16))
}

plot1 <- plotFunction(vars=2)
plot2 <- plotFunction(vars=3)
plot3 <- plotFunction(vars=4)
plot4 <- plotFunction(vars=5)

ggarrange(plot1, plot2, plot2, plot4,
          ncol = 2, nrow = 2)

# plot for 1 variable containing missings
imputationTimes <- do.call("rbind", dfList1)

plotFunction <- function(vars=2){
  imputationTimes %>% filter(variables == vars) %>% 
    ggplot() + 
    geom_point(aes(x=imputations, y=sequential, color="sequential")) +
    geom_line(aes(x=imputations, y=sequential, color="sequential")) +
    geom_point(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_line(aes(x=imputations, y=micePar, color="mice.par")) +
    geom_point(aes(x=imputations, y=fePar, color="foreach")) +
    geom_line(aes(x=imputations, y=fePar, color="foreach")) +
    xlab("Number of Imputations") + ylab("Time in seconds") +
    ggtitle(paste0("Variables: ", vars, " - Variables containing missing: 1")) + 
    scale_x_continuous(breaks=seq(16,96,16))
}

plot1 <- plotFunction(vars=2)
plot2 <- plotFunction(vars=3)
plot3 <- plotFunction(vars=4)
plot4 <- plotFunction(vars=5)

ggarrange(plot1, plot2, plot3, plot4,
          ncol = 2, nrow = 2)
