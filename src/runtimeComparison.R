library(mice)
library(parallel)
library(doParallel)
library(tidyverse)
library(furrr)

# generate data
source("dataGenerator.R")

dat <- dataGenerator()
seed <- 42
nCores <- detectCores()

# mice
miceImp <- function(x){
  imp <- mice(data = x, m = 48, 
              maxit = 5, seed = seed, printFlag = FALSE)
  return(imp)
}

miceTime <- system.time(miceImp(dat))

# foreach
feImp <- function(x){
  seed <- 42
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  clusterSetRNGStream(cl, seed)
  registerDoParallel(cl)
  imp <- foreach(i=1:48, .combine=ibind) %dopar% {
    library(mice)
    mice(data = x, m = 1, maxit = 5, 
         seed = seed, printFlag = FALSE)
  }
  stopCluster(cl)
  return(imp)
}


feTime <- system.time(feImp(dat))

# parlapply
parLImp <- function(x){
  seed <- 42
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  clusterSetRNGStream(cl, seed)
  clusterExport(cl=cl, varlist=c("x", "seed", "nCores"),
                envir=environment())
  clusterEvalQ(cl, library(mice))
  
  imps <- parLapply(cl, X = 1:nCores, function(none){
    mice(data = x, m = (48/nCores), maxit = 5, 
         seed = seed, printFlag = FALSE)
  })
  stopCluster(cl)
  imp <- imps[[1]]
  for (i in 2:length(imps)){
    imp <- ibind(imp, imps[[i]])
  }
  return(imp)
}

parLTime <- system.time(parLImp(dat))

# furrr
furrrImp <- function(x){
  seed <- 42
  nCores <- detectCores()
  plan(multisession, workers = nCores)
  imps <- future_map(rep(1, 48), ~mice(data = x, m = 1, 
                                       maxit = 5, 
                                       seed = seed,
                                       printFlag = FALSE))
  imp <- imps[[1]]
  for(i in 2:length(impFurrr)){
    imp <- ibind(imp, imps[[i]])
  }
  return(imp)
}

furrrTime <- system.time(furrrImp(dat))


# checking integrity of imputations
miceImputation <- complete(miceImp(dat))
feImputation <- complete(feImp(dat))
parLImputation <- complete(parLImp(dat))
furrrImputation <- complete(furrrImp(dat))

summary(miceImputation$x_10)
summary(feImputation$x_10)
summary(parLImputation$x_10)
summary(furrrImputation$x_10)

# The imputed variable is very similar across all 
# methods, which means that all methods should be working
# just fine.

# create df with all times / methods
times <- data.frame(
  time = c(miceTime[1], miceTime[2], miceTime[3],
           feTime[1], feTime[2], feTime[3],
           parLTime[1], parLTime[2], parLTime[3],
           furrrTime[1], furrrTime[2], furrrTime[3]),
  measures = c(rep(c("User", "System", "Elapsed"), 4)),
  method = c(rep("mice", 3), rep("foreach", 3), 
             rep("parLapply", 3), rep("furrr", 3))
)

# graphical comparison of methods
ggplot(times, aes(fill = measures, y = time, x = method,
                  label = round(time, 4))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(position = position_dodge2(width = 0.9, preserve = "single"), 
            angle = 90, vjust=0.25, hjust=0) +
  ggtitle("Comparing runtime of different parallelization methods in R
          using mice and m = 48 imputations") +
  ylab("Runtime in seconds") + xlab("Method / Package")









