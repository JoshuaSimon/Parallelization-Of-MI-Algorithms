library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(tidyverse)
library(furrr)
library(ggpubr)

source("dataGenerator.R")

# creating dataframe / seed / nCores
dat <- dataGenerator()
nCores <- detectCores()

# function for mice
miceImp <- function(x, m){
  seed <- 42
  imp <- mice(data = x, m = m, 
              maxit = 5, seed = seed, printFlag = FALSE)
  return(imp)
}

# function for parlmice
parlmiceImp <- function(data, m, cluster.seed, n.core, n.imp.core, cl.type) {
  imp <- parlmice(data = data, m = m, cluster.seed = cluster.seed,
                  n.core = n.core, n.imp.core = n.imp.core, cl.type = cl.type,
                  maxit = 5)
  return(imp)
}


# function for foreach
feImp <- function(x, m){
  seed <- 42
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  clusterSetRNGStream(cl, seed)
  registerDoParallel(cl)
  imp <- foreach(i=1:m, .combine=ibind) %dopar% {
    library(mice)
    mice(data = x, m = 1, maxit = 5, 
         seed = seed, printFlag = FALSE)
  }
  stopCluster(cl)
  return(imp)
}

# function for parLapply
parLImp <- function(x, m){
  seed <- 42
  nCores <- detectCores()
  cl <- makeCluster(nCores)
  clusterSetRNGStream(cl, seed)
  clusterExport(cl=cl, varlist=c("x", "m", "seed", "nCores"),
                envir=environment())
  clusterEvalQ(cl, library(mice))
  
  imps <- parLapply(cl, X = 1:nCores, function(none){
    mice(data = x, m = (m/nCores), maxit = 5, 
         seed = seed, printFlag = FALSE)
  })
  stopCluster(cl)
  imp <- imps[[1]]
  for (i in 2:length(imps)){
    imp <- ibind(imp, imps[[i]])
  }
  return(imp)
}

# function for furrr
furrrImp <- function(x, m){
  seed <- 42
  nCores <- detectCores()
  plan(multisession, workers = nCores)
  imps <- future_map(rep(1, m), ~mice(data = x, m = 1, 
                                      maxit = 5, 
                                      seed = seed,
                                      printFlag = FALSE))
  imp <- imps[[1]]
  for(i in 2:length(imps)){
    imp <- ibind(imp, imps[[i]])
  }
  return(imp)
}

imputations <- c(4, seq(16, 128, 16))

miceTimes <- list()
parlmiceTimes <- list()
foreachTimes <- list()
parLapplyTimes <- list()
furrrTimes <- list()


for (i in imputations){
  
  print(paste0("Starting with ", i, " imputations."))
  
  
  # mice 
  miceTimes <- c(miceTimes,
                 system.time(miceImp(x = dat, m = i)))
  
  # parlmice
  parlmiceTimes <- c(parlmiceTimes,
                     system.time(parlmiceImp(data = dat,
                                             m = i,
                                             cluster.seed = 42,
                                             n.core = detectCores(),
                                             n.imp.core = (i/detectCores()),
                                             cl.type = "PSOCK")))
  
  # foreach
  foreachTimes <- c(foreachTimes,
                    system.time(feImp(x = dat, m = i)))
  
  # parLapply
  parLapplyTimes <- c(parLapplyTimes,
                      system.time(parLImp(x = dat, m = i)))
  
  # furrr
  furrrTimes <- c(furrrTimes,
                  system.time(furrrImp(x = dat, m = i)))
  
  
  print(paste0("Finished ", i, " imputations."))
  
}


runtimeMice <- numeric()
runtimeParlmice <- numeric()
runtimeForeach <- numeric()
runtimeParlapply <- numeric()
runtimeFurrr <- numeric()
idx <- seq(3, 45, 5)

for (i in idx){
  runtimeMice <- c(runtimeMice, miceTimes[[i]])
  runtimeParlmice <- c(runtimeParlmice, parlmiceTimes[[i]])
  runtimeForeach <- c(runtimeForeach, foreachTimes[[i]])
  runtimeParlapply <- c(runtimeParlapply, parLapplyTimes[[i]])
  runtimeFurrr <- c(runtimeFurrr, furrrTimes[[i]])
}

# create df for time
impTimes <- data.frame(
  Runtime = c(runtimeMice, runtimeParlmice, runtimeForeach,
              runtimeParlapply, runtimeFurrr),
  M = c(rep(imputations, 5)),
  Method = c(rep("mice", 9), rep("parlmice", 9), 
             rep("foreach", 9), rep("parLapply", 9),
             rep("furrr", 9))
)

# create df for speedup
impSpeedup <- data.frame(
  Speedup = c(runtimeMice/runtimeParlmice,
              runtimeMice/runtimeForeach,
              runtimeMice/runtimeParlapply,
              runtimeMice/runtimeFurrr),
  M = c(rep(imputations, 4)),
  Method = c(rep("parlmice", 9), 
             rep("foreach", 9), rep("parLapply", 9),
             rep("furrr", 9))
)

# saving the dataframes
# save(impTimes, impSpeedup, file = "miTimeSpeedup.RData")

# plot m vs runtime
runtimePlot <- ggplot(data = impTimes, aes(x = M, y = Runtime, color = Method)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(4, seq(16, 128, 16)))

speedupPlot <- ggplot(data = impSpeedup, aes(x = M, y = Speedup, color = Method)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(4, seq(16, 128, 16)))

# plot both next to each other
ggarrange(runtimePlot, speedupPlot, ncol = 2, nrow = 1)