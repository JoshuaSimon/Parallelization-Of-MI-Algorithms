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
seed <- 42

source("parallel_functions.R")

imputations <- c(detectCores(), seq(16, 128, 16))

miceTimes <- list()
parlmiceTimes <- list()
foreachTimes <- list()
parLapplyTimes <- list()
furrrTimes <- list()


for (i in imputations){
  
  print(paste0("Starting with ", i, " imputations."))
  
  
  # mice 
  miceTimes <- c(miceTimes,
                 system.time(mice(data = dat, m = i, 
                                  seed = seed,
                                  printFlag = FALSE,
                                  maxit = 5)))
  
  # parlmice
  parlmiceTimes <- c(parlmiceTimes,
                     system.time(parlmice_wrap(data = dat,
                                               m = i,
                                               cluster.seed = seed,
                                               n.core = nCores,
                                               n.imp.core = i/nCores,
                                               cl.type = "PSOCK")))
  
  # foreach
  foreachTimes <- c(foreachTimes,
                    system.time(foreach_wrap(data = dat,
                                                 num_imp = i,
                                                 seed = seed,
                                                 num_cores = nCores,
                                                 backend = "PSOCK")))
  
  # parLapply
  parLapplyTimes <- c(parLapplyTimes,
                      system.time(parLapply_wrap(data = dat, 
                                                 num_imp = i,
                                                 seed = seed,
                                                 num_cores = nCores,
                                                 backend = "PSOCK")))
  
  # furrr
  furrrTimes <- c(furrrTimes,
                  system.time(furrr_wrap(data = dat, 
                                             num_imp = i,
                                             seed = seed,
                                             num_cores = nCores,
                                             backend = "PSOCK")))
  
  
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
  scale_x_continuous(breaks = c(detectCores(), seq(16, 128, 16)))

speedupPlot <- ggplot(data = impSpeedup, aes(x = M, y = Speedup, color = Method)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(detectCores(), seq(16, 128, 16)))

# plot both next to each other
ggarrange(runtimePlot, speedupPlot, ncol = 2, nrow = 1)

