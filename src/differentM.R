library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(tidyverse)
library(furrr)
library(ggpubr)
library(future.apply)

source("dataGenerator.R")

# creating dataframe / seed / nCores
dat <- dataGenerator()
nCores <- detectCores()
seed <- 42

source("parallel_functions.R")

imputations <- c(detectCores(), seq(16, 128, 16))
dfListImp <- list()
dfListSpeed <- list()

for (i in 1:10){
  print(paste0("Iteration ", i, " started."))
  
  # create empty lists for storing the data
  miceTimes <- list()
  parlmiceTimes <- list()
  miceParTimes <- list()
  foreachTimes <- list()
  parLapplyTimes <- list()
  furrrTimes <- list()
  futureTimes <- list()
  
  
  for (m in imputations){
    print(paste0("Starting with ", m, " imputations."))
    # mice 
    miceTimes <- c(miceTimes,
                   system.time(mice_wrap(data = dat,
                                         num_imp = m, 
                                         seed = seed, 
                                         num_cores = detectCores(), 
                                         backend = "PSOCK")))
    # parlmice
    parlmiceTimes <- c(parlmiceTimes,
                       system.time(parlmice_wrap(data = dat,
                                                 m = m,
                                                 cluster.seed = seed,
                                                 n.core = nCores,
                                                 n.imp.core = m/nCores,
                                                 cl.type = "PSOCK")))
    # micemd / mice.par
    miceParTimes <- c(miceParTimes,
                      system.time(micemd_wrap(data = dat, 
                                              num_imp = m, 
                                              seed = seed, 
                                              num_cores = detectCores(), 
                                              backend = "PSOCK", 
                                              n.imp.core = m/nCores)))
    # foreach
    foreachTimes <- c(foreachTimes,
                      system.time(foreach_wrap(data = dat,
                                               num_imp = m,
                                               seed = seed,
                                               num_cores = nCores,
                                               backend = "PSOCK")))
    # parLapply
    parLapplyTimes <- c(parLapplyTimes,
                        system.time(parLapply_wrap(data = dat, 
                                                   num_imp = m,
                                                   seed = seed,
                                                   num_cores = nCores,
                                                   backend = "PSOCK")))
    # furrr
    furrrTimes <- c(furrrTimes,
                    system.time(furrr_wrap(data = dat, 
                                           num_imp = m,
                                           seed = seed,
                                           num_cores = nCores,
                                           backend = "PSOCK")))
    # future
    futureTimes <- c(futureTimes,
                     system.time(future_wrap(data = dat, 
                                             num_imp = m,
                                             seed = seed,
                                             num_cores = nCores,
                                             backend = "PSOCK")))
    print(paste0("Finished ", m, " imputations."))
  }
  
  # create vectors to store the elapsed time in and 
  # idx vector to extract the values needed from the list 
  runtimeMice <- numeric()
  runtimeParlmice <- numeric()
  runtimeMicePar <- numeric()
  runtimeForeach <- numeric()
  runtimeParlapply <- numeric()
  runtimeFurrr <- numeric()
  runTimeFuture <- numeric()
  idxs <- seq(3, 45, 5)
  
  # extract elapsed time from the list of values
  for (idx in idxs){
    runtimeMice <- c(runtimeMice, miceTimes[[idx]])
    runtimeParlmice <- c(runtimeParlmice, parlmiceTimes[[idx]])
    runtimeMicePar <- c(runtimeMicePar, miceParTimes[[idx]])
    runtimeForeach <- c(runtimeForeach, foreachTimes[[idx]])
    runtimeParlapply <- c(runtimeParlapply, parLapplyTimes[[idx]])
    runtimeFurrr <- c(runtimeFurrr, furrrTimes[[idx]])
    runTimeFuture <- c(runTimeFuture, futureTimes[[idx]])
  }
  
  # create df for time
  impTimes <- data.frame(
    Runtime = c(runtimeMice, runtimeParlmice, runtimeMicePar,
                runtimeForeach, runtimeParlapply, 
                runtimeFurrr, runTimeFuture),
    M = c(rep(imputations, 7)),
    Method = c(rep("serial", 9), rep("parlmice", 9), rep("mice.par", 9),
               rep("foreach", 9), rep("parLapply", 9),
               rep("furrr", 9), rep("future.apply", 9))
  )
  
  # create df for speedup
  impSpeedup <- data.frame(
    Speedup = c(runtimeMice/runtimeParlmice,
                runtimeMice/runtimeMicePar,
                runtimeMice/runtimeForeach,
                runtimeMice/runtimeParlapply,
                runtimeMice/runtimeFurrr,
                runtimeMice/runTimeFuture),
    M = c(rep(imputations, 6)),
    Method = c(rep("parlmice", 9), rep("mice.par", 9),
               rep("foreach", 9), rep("parLapply", 9),
               rep("furrr", 9), rep("future.apply", 9))
  )
  
  # store the dataframes in the lists
  dfListImp[[i]] <- impTimes
  dfListSpeed[[i]] <- impSpeedup
  
  print(paste0("Iteration ", i, " ended."))
}


# saving the dataframes
# save(impTimes, impSpeedup, file = "miTimeSpeedup.RData")

source("utils.R")

# plot m vs runtime
runtimePlot <- ggplot(data = impTimes, aes(x = M, y = Runtime, color=Method)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(detectCores(), seq(16, 128, 16))) +
  custom_color_map(test_mode = "runtime", os_test = FALSE)

speedupPlot <- ggplot(data = impSpeedup, aes(x = M, y = Speedup, color=Method)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(detectCores(), seq(16, 128, 16))) +
  custom_color_map(test_mode = "speed_up", os_test = FALSE)

# plot both next to each other
ggarrange(runtimePlot, speedupPlot, ncol = 2, nrow = 1)
