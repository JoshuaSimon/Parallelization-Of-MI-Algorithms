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
miceParTimes <- list()
foreachTimes <- list()
parLapplyTimes <- list()
furrrTimes <- list()


for (i in imputations){
  
  print(paste0("Starting with ", i, " imputations."))
  
  
  # mice 
  miceTimes <- c(miceTimes,
                 system.time(mice_wrap(data = dat,
                                       num_imp = i, 
                                       seed = seed, 
                                       num_cores = detectCores(), 
                                       backend = "PSOCK")))
  
  # parlmice
  parlmiceTimes <- c(parlmiceTimes,
                     system.time(parlmice_wrap(data = dat,
                                               m = i,
                                               cluster.seed = seed,
                                               n.core = nCores,
                                               n.imp.core = i/nCores,
                                               cl.type = "PSOCK")))
  
  # micemd / mice.par
  miceParTimes <- c(miceParTimes,
                    system.time(micemd_wrap(data = dat, 
                                            num_imp = i, 
                                            seed = seed, 
                                            num_cores = detectCores(), 
                                            backend = "PSOCK", 
                                            n.imp.core = i/nCores)))
  
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
runtimeMicePar <- numeric()
runtimeForeach <- numeric()
runtimeParlapply <- numeric()
runtimeFurrr <- numeric()
idx <- seq(3, 45, 5)

for (i in idx){
  runtimeMice <- c(runtimeMice, miceTimes[[i]])
  runtimeParlmice <- c(runtimeParlmice, parlmiceTimes[[i]])
  runtimeMicePar <- c(runtimeMicePar, miceParTimes[[i]])
  runtimeForeach <- c(runtimeForeach, foreachTimes[[i]])
  runtimeParlapply <- c(runtimeParlapply, parLapplyTimes[[i]])
  runtimeFurrr <- c(runtimeFurrr, furrrTimes[[i]])
}

# create df for time
impTimes <- data.frame(
  Runtime = c(runtimeMice, runtimeParlmice, runtimeMicePar,
              runtimeForeach, runtimeParlapply, runtimeFurrr),
  M = c(rep(imputations, 6)),
  Method = c(rep("serial", 9), rep("parlmice", 9), rep("mice.par", 9),
             rep("foreach", 9), rep("parLapply", 9),
             rep("furrr", 9))
)

# create df for speedup
impSpeedup <- data.frame(
  Speedup = c(runtimeMice/runtimeParlmice,
              runtimeMice/runtimeMicePar,
              runtimeMice/runtimeForeach,
              runtimeMice/runtimeParlapply,
              runtimeMice/runtimeFurrr),
  M = c(rep(imputations, 5)),
  Method = c(rep("parlmice", 9), rep("mice.par", 9),
             rep("foreach", 9), rep("parLapply", 9),
             rep("furrr", 9))
)

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
