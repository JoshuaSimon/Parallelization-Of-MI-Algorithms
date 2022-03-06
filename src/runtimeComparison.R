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


source("parallel_functions.R")

# mice
miceTime <- system.time(mice(data = dat, 
                             m = 48, 
                             maxit = 5, 
                             printFlag = FALSE, 
                             seed = seed))

# foreach
feTime <- system.time(foreach_wrap(data = dat, 
                                       num_imp = 48, 
                                       seed = seed, 
                                       num_cores = detectCores(), 
                                       backend = "PSOCK"))

# parlapply
parLTime <- system.time(parLapply_wrap(data = dat,
                                       num_imp = 48,
                                       num_cores = detectCores(),
                                       seed = seed, 
                                       backend = "PSOCK"))

# furrr
furrrTime <- system.time(furrr_wrap(data = dat, 
                                        num_imp = 48, 
                                        num_cores = detectCores(), 
                                        seed = seed, 
                                        backend = "PSOCK"))


# checking integrity of imputations
miceImputation <- complete(mice(data = dat, 
                                m = 48, 
                                maxit = 5, 
                                printFlag = FALSE, 
                                seed = seed))
feImputation <- complete(foreach_wrap_alt(data = dat, 
                                          num_imp = 48, 
                                          seed = seed, 
                                          num_cores = detectCores(), 
                                          backend = "PSOCK"))
parLImputation <- complete(parLapply_wrap(data = dat,
                                          num_imp = 48,
                                          num_cores = detectCores(),
                                          seed = seed, 
                                          backend = "PSOCK"))
furrrImputation <- complete(furrr_wrap_alt(data = dat, 
                                           num_imp = 48, 
                                           num_cores = detectCores(), 
                                           seed = seed, 
                                           backend = "PSOCK"))

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
