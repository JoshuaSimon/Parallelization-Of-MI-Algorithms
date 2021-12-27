# example_joshua.R
# ------------------------------------------------ #
# Calculate a row of multiple impuations using the 
# mice package. Is is done on three ways:
# 1. Using mice sequentially 
# 2. Using in mice in parallel with foreach
# 3. Using the mice bulit-in function parmice
# ------------------------------------------------ #

library(car)
library(tidyverse)

library(mice)
library(parallel)
library(doParallel)


data(SLID)
names(SLID_test) <- c("einkommen", "bildung", "alter", "geschlecht", "sprache")

max_imp <- 100
steps <- 10
imp_runs <- seq(0, max_imp, by = steps)
seq_runtimes <- numeric(length(imp_runs))
par_runtimes <- numeric(length(imp_runs))
par_mice_runtimes <- numeric(length(imp_runs))

# Offset the first iteration. 
imp_runs[1] <- 1

# Calculate multiple imputations for different counts of imputation
# runs M. First the imputations are calculated in a sequential manner
# and then in parallel.
for (i in 1:length(imp_runs)) {
  seed = 1
  set.seed(seed)
  
  num_imp <- imp_runs[i]
  print(paste0("Running M = ", num_imp, " multiple impuations..."))
  
  # Sequential run.
  time_start_seq <- Sys.time()
  imp_mice <- mice(SLID, m = num_imp, maxit = 2, printFlag = FALSE)
  time_end_seq <- Sys.time()
  time_taken_seq <- difftime(time_end_seq, time_start_seq, units = "secs")
  seq_runtimes[i] <- as.numeric(time_taken_seq)
  print(paste0("Runtime in sequential execution is ", time_taken_seq, " seconds."))
  
  
  # Parallel run.
  time_start_par <- Sys.time()
  
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  clusterSetRNGStream(cl, seed)
  registerDoParallel(cl)
  
  par_mice <- foreach(i=1:num_imp) %dopar% {
    library(mice)
    mice(data = SLID, m = 1, maxit = 2, printFlag = FALSE)
  }
  
  stopCluster(cl)
  
  time_end_par <- Sys.time()
  time_taken_par <- difftime(time_end_par, time_start_par, units = "secs")
  par_runtimes[i] <- as.numeric(time_taken_par)
  print(paste0("Runtime in parallel execution is ", time_taken_par, " seconds."))
  
  
  # Parallel run with built-in mice function.
  time_start_par_mice <- Sys.time()
  parlmice(data = SLID, m = num_imp, cluster.seed = seed)
  time_end_par_mice <- Sys.time()
  time_taken_par_mice <- difftime(time_end_par_mice, time_start_par_mice, units = "secs")
  par_mice_runtimes[i] <- as.numeric(time_taken_par_mice)
  print(paste0("Runtime in parallel execution is ", time_taken_par_mice, " seconds."))
}


# Display the results.
runtime_data <- data.frame(imp_runs, seq_runtimes, par_runtimes, par_mice_runtimes)
plot <- ggplot(data = runtime_data, aes(imp_runs, seq_runtimes)) +
  geom_point(color = "red") +
  geom_point(aes(imp_runs, par_runtimes), color = "blue") +
  geom_point(aes(imp_runs, par_mice_runtimes), color = "orange") +
  geom_line(aes(imp_runs, seq_runtimes, color = "sequential runtimes")) +
  geom_line(aes(imp_runs, par_runtimes, color = "parallel runtimes")) +
  geom_line(aes(imp_runs, par_mice_runtimes, color = "parmice runtimes")) +
  scale_colour_manual("", 
                      breaks = c("sequential runtimes", "parallel runtimes", "parmice runtimes"),
                      values = c("sequential runtimes"="red", "parallel runtimes"="blue", 
                                 "parmice runtimes"="orange"))

plot <- plot + 
  ggtitle("Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plot




