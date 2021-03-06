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
library(ggpubr)
library(testit)

library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(ggpubr)


# Returns the first n powers of 2.
power_of_two <- function(n) {
  return(rep(2, n) ^ (1:n))
}


# Making use of Sven's simulated data.
# Returns a dataframe containing n observations of
# simulated data.
generate_sim_data <- function(n, seed) {
  set.seed(seed) 
  # 3 variables
  x_1 <- rnorm(n, 5, 2)
  # x_2 depending on the values of x_1
  x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
  # x_3 depending on the values of x_1 und x_2
  x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)

  # Proportion of missing data for x_3
  p_mis <- 0.25

  dat <- data.frame(x_1, x_2, x_3)

  # generating NAs
  mis <- sample(1:n, p_mis * n, replace = FALSE)
  dat[mis, 3] <- NA

  sapply(dat, function(x) sum(is.na(x))) # 2500 NAs in x3; MCAR
  return(dat)
}


# Take the ESS04 data set and generate missing values within.
generate_ess04_data <- function(seed) {
  set.seed(seed) 

  load("data/ESS04_data.RData")
  dat <- ess[complete.cases(ess), ]
  dat <- sapply(dat, as.numeric) %>% as.data.frame()
  n <- NROW(dat)

  # Create missing values.
  mcar1 <- sample(nrow(dat), round(0.3 * n))
  mcar2 <- sample(nrow(dat), round(0.3 * n))
  mar <- order(dat$tvtot + rnorm(n, 0, 0.5 * sd(dat$tvtot)),
              decreasing = TRUE)[1:(round(0.3 * n))]

  is.na(dat$happy[mcar1]) <- TRUE
  is.na(dat$netuse[mcar2]) <- TRUE
  is.na(dat$vote[mar]) <- TRUE

  return(dat)
}


# Wrapper function for parlmice(). This is mandatory, if you
# want to pass differently named arguments to parlmice form
# another scope. The reason for this is the internal handling
# of parlmice with argument lists, which are passed to the cluster.
parlmice_wrap <- function(data, m, cluster.seed, n.core, n.imp.core) {
  result <- parlmice(data = data, m = m, cluster.seed = cluster.seed,
            n.core = n.core, n.imp.core = n.imp.core, maxit = 5)
  return(result)
}


# Do some unit test on a resulting mids object which is
# the output of mice and parallel mice calls.
assert_output <- function(mids_object, num_imp) {
  assert("Number of imputations did not match", mids_object$m == num_imp)
}


# Calculate multiple imputations for different counts of imputation
# runs M. First the imputations are calculated in a sequential manner
# and then in parallel.
# Returns a dataframe containing all the run times of the different
# parallel MI algorithms.
benmark_imputations <- function(data, imp_runs) {
  seq_runtimes <- numeric(length(imp_runs))
  par_runtimes <- numeric(length(imp_runs))
  par_mice_runtimes <- numeric(length(imp_runs))
  par_mice_runtimes_md <- numeric(length(imp_runs))

  for (i in 1:length(imp_runs)) {
    seed = 42
    set.seed(seed)

    num_imp <- imp_runs[i]
    print(paste0("Running M = ", num_imp, " multiple impuations..."))

    # Sequential run.
    time_start_seq <- Sys.time()
    result_seq <- mice(data, m = num_imp, maxit = 5, printFlag = FALSE)
    time_end_seq <- Sys.time()
    time_taken_seq <- difftime(time_end_seq, time_start_seq, units = "secs")
    seq_runtimes[i] <- as.numeric(time_taken_seq)
    print(paste0("Runtime in sequential execution (mice) is ", time_taken_seq, " seconds."))

    assert_output(result_seq, num_imp)
  
    # Parallel run with foreach.
    time_start_par <- Sys.time()
  
    num_cores <- detectCores()
    cl <- makeCluster(num_cores)
    clusterSetRNGStream(cl, seed)
    registerDoParallel(cl)
  
    result_foreach <- foreach(i=1:num_imp, .combine = ibind, .packages="mice") %dopar% {
      mice(data = data, m = 1, maxit = 5, printFlag = FALSE)
    }
  
    stopCluster(cl)
  
    time_end_par <- Sys.time()
    time_taken_par <- difftime(time_end_par, time_start_par, units = "secs")
    par_runtimes[i] <- as.numeric(time_taken_par)
    print(paste0("Runtime in parallel execution (foreach) is ", time_taken_par, " seconds."))
    
    assert_output(result_foreach, num_imp)
    
    # Parallel run with built-in mice function.
    time_start_par_mice <- Sys.time()
    result_parlmice <- parlmice_wrap(data = data, m = num_imp, 
                                    cluster.seed = seed,
                                    n.core = num_cores, 
                                    n.imp.core = ceiling(num_imp/num_cores))
    time_end_par_mice <- Sys.time()
    time_taken_par_mice <- difftime(time_end_par_mice, time_start_par_mice, units = "secs")
    par_mice_runtimes[i] <- as.numeric(time_taken_par_mice)
    print(paste0("Runtime in parallel execution (parlmice) is ", time_taken_par_mice, " seconds."))
    
    assert_output(result_parlmice, num_imp)

    # Parallel run with parallel mice function from micemd package.
    time_start_par_mice_md <- Sys.time()
    result_micepar <- mice.par(don.na = data, m = num_imp, maxit = 5, seed = seed, nnodes = num_cores)
    time_end_par_mice_md <- Sys.time()
    time_taken_par_mice_md <- difftime(time_end_par_mice_md, time_start_par_mice_md, units = "secs")
    par_mice_runtimes_md[i] <- as.numeric(time_taken_par_mice_md)
    print(paste0("Runtime in parallel execution (mice.par) is ", time_taken_par_mice_md, " seconds."))
    
    assert_output(result_micepar, num_imp)
  }

  runtime_data <- data.frame(imp_runs, seq_runtimes, par_runtimes, par_mice_runtimes, par_mice_runtimes_md)
  return(runtime_data)
}


# Create a plot displaying all the run times as curves
# for the different MI implementations. 
# Returns a ggplot object.
benmark_plot <- function(runtime_data, imp_runs) {
  plot <- ggplot(data = runtime_data, aes(imp_runs, seq_runtimes)) +
    geom_point(color = "red") +
    geom_point(aes(imp_runs, par_runtimes), color = "blue") +
    geom_point(aes(imp_runs, par_mice_runtimes), color = "orange") +
    geom_point(aes(imp_runs, par_mice_runtimes_md), color = "darkgreen") +
    geom_line(aes(imp_runs, seq_runtimes, color = "sequential runtimes")) +
    geom_line(aes(imp_runs, par_runtimes, color = "foreach runtimes")) +
    geom_line(aes(imp_runs, par_mice_runtimes, color = "parlmice runtimes")) +
    geom_line(aes(imp_runs, par_mice_runtimes_md, color = "mice.par runtimes")) +
    scale_colour_manual("", 
                        breaks = c("sequential runtimes", "foreach runtimes",
                                   "parlmice runtimes", "mice.par runtimes"),
                        values = c("sequential runtimes"="red", "foreach runtimes"="blue", 
                                  "parlmice runtimes"="orange", "mice.par runtimes"="darkgreen"))

  plot <- plot +
    ggtitle("Runtime of Multiple Imputations") +
    scale_x_continuous(breaks = imp_runs) +
    xlab("Number of multiple imputations M") + ylab("Runtime in seconds")
  
  return(plot)
}


benmark_run <- function(test_data, test_mode = 1) {
  # Choose the number of imputations which are
  # calculated in the benchmark.
  if (test_mode == 1) {
    max_imp <- 100
    steps <- 16
    imp_runs <- seq(16, max_imp, by = steps)

    # Offset the first iteration.
    #imp_runs[1] <- 1    
  } else if (test_mode == 2) {
     imp_runs <- power_of_two(7)
     imp_runs <- c(1, imp_runs)
  } else {
    stop("This is not implemented.")
  }

  # Run the benchmark on all test data sets. The plot results
  # are saved. 
  plots <- vector(mode = "list", length = length(test_data))
  for (i in 1:length(test_data)) {
     plots[[i]] <- benmark_imputations(data = test_data[[i]], imp_runs = imp_runs) %>%
      benmark_plot(runtime_data = ., imp_runs = imp_runs)
  }
  
  return(plots)
}


# Load up some test data.
data(SLID)
names(SLID) <- c("einkommen", "bildung", "alter", "geschlecht", "sprache")

test_data <- list(SLID)
plots <- benmark_run(test_mode = 1, test_data)
plots[[1]]


test_data <- list(generate_ess04_data(42))
plots <- benmark_run(test_mode = 1, test_data)
plots[[1]]


# Test the impact of different data sizes.
test_data <- list(generate_sim_data(n = 100, seed = 42),
                 generate_sim_data(n = 1000, seed = 42),
                 generate_sim_data(n = 10000, seed = 42),
                 generate_sim_data(n = 100000, seed = 42))
plots <- benmark_run(test_mode = 1, test_data)

ggarrange(plots[[1]],
          plots[[2]],
          plots[[3]],
          plots[[4]],
          ncol = 2,
          nrow = 2)

imp_runs <- c(1, 7, 14, 28, 49, 70, 98)

l <- colnames(dat) %>% as.character()
names(dat) <- l
test <- list(dat)

plots <- benmark_run(test_data= test, test_mode = 1)

