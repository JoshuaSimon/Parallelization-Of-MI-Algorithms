# benchmark_core.R
# ------------------------------------------------ #
# Calculate multiple impuations for simulated data
# using the  mice package. Is is done on four ways:
# 1. Using mice serial
# 2. Using mice in parallel with foreach
# 3. Using the mice bulit-in function parlmice
# 4. Using the micemd paclage and mice.par
# The benchmark is performed on different number of
# CPU cores.
# ------------------------------------------------ #

library(tidyverse)
library(ggpubr)
library(testit)

library(mice)
library(micemd)
library(parallel)
library(doParallel)
#library(doMC)

# Load up the data generator function.
source("src/dataGenerator.R")

# Returns the first n powers of 2.
power_of_two <- function(n) {
    return(rep(2, n) ^ (1:n))
}

# Estimate the parallelism of an algorithm.
# Following Amdahl's law, one can estimapte the percentage
# P of an algorithm with can be parallizied by only providing
# the speed up from one parallel run and the number of
# processes n.
estimate_parallelism <- function(speed_up, n) {
    return((1 / speed_up - 1) / (1 / n - 1))
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
    testit::assert(
        "Number of imputations did not match",
        mids_object$m == num_imp
    )
}

# Runs different parallel MI algorithms and measures the run time.
# The run time is averaged over several benchmark runs.
benmark_imputation <- function(data, num_imp, cores, runs = 5) {
    serial_time <- numeric(length(cores))
    foreach_time <- numeric(length(cores))
    parlmice_time <- numeric(length(cores))
    micemd_time <- numeric(length(cores))

    run_serial_time <- numeric(length(runs))
    run_foreach_time <- numeric(length(runs))
    run_parlmice_time <- numeric(length(runs))
    run_micemd_time <- numeric(length(runs))

    print(paste0("Starting ", runs, " benchmark runs for M = ", num_imp, " multiple imputations."))
    seed <- 42

    # Serial runs.
    for (run in 1:runs) {
        time_start_seq <- Sys.time()
        result_seq <- mice(data, m = num_imp, maxit = 5,
                            printFlag = FALSE, seed = seed)
        time_end_seq <- Sys.time()
        time_taken_seq <- difftime(time_end_seq, time_start_seq, units = "secs")
        run_serial_time[run] <- as.numeric(time_taken_seq)
        print(paste0("Runtime in serial execution (mice) is ", time_taken_seq, " seconds."))

        assert_output(result_seq, num_imp)
    }

    serial_time <- rep(mean(run_serial_time), length(cores))

    # Parallel runs.
    for (i in 1:length(cores)) {
        num_cores <- cores[i]
        cat("\n")
        print(paste0("Running M = ", num_imp, " multiple imputations on ", num_cores, " core(s)."))

        for (run in 1:runs) {
            # Parallel run with foreach.
            time_start_par <- Sys.time()

            cl <- makeCluster(num_cores)
            clusterSetRNGStream(cl, seed)
            registerDoParallel(cl)

            result_foreach <- foreach(i=1:num_imp, .combine = ibind, .packages="mice") %dopar% {
                mice(data = data, m = 1, maxit = 5, printFlag = FALSE)
            }

            stopCluster(cl)

            time_end_par <- Sys.time()
            time_taken_par <- difftime(time_end_par, time_start_par, units = "secs")
            run_foreach_time[run] <- as.numeric(time_taken_par)
            print(paste0("Runtime in parallel execution (foreach) is ", time_taken_par, " seconds."))

            assert_output(result_foreach, num_imp)

            # Parallel run with built-in mice function.
            time_start_par_mice <- Sys.time()
            result_parlmice <- parlmice_wrap(data = data, m = num_imp,
                                            cluster.seed = seed,
                                            n.core = num_cores,
                                            n.imp.core = ceiling(num_imp / num_cores))
            time_end_par_mice <- Sys.time()
            time_taken_par_mice <- difftime(time_end_par_mice, time_start_par_mice, units = "secs")
            run_parlmice_time[run] <- as.numeric(time_taken_par_mice)
            print(paste0("Runtime in parallel execution (parlmice) is ", time_taken_par_mice, " seconds."))

            assert_output(result_parlmice, num_imp)

            # Parallel run with parallel mice function from micemd package.
            time_start_par_mice_md <- Sys.time()
            result_micepar <- mice.par(don.na = data, m = num_imp,
                                     maxit = 5, seed = seed, nnodes = num_cores)
            time_end_par_mice_md <- Sys.time()
            time_taken_par_mice_md <- difftime(time_end_par_mice_md, time_start_par_mice_md, units = "secs")
            run_micemd_time[run] <- as.numeric(time_taken_par_mice_md)
            print(paste0("Runtime in parallel execution (mice.par) is ", time_taken_par_mice_md, " seconds."))

            assert_output(result_micepar, num_imp)
        }

        # Calculate average run time per core.
        foreach_time[i] <- mean(run_foreach_time)
        parlmice_time[i] <- mean(run_parlmice_time)
        micemd_time[i] <- mean(run_micemd_time)
    }

    # Assemble data frame with average results and calculate speed ups
    # as well as the estimated parallelism.
    runtime_data <- data.frame(
        cores, serial_time,
        foreach_time, parlmice_time, micemd_time,
        serial_time / foreach_time,
        serial_time / parlmice_time,
        serial_time / micemd_time)

    colnames(runtime_data) <- c(
        "cores", "serial_time",
        "foreach_time", "parlmice_time", "micemd_time",
        "foreach_speed_up", "parlmice_speed_up", "micemd_speed_up")

    runtime_data$est_par_foreach <- estimate_parallelism(
        runtime_data$foreach_speed_up,
        runtime_data$cores)
    runtime_data$est_par_parlmice <- estimate_parallelism(
        runtime_data$parlmice_speed_up,
        runtime_data$cores)
    runtime_data$est_par_micemd <- estimate_parallelism(
        runtime_data$micemd_speed_up,
        runtime_data$cores)

    return(runtime_data)
}

benmark_plot <- function(runtime_data, cores, value = "time") {
    if (value == "time") {
        plot <- ggplot(data = runtime_data) +
            geom_point(aes(cores, foreach_time, color = "foreach")) +
            geom_point(aes(cores, parlmice_time, color = "parlmice")) +
            geom_point(aes(cores, micemd_time, color = "mice.par")) +
            geom_point(aes(cores, serial_time, color = "serial")) +
            geom_line(aes(cores, foreach_time, color = "foreach")) +
            geom_line(aes(cores, parlmice_time, color = "parlmice")) +
            geom_line(aes(cores, micemd_time, color = "mice.par")) +
            geom_line(aes(cores, serial_time, color = "serial"))

        plot <- plot +
            ggtitle("Runtime of Multiple Imputations") +
            scale_x_continuous(breaks = cores) +
            xlab("Number of CPU cores") + ylab("Runtime in seconds")
        return(plot)

    } else if (value == "speed_up") {
        plot <- ggplot(data = runtime_data) +
            geom_point(aes(cores, foreach_speed_up, color = "foreach")) +
            geom_point(aes(cores, parlmice_speed_up, color = "parlmice")) +
            geom_point(aes(cores, micemd_speed_up, color = "mice.par")) +
            geom_line(aes(cores, foreach_speed_up, color = "foreach")) +
            geom_line(aes(cores, parlmice_speed_up, color = "parlmice")) +
            geom_line(aes(cores, micemd_speed_up, color = "mice.par"))

        plot <- plot +
            ggtitle("Speed Up of Multiple Imputations Running in Parallel") +
            scale_x_continuous(breaks = cores) +
            xlab("Number of CPU cores") + ylab("Speed Up")
        return(plot)
    } else {
       stop("This value is not implementated.")
    }
}


# Run the benmark.
num_cores <- detectCores()
cores <- c(1, power_of_two(log(num_cores) / log(2)))

data <- dataGenerator(n = 10000)
runtime_data <- benmark_imputation(
    data = data, num_imp = 128,
    cores = cores, runs = 5)

# Plot the results.
ggarrange(
    benmark_plot(runtime_data, cores, "time"),
    benmark_plot(runtime_data, cores, "speed_up"),
    ncol = 2,
    nrow = 1)
