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

# Load up the data generator function.
source("src/dataGenerator.R")

# Returns the first n powers of 2.
power_of_two <- function(n) {
    return(rep(2, n) ^ (1:n))
}

# Estimate the parallelism of an algorithm.
# Following Amdahl's law, one can estimapte the percentage
# P of an algorithm which can be parallizied by only providing
# the speed up from one parallel run and the number of
# processes n.
estimate_parallelism <- function(speed_up, n) {
    return((1 / speed_up - 1) / (1 / n - 1))
}

# Wrapper function for parlmice(). This is mandatory, if you
# want to pass differently named arguments to parlmice form
# another scope. The reason for this is the internal handling
# of parlmice with argument lists, which are passed to the cluster.
parlmice_wrap <- function(data, m, cluster.seed, n.core, n.imp.core, cl.type) {
    result <- parlmice(data = data, m = m, cluster.seed = cluster.seed,
                    n.core = n.core, n.imp.core = n.imp.core, cl.type = cl.type,
                    maxit = 5)
    return(result)
}

# Wrapper function for a parallel call of mice using the foreach
# parallel loop.
foreach_wrap <- function(data, num_imp, seed, num_cores, backend) {
    cl <- makeCluster(num_cores, type = backend)
    clusterSetRNGStream(cl, seed)
    registerDoParallel(cl)

    result <- foreach(i = 1:num_imp, .combine = ibind, .packages = "mice") %dopar% {
        mice(data = data, m = 1, maxit = 5, printFlag = FALSE)
    }

    stopCluster(cl)
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
benmark_imputation <- function(data, num_imp, cores, runs = 5, os_test = FALSE) {
    serial_time <- numeric(length(cores))
    foreach_time <- numeric(length(cores))
    parlmice_time <- numeric(length(cores))
    foreach_time_fork <- numeric(length(cores))
    parlmice_time_fork <- numeric(length(cores))
    micemd_time <- numeric(length(cores))

    run_serial_time <- numeric(length(runs))
    run_foreach_time <- numeric(length(runs))
    run_parlmice_time <- numeric(length(runs))
    run_foreach_time_fork <- numeric(length(runs))
    run_parlmice_time_fork <- numeric(length(runs))
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
            # Perform parallel backend benchmark. This is only possible on
            # Linux or MacOS.
            if (os_test) {
                 # Parallel run with foreach.
                time_start_par <- Sys.time()
                result_foreach <- foreach_wrap(data = data,
                                        num_imp = num_imp, seed = seed,
                                        num_cores = num_cores, backend = "FORK")
                time_end_par <- Sys.time()
                time_taken_par <- difftime(time_end_par, time_start_par, units = "secs")
                run_foreach_time_fork[run] <- as.numeric(time_taken_par)
                print(paste0("Runtime in parallel execution (foreach [FORK]) is ", time_taken_par, " seconds."))

                assert_output(result_foreach, num_imp)

                # Parallel run with built-in mice function.
                time_start_par_mice <- Sys.time()
                result_parlmice <- parlmice_wrap(data = data, m = num_imp,
                                                cluster.seed = seed,
                                                n.core = num_cores,
                                                n.imp.core = ceiling(num_imp / num_cores),
                                                cl.type = "FORK")
                time_end_par_mice <- Sys.time()
                time_taken_par_mice <- difftime(time_end_par_mice, time_start_par_mice, units = "secs")
                run_parlmice_time_fork[run] <- as.numeric(time_taken_par_mice)
                print(paste0("Runtime in parallel execution (parlmice [FORK]) is ", time_taken_par_mice, " seconds."))

                assert_output(result_parlmice, num_imp)
            }

            # Parallel run with foreach.
            time_start_par <- Sys.time()
            result_foreach <- foreach_wrap(data = data,
                                    num_imp = num_imp, seed = seed,
                                    num_cores = num_cores, backend = "PSOCK")
            time_end_par <- Sys.time()
            time_taken_par <- difftime(time_end_par, time_start_par, units = "secs")
            run_foreach_time[run] <- as.numeric(time_taken_par)
            print(paste0("Runtime in parallel execution (foreach [PSOCK]) is ", time_taken_par, " seconds."))

            assert_output(result_foreach, num_imp)

            # Parallel run with built-in mice function.
            time_start_par_mice <- Sys.time()
            result_parlmice <- parlmice_wrap(data = data, m = num_imp,
                                            cluster.seed = seed,
                                            n.core = num_cores,
                                            n.imp.core = ceiling(num_imp / num_cores),
                                            cl.type = "PSOCK")
            time_end_par_mice <- Sys.time()
            time_taken_par_mice <- difftime(time_end_par_mice, time_start_par_mice, units = "secs")
            run_parlmice_time[run] <- as.numeric(time_taken_par_mice)
            print(paste0("Runtime in parallel execution (parlmice [PSOCK]) is ", time_taken_par_mice, " seconds."))

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

        if (os_test) {
            foreach_time_fork[i] <- mean(run_foreach_time_fork)
            parlmice_time_fork[i] <- mean(run_parlmice_time_fork)
        }
    }

    # Assemble data frame with average results and calculate speed ups
    # as well as the estimated parallelism.
    runtime_data <- data.frame(
        cores, serial_time,
        foreach_time, 
        foreach_time_fork,
        parlmice_time, 
        parlmice_time_fork,
        micemd_time,
        serial_time / foreach_time,
        serial_time / foreach_time_fork,
        serial_time / parlmice_time,
        serial_time / parlmice_time_fork,
        serial_time / micemd_time)

    colnames(runtime_data) <- c(
        "cores", "serial_time",
        "foreach_time", 
        "foreach_time_fork",
        "parlmice_time", 
        "parlmice_time_fork",
        "micemd_time",
        "foreach_speed_up", 
        "foreach_fork_speed_up",
        "parlmice_speed_up", 
        "parlmice_fork_speed_up",
        "micemd_speed_up"
    )

    runtime_data$est_par_foreach <- estimate_parallelism(
        runtime_data$foreach_speed_up,
        runtime_data$cores)
    runtime_data$est_par_foreach_fork <- estimate_parallelism(
        runtime_data$foreach_fork_speed_up,
        runtime_data$cores)
    runtime_data$est_par_parlmice <- estimate_parallelism(
        runtime_data$parlmice_speed_up,
        runtime_data$cores)
    runtime_data$est_par_parlmice_fork <- estimate_parallelism(
        runtime_data$parlmice_fork_speed_up,
        runtime_data$cores)
    runtime_data$est_par_micemd <- estimate_parallelism(
        runtime_data$micemd_speed_up,
        runtime_data$cores)

    return(runtime_data)
}

benmark_plot <- function(runtime_data, cores, value = "time", os_test = FALSE) {
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

        if (os_test) {
            plot <- plot +
                geom_point(aes(cores, foreach_time_fork, color = "foreach_fork")) +
                geom_point(aes(cores, parlmice_time_fork, color = "parlmice_fork")) +
                geom_line(aes(cores, foreach_time_fork, color = "foreach_fork")) +
                geom_line(aes(cores, parlmice_time_fork, color = "parlmice_fork"))
        }

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

        if (os_test) {
            plot <- plot +
                geom_point(aes(cores, foreach_fork_speed_up, color = "foreach_fork")) +
                geom_point(aes(cores, parlmice_fork_speed_up, color = "parlmice_fork")) +
                geom_line(aes(cores, foreach_fork_speed_up, color = "foreach_fork")) +
                geom_line(aes(cores, parlmice_fork_speed_up, color = "parlmice_fork"))
        }

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

# Check for the OS type of the machine and choose
# a suitiable benchmark run. On Linux and MacOS
# different parallel backends are compared.
os_name <- Sys.info()[["sysname"]]
if (os_name == "Windows") {
    runtime_data <- benmark_imputation(
        data = data, num_imp = 128,
        cores = cores, runs = 5,
        os_test = FALSE)
} else if (os_name %in% c("Darwin", "Linux")) {
    runtime_data <- benmark_imputation(
        data = data, num_imp = 128,
        cores = cores, runs = 5,
        os_test = TRUE)
} else {
    stop("Your OS is not supported by this benchmark.")
}

# Plot the results.
benmark_plot(runtime_data, cores, "time", os_test = TRUE)
benmark_plot(runtime_data, cores, "speed_up", os_test = TRUE)

ggarrange(
    benmark_plot(runtime_data, cores, "time"),
    benmark_plot(runtime_data, cores, "speed_up"),
    ncol = 2,
    nrow = 1)
