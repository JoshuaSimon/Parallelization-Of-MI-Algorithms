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


# Load helper functions.
source("src/utils.R")

# Load the parallel wrapper functions containing
# the impuation functions.
source("src/parallel_functions.R")

# Load up the data generator function.
source("src/dataGenerator.R")


# Runs different parallel MI algorithms and measures the run time.
# The run time is averaged over several benchmark runs.
benmark_imputation <- function(data, num_imp, cores, runs = 5, os_test = FALSE) {
    serial_time <- numeric(length(cores))
    foreach_time <- numeric(length(cores))
    parlmice_time <- numeric(length(cores))
    parlapply_time <- numeric(length(cores))
    foreach_time_fork <- numeric(length(cores))
    parlmice_time_fork <- numeric(length(cores))
    parlapply_time_fork <- numeric(length(cores))
    micemd_time <- numeric(length(cores))
    furrr_time <- numeric(length(cores))

    run_serial_time <- numeric(length(runs))
    run_foreach_time <- numeric(length(runs))
    run_parlmice_time <- numeric(length(runs))
    run_parlapply_time <- numeric(length(runs))
    run_foreach_time_fork <- numeric(length(runs))
    run_parlmice_time_fork <- numeric(length(runs))
    run_parlapply_time_fork <- numeric(length(runs))
    run_micemd_time <- numeric(length(runs))
    run_furrr_time <- numeric(length(runs))

    print(paste0(
        "Starting ", runs, " benchmark runs for M = ", num_imp,
        " multiple imputations.")
    )
    seed <- 42

    # Serial runs.
    for (run in 1:runs) {
        run_serial_time[run] <- mice_timer(
            fun = mice_wrap, fun_name = "mice",
            data = data, num_imp = num_imp,
            seed = seed, num_cores = num_cores,
            backend = NULL, exe_type = "serial", print_flag = TRUE)
    }

    serial_time <- rep(mean(run_serial_time), length(cores))

    # Parallel runs.
    for (i in 1:length(cores)) {
        num_cores <- cores[i]
        cat("\n")
        print(paste0(
            "Running M = ", num_imp,
            " multiple imputations on ", num_cores, " core(s).")
        )

        for (run in 1:runs) {
            # Perform parallel backend benchmark. This is only possible on
            # Linux or MacOS.
            if (os_test) {
                # Parallel run with foreach.
                run_foreach_time_fork[run] <- mice_timer(
                    #fun = foreach_wrap, fun_name = "foreach",
                    fun = foreach_wrap, fun_name = "foreach",
                    data = data, num_imp = num_imp,
                    seed = seed, num_cores = num_cores,
                    backend = "FORK", exe_type = "parallel", print_flag = TRUE)

                # Parallel run with built-in mice function.
                run_parlmice_time_fork[run] <- mice_timer(
                    fun = parlmice_wrap, fun_name = "parlmice",
                    data = data, num_imp = num_imp,
                    seed = seed, num_cores = num_cores,
                    backend = "FORK", exe_type = "parallel", print_flag = TRUE,
                    n.imp.core = ceiling(num_imp / num_cores))

                # Parallel run with parlapply function.
                run_parlapply_time_fork[run] <- mice_timer(
                    fun = parLapply_wrap, fun_name = "parLapply",
                    data = data, num_imp = num_imp,
                    seed = seed, num_cores = num_cores,
                    backend = "FORK", exe_type = "parallel", print_flag = TRUE)
            }

            # Parallel run with foreach.
            run_foreach_time[run] <- mice_timer(
                #fun = foreach_wrap, fun_name = "foreach",
                fun = foreach_wrap, fun_name = "foreach",
                data = data, num_imp = num_imp,
                seed = seed, num_cores = num_cores,
                backend = "PSOCK", exe_type = "parallel", print_flag = TRUE)

            # Parallel run with built-in mice function.
            run_parlmice_time[run] <- mice_timer(
                fun = parlmice_wrap, fun_name = "parlmice",
                data = data, num_imp = num_imp,
                seed = seed, num_cores = num_cores,
                backend = "PSOCK", exe_type = "parallel", print_flag = TRUE,
                n.imp.core = ceiling(num_imp / num_cores))

            # Parallel run with parlapply.
            run_parlapply_time[run] <- mice_timer(
                fun = parLapply_wrap, fun_name = "parLapply",
                data = data, num_imp = num_imp,
                seed = seed, num_cores = num_cores,
                backend = "PSOCK", exe_type = "parallel", print_flag = TRUE)

            # Parallel run with parallel mice function from micemd package.
            run_micemd_time[run] <- mice_timer(
                fun = micemd_wrap, fun_name = "mice.par",
                data = data, num_imp = num_imp,
                seed = seed, num_cores = num_cores,
                backend = NULL, exe_type = "parallel", print_flag = TRUE)

            # Parallel run with furrr.
            run_furrr_time[run] <- mice_timer(
                fun = furrr_wrap, fun_name = "furrr",
                data = data, num_imp = num_imp,
                seed = seed, num_cores = num_cores,
                backend = NULL, exe_type = "parallel", print_flag = TRUE)
        }

        # Calculate average run time per core.
        foreach_time[i] <- mean(run_foreach_time)
        parlmice_time[i] <- mean(run_parlmice_time)
        parlapply_time[i] <- mean(run_parlapply_time)
        micemd_time[i] <- mean(run_micemd_time)
        furrr_time[i] <- mean(run_furrr_time)

        if (os_test) {
            foreach_time_fork[i] <- mean(run_foreach_time_fork)
            parlmice_time_fork[i] <- mean(run_parlmice_time_fork)
            parlapply_time_fork[i] <- mean(run_parlapply_time_fork)
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
        parlapply_time,
        parlapply_time_fork,
        micemd_time,
        furrr_time,
        serial_time / foreach_time,
        serial_time / foreach_time_fork,
        serial_time / parlmice_time,
        serial_time / parlmice_time_fork,
        serial_time / parlapply_time,
        serial_time / parlapply_time_fork,
        serial_time / micemd_time,
        serial_time / furrr_time
    )

    colnames(runtime_data) <- c(
        "cores", "serial_time",
        "foreach_time",
        "foreach_time_fork",
        "parlmice_time",
        "parlmice_time_fork",
        "parlapply_time",
        "parlapply_time_fork",
        "micemd_time",
        "furrr_time",
        "foreach_speed_up",
        "foreach_fork_speed_up",
        "parlmice_speed_up",
        "parlmice_fork_speed_up",
        "parlapply_speed_up",
        "parlapply_fork_speed_up",
        "micemd_speed_up",
        "furrr_speed_up"
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
    runtime_data$est_par_parlapply <- estimate_parallelism(
        runtime_data$parlapply_speed_up,
        runtime_data$cores)
    runtime_data$est_par_parlapply_fork <- estimate_parallelism(
        runtime_data$parlapply_fork_speed_up,
        runtime_data$cores)
    runtime_data$est_par_micemd <- estimate_parallelism(
        runtime_data$micemd_speed_up,
        runtime_data$cores)
    runtime_data$est_par_furrr <- estimate_parallelism(
        runtime_data$furrr_speed_up,
        runtime_data$cores)

    return(runtime_data)
}


benmark_plot <- function(runtime_data, cores, value = "time", os_test = FALSE) {
    if (value == "time") {
        plot <- ggplot(data = runtime_data) +
            geom_point(aes(cores, foreach_time, color = "foreach")) +
            geom_point(aes(cores, parlmice_time, color = "parlmice")) +
            geom_point(aes(cores, parlapply_time, color = "parLapply")) +
            geom_point(aes(cores, micemd_time, color = "mice.par")) +
            geom_point(aes(cores, furrr_time, color = "furrr")) +
            geom_point(aes(cores, serial_time, color = "serial")) +
            geom_line(aes(cores, foreach_time, color = "foreach")) +
            geom_line(aes(cores, parlmice_time, color = "parlmice")) +
            geom_line(aes(cores, parlapply_time, color = "parLapply")) +
            geom_line(aes(cores, micemd_time, color = "mice.par")) +
            geom_line(aes(cores, furrr_time, color = "furrr")) +
            geom_line(aes(cores, serial_time, color = "serial"))

        if (os_test) {
            plot <- plot +
                geom_point(aes(cores, foreach_time_fork, color = "foreach_fork")) +
                geom_point(aes(cores, parlmice_time_fork, color = "parlmice_fork")) +
                geom_point(aes(cores, parlapply_time_fork, color = "parLapply_fork")) +
                geom_line(aes(cores, foreach_time_fork, color = "foreach_fork")) +
                geom_line(aes(cores, parlmice_time_fork, color = "parlmice_fork")) +
                geom_line(aes(cores, parlapply_time_fork, color = "parLapply_fork"))
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
            geom_point(aes(cores, parlapply_speed_up, color = "parLapply")) +
            geom_point(aes(cores, micemd_speed_up, color = "mice.par")) +
            geom_point(aes(cores, furrr_speed_up, color = "furrr")) +
            geom_line(aes(cores, foreach_speed_up, color = "foreach")) +
            geom_line(aes(cores, parlmice_speed_up, color = "parlmice")) +
            geom_line(aes(cores, parlapply_speed_up, color = "parLapply")) +
            geom_line(aes(cores, micemd_speed_up, color = "mice.par")) +
            geom_line(aes(cores, furrr_speed_up, color = "furrr"))

        if (os_test) {
            plot <- plot +
                geom_point(aes(cores, foreach_fork_speed_up, color = "foreach_fork")) +
                geom_point(aes(cores, parlmice_fork_speed_up, color = "parlmice_fork")) +
                geom_point(aes(cores, parlapply_fork_speed_up, color = "parLapply_fork")) +
                geom_line(aes(cores, foreach_fork_speed_up, color = "foreach_fork")) +
                geom_line(aes(cores, parlmice_fork_speed_up, color = "parlmice_fork")) +
                geom_line(aes(cores, parlapply_fork_speed_up, color = "parLapply_fork"))
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


main <- function() {
    # Run the benmark.
    num_cores <- detectCores()
    cores <- c(1, power_of_two(log(num_cores) / log(2)))

    data <- dataGenerator(n = 10000)

    # Check for the OS type of the machine and choose
    # a suitiable benchmark run. On Linux and MacOS
    # different parallel backends are compared.
    os_name <- Sys.info()[["sysname"]]
    if (os_name == "Windows") {
        os_test <- FALSE
        runtime_data <- benmark_imputation(
            data = data, num_imp = 128,
            cores = cores, runs = 5,
            os_test = os_test)
    } else if (os_name %in% c("Darwin", "Linux")) {
        os_test <- TRUE
        runtime_data <- benmark_imputation(
            data = data, num_imp = 128,
            cores = cores, runs = 1,
            os_test = os_test)
    } else {
        stop("Your OS is not supported by this benchmark.")
    }

    # Save the benchmark and session data as an .RData object.
    filename <- paste0("data/", Sys.Date(), "_benchmark_core_", os_name, ".RData")
    save(runtime_data, os_name, os_test, cores, file = filename)

    # Plot the results.
    plot_runtime <- benmark_plot(runtime_data, cores,
        "time", os_test = os_test)
    plot_speed_up <- benmark_plot(runtime_data, cores,
        "speed_up", os_test = os_test)

    plot_together <- ggarrange(
        plot_runtime,
        plot_speed_up,
        ncol = 2,
        nrow = 1)

    # Save the plots.
    filename <- paste0("img/", Sys.Date(), "_benchmark_core_", os_name, "_runtime.png")
    ggsave(filename = filename, plot = plot_runtime)

    filename <- paste0("img/", Sys.Date(), "_benchmark_core_", os_name, "_speed_up.png")
    ggsave(filename = filename, plot = plot_speed_up)

    filename <- paste0("img/", Sys.Date(), "_benchmark_core_", os_name, "_both.png")
    ggsave(filename = filename, plot = plot_together, width = 12, height = 5)
}


main()