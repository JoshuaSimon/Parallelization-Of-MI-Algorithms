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
benmark_imputation <- function(data, num_imp, cores, runs = 5,
  timer_method = "simple", os_test = FALSE) {
    seed <- 42
    print(paste0(
        "Starting ", runs, " benchmark runs for M = ", num_imp,
        " multiple imputations.")
    )

    # Set up lists/vectors with functions and individual parameters for
    # benchmark executions.
    fun_calls <- list(mice_wrap, foreach_wrap, parlmice_wrap, parLapply_wrap, micemd_wrap, furrr_wrap)
    fun_names <- c("serial", "foreach", "parlmice", "parLapply", "mice.par", "furrr")
    exe_types <- c("serial", "parallel", "parallel", "parallel", "parallel", "parallel")
    backends <- list("", "PSOCK", "PSOCK", "PSOCK", "", "")

    # Perform parallel backend benchmark. This is only possible on
    # Linux or MacOS. Therefore the list of functions is extendend
    # by the FORK calls.
    if (os_test) {
        fun_calls <- c(fun_calls, foreach_wrap, parlmice_wrap, parLapply_wrap)
        fun_names <- c(fun_names, "foreach_fork", "parlmice_fork", "parLapply_fork")
        exe_types <- c(exe_types, "parallel", "parallel", "parallel")
        backends <- c(backends, "FORK", "FORK", "FORK")
    }

    # Setup empty data.frame to save the benchmark result data.
    runtime_data <- data.frame(fun_name = character(),
                                backend = character(),
                                run = integer(),
                                cores = integer(),
                                imputations = integer(),
                                user_time = double(),
                                system_time = double(),
                                elapsed_time = double(),
                                speed_up = double(),
                                parallelism = double(),
                                stringsAsFactors = FALSE)

    # Iterate over benchmark runs and core combinations
    # and perform the mice imputations.
    for (run in 1:runs) {
        cat("\n")
        print(paste0("-------- Run ", run, " --------"))
        for (i in 1:length(cores)) {
            num_cores <- cores[i]
            cat("\n")
            print(paste0(
                "Running M = ", num_imp,
                " multiple imputations on ", num_cores, " core(s).")
            )

            # Measure time for every imputation method.
            for (i in 1:length(fun_calls)) {
                time <- mice_timer(
                    fun = fun_calls[[i]], fun_name = fun_names[i],
                    method = timer_method, print_flag = TRUE,
                    data = data, num_imp = num_imp,
                    seed = seed, num_cores = num_cores,
                    backend = backends[[i]], exe_type = exe_types[i],
                    n.imp.core = ceiling(num_imp / num_cores))

                # Add test results as a new row to the data.frame.
                if (timer_method == "simple") {
                    new_row <- c(fun_names[i], backends[[i]],
                        run, num_cores, num_imp, NA, NA, time, NA, NA)
                } else if (timer_method == "verbose") {
                    new_row <- c(fun_names[i], backends[[i]],
                        run, num_cores, num_imp,
                        time[[1]], time[[2]], time[[3]], NA, NA)
                }
                runtime_data[nrow(runtime_data) + 1, ] <- new_row
            }
        }
    }

    # Cast columns in dataframe to the correct data type.
    # Because of adding new rows with c(), everything is
    # of tyoe character.
    runtime_data$run <- as.integer(runtime_data$run)
    runtime_data$cores <- as.integer(runtime_data$cores)
    runtime_data$imputations <- as.integer()(runtime_data$imputations)
    runtime_data$user_time <- as.double(runtime_data$user_time)
    runtime_data$system_time <- as.double(runtime_data$system_time)
    runtime_data$elapsed_time <- as.double(runtime_data$elapsed_time)
    runtime_data$speed_up <- as.double(runtime_data$speed_up)
    runtime_data$parallelism <- as.double(runtime_data$parallelism)

    # Calculate speed up and estimated parallelism.
    serial_time <- mean(runtime_data$elapsed_time[runtime_data$fun_name == "serial"])
    runtime_data$speed_up[runtime_data$fun_name != "serial"] <- serial_time /
        runtime_data$elapsed_time[runtime_data$fun_name != "serial"]
    runtime_data$parallelism[runtime_data$fun_name != "serial"] <- estimate_parallelism(
        runtime_data$speed_up[runtime_data$fun_name != "serial"],
        runtime_data$cores[runtime_data$fun_name != "serial"])

    return(runtime_data)
}
#result <- benmark_imputation(data, 128, cores, runs = 1, timer_method = "simple", os_test = TRUE)
#result <- benmark_imputation(data, 16, cores, runs = 1, timer_method = "verbose", os_test = TRUE)


# Creates different plots of the average of the benchmark
# results. The plots differ between runtime and speed up.
benmark_plot <- function(runtime_data, cores, test_mode = "runtime", os_test = FALSE) {
    # Aggregate data from muliple benchmark runs.
    runtime_data_group <- runtime_data %>%
        group_by(fun_name, cores) %>%
        summarize(avg_elapsed_time = mean(elapsed_time),
                avg_speed_up = mean(speed_up))

    print(runtime_data_group)

    if (test_mode == "runtime") {
        plot <- ggplot(data = runtime_data_group,
                        aes(x = cores, y = avg_elapsed_time, color = fun_name))
        title <- "Runtime of Multiple Imputations"
        label <- "Runtime in seconds"
    } else if (test_mode == "speed_up") {
        plot <- ggplot(data = runtime_data_group %>%
                        filter(fun_name != "serial"),
                        aes(x = cores, y = avg_speed_up, color = fun_name))
        title <- "Speed Up of Multiple Imputations Running in Parallel"
        label <- "Speed Up"
    } else {
       stop("This kind of plot is not implementated.")
    }

    plot <- plot +  geom_line() + geom_point() +
        custom_color_map(test_mode = test_mode, os_test = os_test) +
        ggtitle(title) +
        scale_x_continuous(breaks = cores) +
        xlab("Number of CPU cores") + ylab(label)

    return(plot)
}
#benmark_plot(result, cores, test_mode = "runtime", os_test = TRUE)
#benmark_plot(result, cores, test_mode = "speed_up", os_test = TRUE)


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
            timer_method = "simple",
            os_test = os_test)
    } else if (os_name %in% c("Darwin", "Linux")) {
        os_test <- TRUE
        runtime_data <- benmark_imputation(
            data = data, num_imp = 16,
            cores = cores, runs = 2,
            timer_method = "verbose",
            os_test = os_test)
    } else {
        stop("Your OS is not supported by this benchmark.")
    }

    # Save the benchmark and session data as an .RData object.
    filename <- paste0("data/", Sys.Date(), "_benchmark_core_", os_name, ".RData")
    save(runtime_data, os_name, os_test, cores, file = filename)

    # Plot the results.
    plot_runtime <- benmark_plot(runtime_data, cores,
        test_mode = "runtime", os_test = os_test)
    plot_speed_up <- benmark_plot(runtime_data, cores,
        test_mode = "speed_up", os_test = os_test)

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