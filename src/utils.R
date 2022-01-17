# utils.R
# ------------------------------------------------ #
# This R script contains helper functions for the
# benchmarks and analyses that are performed in this
# study.
# ------------------------------------------------ #

library(tidyverse)
library(testit)
library(xtable)


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


# Do some unit testing on a resulting mids object which is
# the output of mice and parallel mice calls.
assert_output <- function(mids_object, num_imp) {
    testit::assert(
        "Number of imputations did not match",
        mids_object$m == num_imp
    )
}


# Timer function to measure the runtime of mice calls from
# different mice wrapper functions. This function also
# assserts the output produced by the mice calls. The runtime
# in seconds is returned as a single value (method = "simple")
# or as a proc_time object containing user, system and elapsed
# time (method = "verbose").
mice_timer <- function(fun, fun_name, method = "simple", print_flag = TRUE,
    data, num_imp, seed, num_cores, backend, exe_type, ...) {

    if (method == "simple") {
        time_start <- Sys.time()
        imp_result <- fun(data, num_imp, seed, num_cores, backend, ...)
        time_end <- Sys.time()
        time_taken <- as.numeric(difftime(time_end, time_start, units = "secs"))
        time_print <- time_taken
    } else if (method == "verbose") {
        time_taken <- system.time(
           imp_result <- fun(data, num_imp, seed, num_cores, backend, ...)
        )
        time_print <- time_taken[3]
    }

    if (print_flag) {
        print(paste0(
            "Runtime in ", exe_type, " execution (", fun_name,
            " [", backend, "]) is ", time_print, " seconds.")
        )
    }

    assert_output(imp_result, num_imp)

    return(time_taken)
}


# Returns a custom color map for runtime and speed up plots.
# The return value can be added to a ggplot object.
custom_color_map <- function(test_mode = "runtime", os_test = FALSE) {
    # Define vector containing the custom color map.
    colourvec <- c("cyan2", "blue3", "darkorchid3", "deeppink2",
                "sienna2", "red2", "yellow1", "green1", "springgreen4")
    colourtable <- cbind(colourvec,
                        c("foreach", "parlmice", "parLapply", "mice.par",
                        "furrr", "serial", "foreach_fork", "parlmice_fork",
                        "parLapply_fork"))

    # Switch through different test modes with specifications of os_test,
    # that is, if the parallel backend is evaluated as well.
    if (test_mode == "runtime" & os_test == FALSE) {
        map <- scale_colour_manual("",
            breaks = c("foreach", "parlmice", "parLapply", "mice.par", "furrr", "serial"),
            values = c("foreach" = colourvec[1], "parlmice" = colourvec[2],
                        "parLapply" = colourvec[3], "mice.par" = colourvec[4],
                        "furrr" = colourvec[5], "serial" = colourvec[6])
        )
    } else if (test_mode == "runtime" & os_test == TRUE) {
        map <- scale_colour_manual("",
            breaks = c("foreach", "parlmice", "parLapply", "mice.par", "furrr",
                        "serial", "foreach_fork", "parlmice_fork", "parLapply_fork"),
            values = c("foreach" = colourvec[1], "parlmice" = colourvec[2],
                        "parLapply" = colourvec[3], "mice.par" = colourvec[4],
                        "furrr" = colourvec[5], "serial" = colourvec[6],
                        "foreach_fork" = colourvec[7], "parlmice_fork" = colourvec[8],
                        "parLapply_fork" = colourvec[9])
        )
    } else if (test_mode == "speed_up" & os_test == FALSE) {
        map <- scale_colour_manual("",
            breaks = c("foreach", "parlmice", "parLapply", "mice.par", "furrr"),
            values = c("foreach" = colourvec[1], "parlmice" = colourvec[2],
                        "parLapply" = colourvec[3], "mice.par" = colourvec[4],
                        "furrr" = colourvec[5])
        )
    } else if (test_mode == "speed_up" & os_test == TRUE) {
        map <- scale_colour_manual("",
            breaks = c("foreach", "parlmice", "parLapply", "mice.par", "furrr"
                        , "foreach_fork", "parlmice_fork", "parLapply_fork"),
            values = c("foreach" = colourvec[1], "parlmice" = colourvec[2],
                        "parLapply" = colourvec[3], "mice.par" = colourvec[4],
                        "furrr" = colourvec[5],
                        "foreach_fork" = colourvec[7], "parlmice_fork" = colourvec[8],
                        "parLapply_fork" = colourvec[9])
        )
    } else {
       stop("Custom color map: This combination of arguments is not valid.")
    }

    return(map)
}



# Create LaTeX code for a table representation of some data.
table_to_latex <- function(data, export_filename) {
    print(xtable(data, type = "latex"), file = export_filename)
}


# Tabulate runtime data from the core benchmark.
# The data can be provided as a data.frame object or
# as a string containing the file path to an .RData
# file which contains an object called "runtime_data".
tabulate_core_benchmark <- function(data) {
    if (class(data) == "character") {
        load(data)
    } else if (class(data) == "data.frame") {
        runtime_data <- data
    } else {
       stop("This data type is not supported.")
    }

    # Aggregate the data from each run by the average.
    runtime_data_group <- runtime_data %>%
        group_by(fun_name, cores) %>%
        summarize(
            avg_user_time = mean(user_time, na.rm = TRUE),
            avg_system_time = mean(system_time, na.rm = TRUE),
            avg_elapsed_time = mean(elapsed_time, na.rm = TRUE),
            avg_speed_up = mean(speed_up, na.rm = TRUE)) %>%
        filter(cores == max(runtime_data$cores))
    colnames(runtime_data_group) <- c("Function", "Cores", "Avg. user time",
                                    "Avg. system time", "Avg. elapsed time",
                                    "Avg. speed up")

    # Export table data to LaTex code.
    table_to_latex(
        data = runtime_data_group,
        export_filename = "poster/tables/table_benchmark_core.tex"
    )
}