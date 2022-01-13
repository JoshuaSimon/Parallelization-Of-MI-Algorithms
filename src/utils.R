# utils.R
# ------------------------------------------------ #
# This R script contains helper functions for the
# benchmarks and analysis that is performed in this
# study.
# ------------------------------------------------ #

library(testit)


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
# in seconds is returned as a single value.
mice_timer <- function(fun, fun_name, data, num_imp, seed, num_cores, backend, exe_type, print_flag = TRUE, ...) {
    time_start <- Sys.time()
    imp_result <- fun(data, num_imp, seed, num_cores, backend, ...)
    time_end <- Sys.time()
    time_taken <- difftime(time_end, time_start, units = "secs")

    if (print_flag) {
        print(paste0(
            "Runtime in ", exe_type, " execution (", fun_name,
            " [", backend, "]) is ", time_taken, " seconds.")
        )
    }

    assert_output(imp_result, num_imp)

    return(as.numeric(time_taken))
}