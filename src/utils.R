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