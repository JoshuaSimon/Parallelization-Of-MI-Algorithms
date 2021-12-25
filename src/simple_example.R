# simple_example.R
# ------------------------------------------------ #
# This script implements a simple demonstration of
# use of a parallel for loop with reproduceable 
# random numbers using a seed in each worker thread.
# For Unix systems the doMC backend is used. For 
# Windows system doParallel is used. For the sake
# of understanding, each namespace is explicitly 
# referenced in a function call. 
# ------------------------------------------------ #

library(parallel)
library(doMC)
library(doParallel)
library(testit)


#' A simple wordload to create some usage of the CPU.
#' 
#' @param i A integer value
#' @retrun A single random draw of a standard normal distribution.
workload <- function(i) {
    set.seed(i)
    Sys.sleep(1)
    return(rnorm(1))
}

#' A wrapper function, which performs a parallel execution of 
#' a passed function on the passed number of threads for the
#' passed number of iterations. 
#' 
#' @param func A fuction, which is suited for execution in a parallel for
#' loop and takes only one integer as input arguments
#' @param num_threads The number of threads, which will be created
#' @param max_iter The maximum number of parallel iterations
#' @return A vector with the results of \code{func}.
parallel_worker <- function(func, num_threads, max_iter) {
    # Register a parallel execution on your system. 
    # Depending on the OS, a different parallel backend is used.
    # This step sets everything up to create a number of R 
    # processes where the parallel fuction call is distributed to. 
    if (Sys.info()[["sysname"]] == "Windows") {
        cl <- parallel::makeCluster(num_threads)
        doParallel::registerDoParallel(cl)
    } else if (Sys.info()[["sysname"]] %in% c("Darwin", "Linux")) {
        doMC::registerDoMC(num_threads)
    }

    # Call a parallel foreach loop, that execute
    # the workload in parallel.
    result <- foreach::foreach(i=1:max_iter, .combine=c) %dopar% {
        func(i)
    }
        
    # Stop the parallel cluster after the execution has finished.
    if (Sys.info()[["sysname"]] == "Windows") {
        parallel::stopCluster(cl)
    }

    return(result)
}


# Get the number of cores available on your machine. 
num_cores <- parallel::detectCores()
print(paste0("Number of CPU cores on your machine: ", num_cores))

# Loop over the number of cores to get the specific runtimes
# and see the difference when using a different number of
# CPU cores. 
result <- list()
for (i in 1:num_cores) {
    start_time <- Sys.time()
    result[[i]] <- parallel_worker(func = workload,
                                    num_threads = i,
                                    max_iter = num_cores)
    end_time <- Sys.time()
    time_taken <- difftime(end_time, start_time, units = "secs")

    print(paste0("Runtime on ", i, " CPU core(s) is ", time_taken, " seconds."))
}

# Print the result of all test runs. Each list element is the
# result of one test run. So each of the list elements should
# be the same vector with a length equal to the number of CPU
# cores on your machine. 
#print(result)

# An automated way to proof this, is using asserts.
# If the condition in the assert clause is not true, 
# an console output will be raised. So if you see nothing,
# everthing is fine. 
for (i in 1:(length(result)-1)) {
    testit::assert("Vectors are eqaul", all.equal(result[[i]], result[[i+1]]))
}
