# parallel_functions.R
# ------------------------------------------------ #
# This R script contains wrapper functions for the
# parallel calls of mice function.
# ------------------------------------------------ #

library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(furrr)


# Wrapper function for parlmice(). This is mandatory, if you
# want to pass differently named arguments to parlmice form
# another scope. The reason for this is the internal handling
# of parlmice with argument lists, which are passed to the cluster.
# See for the issue tracking:
# https://github.com/amices/mice/issues/189
parlmice_wrap <- function(data, m, cluster.seed, n.core, cl.type, n.imp.core) {
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


# Alternative implementation for a parallel mice call using the foreach
# parallel loop. Here the loop size is matched to the number of CPU cores.
# The number of imputations is then equally distributed to the threds.
foreach_wrap_alt <- function(data, num_imp, seed, num_cores, backend) {
    cl <- makeCluster(num_cores, type = backend)
    clusterSetRNGStream(cl, seed)
    registerDoParallel(cl)

    result <- foreach(i = 1:num_cores, .combine = ibind, .packages = "mice") %dopar% {
        mice(data = data, m = ceiling(num_imp / num_cores),
            maxit = 5, printFlag = FALSE)
    }

    stopCluster(cl)
    return(result)
}


# Wrapper function for a parallel call of mice using the parLapply
# function.
parLapply_wrap <- function(data, num_imp, seed, num_cores, backend) {
    cl <- makeCluster(num_cores, type = backend)
    clusterSetRNGStream(cl, seed)
    clusterExport(cl = cl,
                    varlist = c("data", "num_imp", "seed", "num_cores"),
                    envir = environment())
    clusterEvalQ(cl, library(mice))

    imps <- parLapply(cl, X = 1:num_cores, function(none) {
        mice(data = data, m = (num_imp / num_cores), maxit = 5,
            seed = seed, printFlag = FALSE)
    })

    stopCluster(cl)

    imp <- imps[[1]]
    if (length(imps) >= 2) {
        for (i in 2:length(imps)) {
            imp <- ibind(imp, imps[[i]])
        }
        return(imp)
    } else if (length(imps) == 1) {
       return(imp)
    }
}


# Wrapper function for a parallel call of mice using the furrr
# function.
furrr_wrap <- function(data, num_imp, seed, num_cores, backend) {
    plan(multisession, workers = num_cores)
    imps <- future_map(rep(1, num_imp), ~mice(data = data, m = 1,
                                        maxit = 5,
                                        seed = seed,
                                        printFlag = FALSE))
    imp <- imps[[1]]
    if (length(imps) >= 2) {
        for (i in 2:length(imps)) {
            imp <- ibind(imp, imps[[i]])
        }
        return(imp)
    } else if (length(imps) == 1) {
       return(imp)
    }
}


# Wrapper function for a parallel call of mice using the par.mice
# function.
micemd_wrap <- function(data, num_imp, seed, num_cores, backend) {
    imps <- mice.par(don.na = data, m = num_imp,
                    maxit = 5, seed = seed, nnodes = num_cores)
    return(imps)
}


# Wrapper function for a standard call of mice using.
mice_wrap <- function(data, num_imp, seed, num_cores, backend) {
    imps <- mice(data, m = num_imp, maxit = 5,
                printFlag = FALSE, seed = seed)
    return(imps)
}