# parallel_functions.R
# ------------------------------------------------ #
# This R script contains wrapper functions for the
# parallel calls of the mice function and a serial
# call.
# ------------------------------------------------ #

library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(furrr)
library(future.apply)


# Wrapper function for parlmice(). This is mandatory, if you
# want to pass differently named arguments to parlmice form
# another scope. The reason for this is the internal handling
# of parlmice with argument lists, which are passed to the cluster.
# See this for issue tracking:
# https://github.com/amices/mice/issues/189
parlmice_wrap <- function(data, m, cluster.seed, n.core, cl.type, n.imp.core) {
    result <- parlmice(data = data, m = m, cluster.seed = cluster.seed,
                    n.core = n.core, n.imp.core = n.imp.core, cl.type = cl.type,
                    maxit = 5)
    return(result)
}


# Implementation for a parallel mice call using the foreach
# parallel loop. Here, the loop size is matched to the number of CPU cores.
# The number of imputations is then equally distributed to the threads.
foreach_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
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
parLapply_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
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
# function. Using num_cores instead of num_imp in rep() and
# m = num_imp / num_cores speeds up the function significantly.
furrr_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
    plan(multisession, workers = num_cores)
    imps <- future_map(rep(1, num_cores), ~mice(data = data, m = num_imp / num_cores,
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


# Wrapper function for a parallel call of mice using the future.apply
# package and the future_lapply function.
future_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
  plan(multisession, workers = num_cores)
  imps <- future_lapply(X = 1:num_cores, function(None) {
    mice(data = data, m = (num_imp / num_cores), maxit = 5,
         seed = seed, printFlag = FALSE)
  })

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
micemd_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
    imps <- mice.par(don.na = data, m = num_imp,
                    maxit = 5, seed = seed, nnodes = num_cores)
    return(imps)
}


# Wrapper function for a standard call of mice using.
mice_wrap <- function(data, num_imp, seed, num_cores, backend, n.imp.core = NULL) {
    imps <- mice(data, m = num_imp, maxit = 5,
                printFlag = FALSE, seed = seed)
    return(imps)
}