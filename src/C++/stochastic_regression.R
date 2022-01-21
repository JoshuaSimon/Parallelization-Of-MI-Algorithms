# stochastic_regression.R

library(MASS)
library(doParallel)


# Generate some simulated data with three variables
# X1, X2 and X3, where X3 contains some missing values.
generate_sim_data <- function(n = 1000, save_flag = FALSE) {
     set.seed(42)
     X1 <- rnorm(n, 8, 3)
     X2 <- 10 - 0.5 * X1 + rnorm(n, 0, 3)
     X3 <- 5 + 0.6 * X1 + 0.5 * X2 + rnorm(n, 0, sqrt(2))
     data1 <- as.data.frame(cbind(X1, X2, X3))
     BD.dat <- data1 # BD (before deletion)
     n <- nrow(BD.dat)

     if (save_flag) {
          write.csv(as.data.frame(cbind(X1, X2)), "src/C++/X_complete.csv",
               row.names = FALSE, col.names = FALSE)
          write.csv(as.data.frame(cbind(X3)), "src/C++/y_complete.csv",
               row.names = FALSE, col.names = FALSE)
     }

     # Create missing values.
     misind <- sample(1:n, round(n / 2))
     is.na(data1$X3[misind]) <- TRUE # moved this one, one line up
     obsind <- which(!is.na(data1$X3))

     if (save_flag) {
          write.csv(as.data.frame(cbind(X1[obsind], X2[obsind])),
               "src/C++/X_obs.csv",
               row.names = FALSE, col.names = FALSE)
          write.csv(as.data.frame(cbind(X3[obsind])),
               "src/C++/y_obs.csv",
               row.names = FALSE, col.names = FALSE)
          write.csv(as.data.frame(cbind(X1[misind], X2[misind])),
               "src/C++/X_miss.csv",
               row.names = FALSE, col.names = FALSE)
          write.csv(as.data.frame(cbind(X3[misind])),
               "src/C++/y_miss.csv",
               row.names = FALSE, col.names = FALSE)
     }
     return(data1)
}


stochastic_regression <- function(data, misind, obsind) {
     n_obs <- length(obsind)
     regmod <- lm(X3 ~ X1 + X2, data = data, subset = obsind)
     beta_hat <- coef(regmod)
     sigma_hat <- summary(regmod)$sigma
     design <- cbind(1, data$X1, data$X2)

     # Random draws for sigma^2 from the scaled inverse Chi^2-distribution
     sig.sq.tilde <- (n_obs - 3) * sigma_hat^2 / rchisq(1, n_obs - 3)

     # Random draw for beta from a mv. normal distribution
     beta_tilde <- mvrnorm(1, beta_hat,
                              solve(t(design[obsind, ]) %*%
                                   design[obsind, ]) * sig.sq.tilde)

     X3_tilde <- rnorm(nrow(design), design %*% beta_tilde, sqrt(sig.sq.tilde))
     return(X3_tilde[misind])
}


multiple_imputation <- function(data, num_imp) {
     for (m in 1:num_imp) {
          misind <- which(is.na(data$X3))
          obsind <- which(!is.na(data$X3))
          result <- c(data$X3[obsind],
               stochastic_regression(data, misind, obsind))
          imputations <- cbind(imputations, result)
     }
     return(imputations)
}


multiple_imputation_parallel <- function(data, num_imp) {
     num_cores <- detectCores()
     cl <- makeCluster(num_cores, type = "FORK")
     #clusterSetRNGStream(cl, 42)
     registerDoParallel(cl)
     result <- foreach(i = 1:num_cores, .combine = cbind) %dopar% {
          multiple_imputation(data, num_imp / num_cores)
     }
     stopCluster(cl)
     return(result)
}


# Set imputation parameters.
num_imp <- 10000
n <- 1000
data <- generate_sim_data(n, save_flag = FALSE)

# Perform multiple imputation using stochastic regression.
start <- Sys.time()
imp_serial <- multiple_imputation(data, num_imp)
end <- Sys.time()
time_taken <- difftime(end, start, units = "secs")
print(paste0("(Serial) Runtime: ", time_taken, " seconds."))

# Perform multiple imputation in paralell using stochastic regression.
start <- Sys.time()
imp_parallel <- multiple_imputation_parallel(data, num_imp)
end <- Sys.time()
time_taken <- difftime(end, start, units = "secs")
print(paste0("(Parallel) Runtime: ", time_taken, " seconds."))

# Print the mean of the imputations.
print(paste0("(Serial) mean: ", mean(as.matrix(imp_serial))))
print(paste0("(Parallel) mean: ", mean(as.matrix(imp_parallel))))
