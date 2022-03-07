dataGenerator <- function(n = 10000, nvar = 1000) {
  # Function to generate the same dataframe with n
  # observations.
  # The dataframe contains 10 variables, some of which
  # are dependent on other variables and some of which are
  # independent. After creating the variables and binding
  # them into a dataframe, missing values are introduced into
  # five of the variables (x_3, x_4, x_7, x_9 and x_10)
  # and the dataframe is returned.

  # Setting seed
  seed <- 42
  set.seed(seed)

  dat <- matrix(nrow = n, ncol = nvar)
  for (i in seq(1, nvar - 3, 3)) {
  # Creating variables x_1 to x_1000
  dat[, i] <- rnorm(n, 5, 2)
  dat[, (i + 1)] <- 5 - 0.7 * dat[, i] + rnorm(n, 0, 4)
  dat[, (i + 2)] <- 5 + 0.5 * dat[, i] + 0.3 * dat[, (i + 1)] + rnorm(n, 0, 2)
  }

  #Create data.frame
  dat <- as.data.frame(dat)
  colnames(dat) <- paste0("x_", 1:nvar)

  # Introducting NAs / missing values into the dataframe
  for (i in 1:nvar) {
    mis <- sample(1:n, sample(c(0.25, 0.4, 0.2, 0.15, 0.5), 1) * n,
                  replace = FALSE)
    dat[mis, i] <- NA
  }

  # Return dataframe
  return(dat)
}
