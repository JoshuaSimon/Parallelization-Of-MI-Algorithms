dataGenerator <- function(n = 10000){
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
  
  # Creating variables x_1 to x_10
  x_1 <- rnorm(n, 5, 2)
  x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
  x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
  x_4 <- 5 - 0.1 * x_1 + 0.5 * x_2 - 0.2 * x_3 + rnorm(n, 0, 5)
  x_5 <- rnorm(n, 8, 4)
  x_6 <- 3 + 0.25 * x_5 + rnorm(n, 2, 3)
  x_7 <- 1.3 - 0.4 * x_5 + 0.2 * x_6 + rnorm(n, 1, 8)
  x_8 <- rnorm(n, 5, 10)
  x_9 <- 7 - 0.1 * x_8 + rnorm(n, 3, 5)
  x_10 <- 11 - 0.5 * x_8 - 0.25 * x_9 + rnorm(n, 2, 4)
  
  # Binding the variables into one dataframe
  dat <- data.frame(x_1, x_2, x_3, x_4, x_5, x_6, x_7,
                    x_8, x_9, x_10)

  # Create indices for missing values of the variables
  mis_x3 <- sample(1:n, 0.25 * n, replace = FALSE)
  mis_x4 <- sample(1:n, 0.4 * n, replace = FALSE)
  mis_x7 <- sample(1:n, 0.2 * n, replace = FALSE)
  mis_x9 <- sample(1:n, 0.15 * n, replace = FALSE)
  mis_x10 <- sample(1:n, 0.5 * n, replace = FALSE)
  
  # Introducting NAs / missing values into the dataframe
  dat[mis_x3, 3] <- NA
  dat[mis_x4, 4] <- NA
  dat[mis_x7, 7] <- NA
  dat[mis_x9, 9] <- NA
  dat[mis_x10, 10] <- NA
  
  # Return dataframe
  return(dat)
}
