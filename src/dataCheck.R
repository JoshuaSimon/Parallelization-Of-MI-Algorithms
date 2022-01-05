library(mice)
library(micemd)
library(parallel)
library(doParallel)
library(tidyverse)
library(foreach)

dataGenerator <- function(n = 10000, p_mis = 0.25){
  seed <- 42
  set.seed(seed)
  
  # 3 variables
  x_1 <- rnorm(n, 5, 2)
  # x_2 depending on the values of x_1
  x_2 <- 5 - 0.7 * x_1 + rnorm(n, 0, 4)
  # x_3 depending on the values of x_1 und x_2
  x_3 <- 5 + 0.5 * x_1 + 0.3 * x_2 + rnorm(n, 0, 2)
  
  dat <- data.frame(x_1, x_2, x_3)
  
  # generating NAs
  mis <- sample(1:n, p_mis * n, replace = FALSE)
  dat[mis, 3] <- NA
  
  return(dat)
  
}

seed <- 42
set.seed(seed)
nCores <- detectCores()
dat <- dataGenerator() # 10.000 observations / x_3 25% NA


# regular mice
impMice <- mice(dat, m = 16, maxit = 5, seed = seed,
                printFlag = FALSE)

# built in parlmice function
impParlmice <- parlmice(dat, m = 16, maxit = 5, 
                        cluster.seed = seed, 
                        n.core = nCores,
                        n.imp.core = (16/nCores))

# mice.par function from the micemd package
impMicePar <- mice.par(don.na = dat, m = 16, maxit = 5,
                       seed = seed, printFlag = FALSE)


# foreach using .combine = ibind to combine into one mids obj
cluster <- makeCluster(nCores)
clusterSetRNGStream(cluster, seed)
registerDoParallel(cluster)
impForeach <- foreach(1:16, .combine = ibind) %dopar% {
  library(mice)
  mice(dat, m = 1, maxit = 5, seed = seed,
       printFlag = FALSE)
}
stopCluster(cluster)


# Plots of the different methods

plot(impMice) # 16 chains / 5 iterations each
plot(impParlmice) # 16 chains / 5 iterations each
plot(impMicePar) # 5 chains / 5 iterations each
plot(impForeach) # 1 chain / 5 iterations; maybe 
                 # due to the mids objects being combined?

# comparing linear regressions

# regular mice
thetaMice <- with(impMice, lm(x_1 ~ x_2 + x_3))
summary(pool(thetaMice))

# built in parlmice function
thetaParlmice <- with(impParlmice, lm(x_1 ~ x_2 + x_3))
summary(pool(thetaParlmice))

# mice.par function from the micemd package
thetaMicePar <- with(impMicePar, lm(x_1 ~ x_2 + x_3))
summary(pool(thetaMicePar))

# foreach
thetaForeach <- with(impForeach, lm(x_1 ~ x_2 + x_3))
summary(pool(thetaForeach))

# estimates / se of all methods are very similar
# statistics differ a bit; df vary widely
# ==> the methods seem to work correctly

# comparing summary statistics

# regular mice
miceComplete <- complete(impMice)
summary(miceComplete)

# built in parlmice function
parlmiceComplete <- complete(impParlmice)
summary(parlmiceComplete)

# mice.par function from the micemd package
miceParComplete <- complete(impMicePar)
summary(miceParComplete)

# foreach
foreachComplete <- complete(impForeach)
summary(foreachComplete)

# graphical comparison of x_3
ggplot() + 
  geom_density(aes(x = miceComplete$x_3, color = "mice")) +
  geom_density(aes(x = parlmiceComplete$x_3, color = "parlmice")) +
  geom_density(aes(x = miceParComplete$x_3, color = "mice.par")) +
  geom_density(aes(x = foreachComplete$x_3, color = "foreach")) +
  xlab("x_3") + ylab("Density") + 
  ggtitle("Comparing Densities of imputed variable x_3")

# all very similar ==> all methods seem to work correctly