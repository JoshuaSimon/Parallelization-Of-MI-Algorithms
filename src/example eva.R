#Parallelization of Multiple Imputation
#Packages
library("pacman") # packagemanager
p_load("mice", "car", "tidyverse", "foreach", "parallel", "doParallel",
       "iterators", "testit", "devtools", "usethis", "micemd")

#Data: ESS data 2004 on satisfaction in life 
load("C:\\Users\\evawo\\Documents\\OFU Survey Statistik\\WS 21 22\\Unvollständige Daten\\ESS04_data.RData")


dat <- ess[complete.cases(ess), ]
dat <- sapply(dat, as.numeric) %>% as.data.frame()

R <- c(1, 5, 10, 20, 30, 40, 60, 80, 100) #Multiple Imputations 
n <- NROW(dat)

# create missing values:
mcar1 <- sample(nrow(dat), round(0.3 * n))
mcar2 <- sample(nrow(dat), round(0.3 * n))

mar <- order(dat$tvtot + rnorm(n, 0, 0.5 * sd(dat$tvtot)),
             decreasing = TRUE)[1:(round(0.3 * n))]

is.na(dat$happy[mcar1]) <- TRUE
is.na(dat$netuse[mcar2]) <- TRUE
is.na(dat$vote[mar]) <- TRUE


### Multiple imputation and Analysis ###

# use all variables as predictors, but just consider linear relationships..
# initialization:
ini <- mice(data=dat, m = 1, maxit = 0)
pred <- ini$pred


##Parallelization

#Core Idenitification and Clustering
num_cores <- 7
cl <- makeCluster(num_cores)
clusterSetRNGStream(cl, 9956)
registerDoParallel(cl)

#Store imputation results and runtime
result <- resultp <- resultparl <- resultpar <-   list()
systp <- syst <- systparl <- systpar <- vector(length=length(R))

#Sequential and different parallelized MIs
for(i in 1:length(R)){
systp[i] <- system.time(resultp[[i]] <- foreach(no=1:R[i], .combine=ibind, .export = c("dat", "i"), .packages="mice") %dopar% {
 mice(data=dat,m = 1, predictorMatrix = pred, print = F)
 })[3] #run time of parallelized MI

syst[i] <- system.time(result[[i]] <- mice(data=dat,m = R[i], predictorMatrix = pred, print = F))[3]
#run time of sequential MI

systparl[i] <- system.time(resultparl[[i]] <- parlmice(data=dat, m=R[i], predictorMatrix = pred, print = F, cluster.seed = 123, n.core=7, n.imp.core = ceiling(R[i]/7)))[[3]]
#run time parlmice

systpar[i] <- system.time(resultpar[[i]] <- micemd::mice.par(don.na=dat, m=R[i], predictorMatrix = pred, print = F))[3]
#runtime parmice, not available on Windows?
}

#Runtime Visualization
X <- data.frame(R, systp, syst, systparl, systpar)
plot <- ggplot(data = X, aes(R, syst)) +
  geom_point(color = "red") +
  geom_point(aes(R, systp), color = "blue") +
  geom_point(aes(R, systparl), color = "green") +
  geom_point(aes(R, systpar), color = "orange") +
  geom_line(aes(R, syst, color = "sequential runtimes")) +
  geom_line(aes(R, systp, color = "parallel runtimes")) +
  geom_line(aes(R, systparl, color = "parlmice runtimes")) +
  geom_line(aes(R, systpar, color = "parmice runtimes")) +
  scale_colour_manual("", 
                      breaks = c("sequential runtimes", "parallel runtimes", "parlmice runtimes", "parmice runtimes"),
                      values = c("sequential runtimes"="red", "parallel runtimes"="blue", 
                                 "parlmice runtimes"="green", "parmice runtimes"="orange"))

plot <- plot + 
  ggtitle("Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plot

#Stop Clustering
stopCluster(cl)