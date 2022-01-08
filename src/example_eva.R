#Parallelization of Multiple Imputation
#Packages
library("pacman") # packagemanager
p_load("mice", "car", "tidyverse", "foreach", "parallel", "doParallel",
       "iterators", "testit", "devtools", "usethis", "micemd")

#Data: ESS data 2004 on satisfaction in life 
source("src/dataGenerator.R")

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
systp <- syst <- systparl <- systpar <- matrix(nrow = length(R), ncol=3)
R<- 1
#Sequential and different parallelized MIs
for(i in 1:length(R)){
systp[i,] <- system.time(resultp[[i]] <- foreach(no=1:R[i], .combine=ibind, .export = c("dat", "i"), .packages="mice") %dopar% {
 mice(data=dat,m = 1, predictorMatrix = pred, print = F)
 }) [1:3] #run time of parallelized MI

syst[i,] <- system.time(result[[i]] <- mice(data=dat,m = R[i], predictorMatrix = pred, print = F))[1:3]
#run time of sequential MI

systparl[i,] <- system.time(resultparl[[i]] <- parlmice(data=dat, m=R[i], predictorMatrix = pred, print = F, cluster.seed = 123, n.core=7, n.imp.core = ceiling(R[i]/7)))[1:3]
#run time parlmice

systpar[i,] <- system.time(resultpar[[i]] <- micemd::mice.par(don.na=dat, m=R[i], predictorMatrix = pred, print = F))[1:3]
#runtime micemd::mice.par()
}

#Runtime Visualization
Xuser<- data.frame(R, systp[,1], syst[,1], systparl[,1], systpar[,1])
Xsystem <- data.frame(R, systp[,2], syst[,2], systparl[,2], systpar[,2])
Xelapsed <- data.frame(R, systp[,3], syst[,3], systparl[,3], systpar[,3])
plote <- ggplot(data = Xelapsed, aes(Xelapsed[,1], Xelapsed[,2])) +
  geom_point(color = "red") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,3]), color = "blue") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,4]), color = "green") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,5]), color = "orange") +
  geom_line(aes(Xelapsed[,1], Xelapsed[,2], color = "foreach runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,3], color = "sequential runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,4], color = "parlmice runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,5], color = "mice.par runtimes")) +
  geom_point(aes(Xsystem[,1], Xsystem[,2]), color = "red") +
  geom_point(aes(Xsystem[,1], Xsystem[,3]), color = "blue") +
  geom_point(aes(Xsystem[,1], Xsystem[,4]), color = "green") +
  geom_point(aes(Xsystem[,1], Xsystem[,5]), color = "orange") +
  geom_line(aes(Xsystem[,1], Xsystem[,2], color = "foreach runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,3], color = "sequential runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,4], color = "parlmice runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,5], color = "mice.par runtimes"), linetype="dashed") +
  geom_point(aes(Xuser[,1], Xuser[,2]),color = "red") +
  geom_point(aes(Xuser[,1], Xuser[,3]), color = "blue") +
  geom_point(aes(Xuser[,1], Xuser[,4]), color = "green") +
  geom_point(aes(Xuser[,1], Xuser[,5]), color = "orange") +
  geom_line(aes(Xuser[,1], Xuser[,2], color = "foreach runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,3], color = "sequential runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,4], color = "parlmice runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,5], color = "mice.par runtimes"), linetype="dotted") +
  scale_colour_manual("", 
                      breaks = c("parallel runtimes", "sequential runtimes", "parlmice runtimes", "parmice runtimes"),
                      values = c("parallel runtimes"="red", "sequential runtimes"="blue", 
                                 "parlmice runtimes"="green", "parmice runtimes"="orange"))+
  scale_linetype_manual(values=c("elapsed"="solid","system time"="dashed", "user time"="dotted"))

plote <- plote + 
  ggtitle("Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plote

cd"#Stop Clustering
stopCluster(cl)
