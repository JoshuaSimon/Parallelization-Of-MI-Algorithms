#Parallelization of Multiple Imputation
#Packages
library("pacman") # packagemanager
p_load("mice", "car", "tidyverse", "foreach", "parallel", "doParallel",
       "iterators", "testit", "devtools", "usethis", "micemd", "ggpubr")


#Data: ESS data 2004 on satisfaction in life 
source("C:\\Users\\evawo\\Documents\\OFU Survey Statistik\\WS 21 22\\Unvollständige Daten\\Projekt\\Parallelization-Of-MI-Algorithms\\src\\dataGenerator.R")

dat <- dataGenerator(n=50000)

R <- c(1, 5, 10, 20, 30, 40, 60, 80, 100) #Multiple Imputations 
n <- NROW(dat)
seed <- 42

### Multiple imputation and Analysis ###

# use all variables as predictors, but just consider linear relationships..
# initialization:
ini <- mice(data=dat, m = 1, maxit = 0)
predictorMatrix <- ini$pred


##Parallelization

#Core Idenitification and Clustering
foreachfunction <- function(data, seed, num_imp, maxit, m, predictorMatrix){
  num_cores <- detectCores()-1
  cl <- makeCluster(num_cores)
  clusterSetRNGStream(cl, seed)
  registerDoParallel(cl)
  
  par_mice <- foreach(i=1:num_imp, .combine = ibind, .packages="mice") %dopar% {
    mice(data = data, m = m, maxit = maxit, printFlag = FALSE, predictorMatrix = predictorMatrix)
  }
  return(par_mice)
  stopCluster(cl)
}

#Store imputation results and runtime
result <- resultp <- resultparl <- resultpar <-   list()
systp <- syst <- systparl <- systpar <- matrix(nrow = length(R), ncol=3)

#Sequential and different parallelized MIs
for(i in 1:length(R)){
  
systp[i,] <- system.time(
resultp[[i]] <- foreachfunction(data=dat, seed=seed, num_imp = R[i], maxit=5, m=1, predictorMatrix = predictorMatrix)
)[1:3]
#run time of parallelized MI

syst[i,] <- system.time(result[[i]] <- mice(data=dat,m = R[i], predictorMatrix = predictorMatrix, print = F, seed=seed))[1:3]
#run time of sequential MI

systparl[i,] <- system.time(resultparl[[i]] <- parlmice(data=dat, m=R[i], predictorMatrix = predictorMatrix, print = F, cluster.seed = seed, n.core=7, n.imp.core = ceiling(R[i]/7)))[1:3]
#run time parlmice

systpar[i,] <- system.time(resultpar[[i]] <- micemd::mice.par(don.na=dat, m=R[i], predictorMatrix = predictorMatrix, print = F, seed=seed))[1:3]
#runtime micemd::mice.par()
}

#Runtime Visualization
Xuser<- data.frame(R, systp[,1], syst[,1], systparl[,1], systpar[,1])
Xsystem <- data.frame(R, systp[,2], syst[,2], systparl[,2], systpar[,2])
Xelapsed <- data.frame(R, systp[,3], syst[,3], systparl[,3], systpar[,3])

#plot elapsed time
plote <- ggplot(data = Xelapsed, aes(Xelapsed[,1], Xelapsed[,2])) +
  geom_point(color = "red") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,3]), color = "blue") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,4]), color = "green") +
  geom_point(aes(Xelapsed[,1], Xelapsed[,5]), color = "orange") +
  geom_line(aes(Xelapsed[,1], Xelapsed[,2], color = "foreach runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,3], color = "sequential runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,4], color = "parlmice runtimes")) +
  geom_line(aes(Xelapsed[,1], Xelapsed[,5], color = "mice.par runtimes")) +
  scale_colour_manual("", 
                      breaks = c("foreach runtimes", "sequential runtimes", "parlmice runtimes", "mice.par runtimes"),
                      values = c("foreach runtimes"="red", "sequential runtimes"="blue", 
                                 "parlmice runtimes"="green", "mice.par runtimes"="orange"))+
  scale_linetype_manual(values=c("elapsed"="solid","system time"="dashed", "user time"="dotted"))

plote <- plote + 
  ggtitle("Elapsed Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plote  

#plot user cpu time
plotu <- ggplot(data = Xuser, aes(Xuser[,1], Xuser[,2])) +
  geom_point(color = "red") +
  geom_point(aes(Xuser[,1], Xuser[,3]), color = "blue") +
  geom_point(aes(Xuser[,1], Xuser[,4]), color = "green") +
  geom_point(aes(Xuser[,1], Xuser[,5]), color = "orange") +
  geom_line(aes(Xuser[,1], Xuser[,2], color = "foreach runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,3], color = "sequential runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,4], color = "parlmice runtimes"), linetype="dotted") +
  geom_line(aes(Xuser[,1], Xuser[,5], color = "mice.par runtimes"), linetype="dotted") +
  scale_colour_manual("", 
                      breaks = c("foreach runtimes", "sequential runtimes", "parlmice runtimes", "mice.par runtimes"),
                      values = c("foreach runtimes"="red", "sequential runtimes"="blue", 
                                 "parlmice runtimes"="green", "mice.par runtimes"="orange"))+
  scale_linetype_manual(values=c("elapsed"="solid","system time"="dashed", "user time"="dotted"))

plotu <- plotu + 
  ggtitle("User CPU Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plotu  

#plot system cpu time
plots <- ggplot(data = Xsystem, aes(Xsystem[,1], Xsystem[,2])) +
  geom_point(color = "red") +
  geom_point(aes(Xsystem[,1], Xsystem[,3]), color = "blue") +
  geom_point(aes(Xsystem[,1], Xsystem[,4]), color = "green") +
  geom_point(aes(Xsystem[,1], Xsystem[,5]), color = "orange") +
  geom_line(aes(Xsystem[,1], Xsystem[,2], color = "foreach runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,3], color = "sequential runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,4], color = "parlmice runtimes"), linetype="dashed") +
  geom_line(aes(Xsystem[,1], Xsystem[,5], color = "mice.par runtimes"), linetype="dashed") +
  scale_colour_manual("", 
                      breaks = c("foreach runtimes", "sequential runtimes", "parlmice runtimes", "mice.par runtimes"),
                      values = c("foreach runtimes"="red", "sequential runtimes"="blue", 
                                 "parlmice runtimes"="green", "mice.par runtimes"="orange"))+
  scale_linetype_manual(values=c("elapsed"="solid","system time"="dashed", "user time"="dotted"))

plots <- plots + 
  ggtitle("System CPU Runtime of Multiple Imputations") +
  xlab("Number of multiple imputations M") + ylab("Runtime in seconds")

plots

dev.new()
ggarrange(plote, plotu, plots, nrow=3, ncol=1)
