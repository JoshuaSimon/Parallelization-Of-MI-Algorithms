library("pacman") # packagemanager
p_load("mice", "car", "tidyverse", "foreach", "parallel", "doParallel",
       "iterators", "testit", "devtools", "usethis", "micemd", "ggpubr",
       "beepr", "dplyr", "disk.frame")
library(nycflights13)
object.size(flights)%>% format(., units="MB")
memory.size()
#Data
source("C:\\Users\\evawo\\Documents\\OFU Survey Statistik\\WS 21 22\\Unvollständige Daten\\Projekt\\Parallelization-Of-MI-Algorithms\\src\\dataGeneratorMoreVariables.R")
dat <- dataGenerator(n=10000, nvar=366)

# this will setup disk.frame's parallel backend with number of workers equal to the number of CPU cores (hyper-threaded cores are counted as one not two)
setup_disk.frame()
#> The number of workers available for disk.frame is 6
# this allows large datasets to be transferred between sessions
options(future.globals.maxSize = Inf)

# convert the flights data.frame to a disk.frame
# optionally, you may specify an outdir, otherwise, the 
dat.df <- as.disk.frame(dat)

beep(stime <- system.time(datmi.df <- collect(dat.df) %>% mice(data=., m=1, maxit = 1)%>%complete(., action = "long")))
datmi.df %>% is.na %>% sum
object.size(dat) %>% format(., units="MB")


setup_disk.frame(workers = 1)
#> The number of workers available for disk.frame is 6
# this allows large datasets to be transferred between sessions
options(future.globals.maxSize = Inf)
beep(stimem <- system.time(datmim.df <- collect(dat.df) %>% mice(data=., m=1, maxit = 1)%>%complete(., action = "long")))
