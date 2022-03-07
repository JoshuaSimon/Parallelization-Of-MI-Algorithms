library("pacman") # packagemanager
p_load("mice", "car", "tidyverse", "foreach", "parallel", "doParallel",
       "iterators", "testit", "devtools", "usethis", "micemd", "ggpubr",
       "beepr", "dplyr", "disk.frame", "nycflights13")

object.size(flights) %>% format(., units = "MB")
memory.size()

#Data
source("src/dataGeneratorMoreVariables.R")
dat <- dataGenerator(n = 10000, nvar = 366)

# this will setup disk.frame's parallel backend with number of workers
# equal to the number of CPU cores (hyper-threaded cores are counted
# as one not two)
setup_disk.frame()

# number of cpus available for disk.frame is 4
options(future.globals.maxSize = Inf)

# convert our own generated data to a disk.frame
dat.df <- as.disk.frame(dat)

# multiply impute the huge data set with the mice function and m = 1
# to keep the computation time bearable
beep(stime <- system.time(datmi.df <- collect(dat.df)
                        %>% mice(data = ., m = 1, maxit = 1)
                        %>%complete(., action = "long")))
datmi.df %>% is.na %>% sum
object.size(dat) %>% format(., units = "MB")

# second run: use only 1 cpu for the disk framing to compare the time gain
# through parallelization
setup_disk.frame(workers = 1)
beep(stimem <- system.time(datmim.df <- collect(dat.df) 
                           %>% mice(data = ., m = 1, maxit = 1)
                           %>%complete(., action = "long")))
