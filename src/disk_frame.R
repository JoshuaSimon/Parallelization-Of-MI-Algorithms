install.packages("disk.frame")
library(dplyr)
library(disk.frame)
library(nycflights13)

# this will setup disk.frame's parallel backend with number of workers equal to the number of CPU cores (hyper-threaded cores are counted as one not two)
setup_disk.frame()
#> The number of workers available for disk.frame is 6
# this allows large datasets to be transferred between sessions
options(future.globals.maxSize = Inf)

# convert the flights data.frame to a disk.frame
# optionally, you may specify an outdir, otherwise, the 
flights.df <- as.disk.frame(nycflights13::flights)

flights.df %>%
  filter(year == 2013) %>%
  mutate(origin_dest = paste0(origin, dest)) %>%
  head(2)

summarise(flights.df, mean_dep_delay = mean(dep_delay, na.rm =T)) %>% collect
