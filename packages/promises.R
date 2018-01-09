

library(future)
library(promises)
library(tidyverse)
library(rlang)
plan(multiprocess)


long_running_function <- function(sec) {
  now <- Sys.time()
  i <- 1.0
  while (difftime(Sys.time(), now, units = 'secs') < sec) {
    i <- i + 1
  }
  i
}


ns <- c(5, 5, 4, 6, 5, 6, 5)


## Single thread
system.time({
  ns %>% 
    map(function(x){
      print(x)
      long_running_function(x)
    }) %>% 
    print()
})

## Multi-thread
system.time({
  ns %>% 
    map(function(x){
      print(x)
      future(long_running_function(x))
    }) %>% 
    map(value) %>%
    print()
})














