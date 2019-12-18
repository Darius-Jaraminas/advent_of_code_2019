library(dplyr)
library(tidyr)

source("fun.R")

# part 1
inp <- read_program("input.txt")
map <- run_robot(x = inp)
rp1 <- nrow(map$path$path)
print(rp1)

# part 2
full_map <- run_robot_fully(x = inp)
rp2 <- release_oxygen(x = full_map)
print(rp2)
