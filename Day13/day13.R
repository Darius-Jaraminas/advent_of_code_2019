library(dplyr)
library(tidyr)

source("fun.R")

# part 1
inp <- read_program("input.txt")
r <- intcode_computer(x = inp, at = 1, rb = 0, output_counter = 0)
game <- tile_to_df(r)
blocks <- filter(game, tile_id == 2)
rp1 <- n_distinct(blocks, "x", "y")
print(rp1)

# part 2
inp[1] <- 2
r <- initial_tiles(x = inp)
rp2 <- play_full_game(r = r)
print(rp2)
