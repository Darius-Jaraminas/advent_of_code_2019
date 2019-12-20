library(dplyr)
library(tidyr)

source("fun.R")

# part 1
inp <- read_program("input.txt")
scf <- intcode_computer(x = inp, at = 1, input = NULL, rb = 0)
map <- make_map(x = scf)
int <- find_intersections(m = map)
rp1 <- sum((int[, 1] - 1) * (int[, 2] - 1))
print(rp1)

# part 2
inp[1] <- 2

main <- c("A", "B", "A", "B", "A", "C", "A", "C", "B", "C")
A <- c("R", "6", "L", "10", "R", "10", "R", "10")
B <- c("L", "10", "L", "12", "R", "10")
C <- c("R", "6", "L", "12", "L", "10")
main_ascii <- c(65, 44, 66, 44, 65, 44, 66, 44, 65, 44,
                67, 44, 65, 44, 67, 44, 66, 44, 67, 10)
A_ascii <- c(82, 44, 54, 44, 76, 44, 49, 48, 44, 82,
             44, 49, 48, 44, 82, 44, 49, 48, 10)
B_ascii <- c(76, 44, 49, 48, 44, 76, 44, 49, 50, 44,
             82, 44, 49, 48, 10)
C_ascii <- c(82, 44, 54, 44, 76, 44, 49, 50, 44, 76,
             44, 49, 48, 10)
feed_ascii <- c(110, 10)
instructions <- c(main_ascii, A_ascii, B_ascii, C_ascii, feed_ascii)

scf2 <- intcode_computer(x = inp, at = 1, input = instructions, rb = 0)
rp2 <- scf2$out
rp2 <- rp2[rp2 > 255]
names(rp2) <- NULL
print(rp2)
