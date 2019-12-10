library(dplyr)

source("fun.R")

# phase 1
t1 <- read_program("test1.txt")
r1 <- intcode_computer(x = t1, at = 1, input = NULL)
print(all(r1$out == t1))

t2 <- read_program("test2.txt")
r2 <- intcode_computer(x = t2, at = 1, input = NULL)
ndig <- r2$out %>%
  as.character() %>%
  nchar()
print(ndig == 16)

t3 <- read_program("test3.txt")
r3 <- intcode_computer(x = t3, at = 1, input = NULL)
print(r3$out == 1125899906842624)

inp <- read_program("input.txt")
rp1 <- intcode_computer(x = inp, at = 1, input = 1)
print(rp1$out) # 3340912345

rp2 <- intcode_computer(x = inp, at = 1, input = 2)
print(rp2$out) # 51754
