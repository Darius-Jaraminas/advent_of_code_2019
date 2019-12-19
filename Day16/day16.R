library(dplyr)

source("fun.R")

# part 1
t1 <- read_pattern("test1.txt")
r11 <- fft(x = t1, k = 1)
print(r11 == "48226158")
r12 <- fft(x = t1, k = 2)
print(r12 == "34040438")
r13 <- fft(x = t1, k = 3)
print(r13 == "03415518")
r14 <- fft(x = t1, k = 4)
print(r14 == "01029498")

t2 <- read_pattern("test2.txt")
r2 <- fft(x = t2, k = 100)
print(substring(r2, 1, 8) == "24176176")

t3 <- read_pattern("test3.txt")
r3 <- fft(x = t3, k = 100)
print(substring(r3, 1, 8) == "73745418")

t4 <- read_pattern("test4.txt")
r4 <- fft(x = t4, k = 100)
print(substring(r4, 1, 8) == "52432133")

inp <- read_pattern("input.txt")
r <- fft(x = inp, k = 100)
rp1 <- substring(r, 1, 8)
print(rp1)

# part 2
inp <- read_pattern("input.txt")
inp <- rep.int(inp, 10000)
rp2 <- simple_solution(x = inp)
print(rp2)
