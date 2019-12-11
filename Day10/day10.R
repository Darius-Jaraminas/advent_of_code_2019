library(dplyr)

source("fun.R")

t1 <- read_map("test1.txt")
a1 <- find_max_detected(map = t1)
r1 <- as.numeric(a1[1, c("cols", "rows")])
print(all(r1 == c(3, 4)))

t2 <- read_map("test2.txt")
a2 <- find_max_detected(map = t2)
r2 <- as.numeric(a2[1, c("cols", "rows")])
print(all(r2 == c(5, 8)))
print(a2[1, "detected"] == 33)

t3 <- read_map("test3.txt")
a3 <- find_max_detected(map = t3)
r3 <- as.numeric(a3[1, c("cols", "rows")])
print(all(r3 == c(1, 2)))
print(a3[1, "detected"] == 35)

t4 <- read_map("test4.txt")
a4 <- find_max_detected(map = t4)
r4 <- as.numeric(a4[1, c("cols", "rows")])
print(all(r4 == c(6, 3)))
print(a4[1, "detected"] == 41)

t5 <- read_map("test5.txt")
a5 <- find_max_detected(map = t5)
r5 <- as.numeric(a5[1, c("cols", "rows")])
print(all(r5 == c(11, 13)))
print(a5[1, "detected"] == 210)

inp <- read_map("input.txt")
a <- find_max_detected(map = inp)
r <- as.numeric(a[1, c("cols", "rows")])
print(r)
print(a[1, "detected"])

# part 2
t6 <- read_map("test6.txt")
a6 <- find_max_detected(map = t6)
r6 <- as.numeric(a6[1, c("cols", "rows")])
v6 <- vaporization(map = t6, station = r6[c(2, 1)])

v5 <- vaporization(map = t5, station = r5[c(2, 1)])
print(all(v5[1, c("cols", "rows")] == c(11, 12)))
print(all(v5[2, c("cols", "rows")] == c(12, 1)))
print(all(v5[3, c("cols", "rows")] == c(12, 2)))
print(all(v5[10, c("cols", "rows")] == c(12, 8)))
print(all(v5[20, c("cols", "rows")] == c(16, 0)))
print(all(v5[50, c("cols", "rows")] == c(16, 9)))
print(all(v5[100, c("cols", "rows")] == c(10, 16)))
print(all(v5[199, c("cols", "rows")] == c(9, 6)))
print(all(v5[200, c("cols", "rows")] == c(8, 2)))
print(all(v5[299, c("cols", "rows")] == c(11, 1)))

v <- vaporization(map = inp, station = r[c(2, 1)])
rp2 <- v[200, "cols"] * 100 + v[200, "rows"]
