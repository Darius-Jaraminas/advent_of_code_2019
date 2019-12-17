library(dplyr)

source("fun.R")

# part 1
t1 <- read_reactions("test1.txt")
l1 <- trace_back_level1(x = t1)
print(l1$ore_needed$Q == 31)

t2 <- read_reactions("test2.txt")
l2 <- trace_back_level1(x = t2)
print(l2$ore_needed$Q  == 165)

t3 <- read_reactions("test3.txt")
l3 <- trace_back_level1(x = t3)
print(l3$ore_needed$Q  == 13312)

t4 <- read_reactions("test4.txt")
l4 <- trace_back_level1(x = t4)
print(l4$ore_needed$Q  == 180697)

t5 <- read_reactions("test5.txt")
l5 <- trace_back_level1(x = t5)
print(l5$ore_needed$Q == 2210736)

inp <- read_reactions("input.txt")
l <- trace_back_level1(x = inp)
print(l$ore_needed$Q)

# part 2
n3 <- search_N(x = t3, ores = 1e+12)
print(n3 == 82892753)

n4 <- search_N(x = t4, ores = 1e+12)
print(n4 == 5586022)

n5 <- search_N(x = t5, ores = 1e+12)
print(n5 == 460664)

n <- search_N(x = inp, ores = 1e+12)
print(n)
