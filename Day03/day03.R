library(dplyr)

source("fun.R")

# test1
t1 <- "test1.txt" %>%
  file() %>%
  readLines() %>% 
  prep_wires()

w1 <- draw_wire(w = t1[[1]])
w2 <- draw_wire(w = t1[[2]])

inter <- inner_join(w1, w2, by = c("rows", "cols"))
r1 <- find_min_dist(int = inter)
r1 == 6

pl <- path_length(w1, w2)
pl1 <- min(pl$sum)
pl1 == 30

# test2
t2 <- "test2.txt" %>%
  file() %>%
  readLines() %>%
  prep_wires()

w1 <- draw_wire(w = t2[[1]])
w2 <- draw_wire(w = t2[[2]])

inter <- inner_join(w1, w2, by = c("rows", "cols"))
r2 <- find_min_dist(int = inter)
r2 == 159

pl <- path_length(w1, w2)
pl2 <- min(pl$sum)
pl2 == 610

# test3
t3 <- "test3.txt" %>%
  file() %>%
  readLines() %>%
  prep_wires()

w1 <- draw_wire(w = t3[[1]])
w2 <- draw_wire(w = t3[[2]])

inter <- inner_join(w1, w2, by = c("rows", "cols"))
r3 <- find_min_dist(int = inter)
r3 == 135

pl <- path_length(w1, w2)
pl3 <- min(pl$sum)
pl3 == 410

# input
inp <- "input.txt" %>%
  file() %>%
  readLines() %>%
  prep_wires()

w1 <- draw_wire(w = inp[[1]])
w2 <- draw_wire(w = inp[[2]])

inter <- inner_join(w1, w2, by = c("rows", "cols"))
r <- find_min_dist(int = inter)

# part 2
pl <- path_length(w1, w2)
pl_inp <- min(pl$sum)
