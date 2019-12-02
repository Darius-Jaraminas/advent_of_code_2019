
library(dplyr)

source("fun.R")

test1 <- c(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)
answ1 <- c(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
test_program(x = test1, y = answ1)

test2 <- c(1, 0, 0, 0, 99)
answ2 <- c(2, 0, 0, 0, 99)
test_program(x = test2, y = answ2)

test3 <- c(2, 3, 0, 3, 99)
answ3 <- c(2, 3, 0, 6, 99)
test_program(x = test3, y = answ3)

test4 <- c(2, 4, 4, 5, 99, 0)
answ4 <- c(2, 4, 4, 5, 99, 9801)
test_program(x = test4, y = answ4)

test5 <- c(1, 1, 1, 4, 99, 5, 6, 0, 99)
answ5 <- c(30, 1, 1, 4, 2, 5, 6, 0, 99)
test_program(x = test5, y = answ5)

cn <- file("Input.txt")
inp <- readLines(cn)
inp <- strsplit(inp, ",")
inp <- inp[[1]]
inp <- as.numeric(inp)

# restore
inp2 <- inp
inp2[2] <- 12
inp2[3] <- 2
r <- run_program(inp2)
r[1]

# part 2
all_r <- list()
for (i in 0:99){
  for (j in 0:99){
    inp2 <- inp
    inp2[2] <- i
    inp2[3] <- j
    rij <- run_program(inp2)[1]
    all_r[[length(all_r) + 1]] <- data.frame(i = i, j = j, output = rij)
  }
}
all_r <- bind_rows(all_r)
w <- which(all_r$output == 19690720)
noun <- all_r[w, "i"]
verb <- all_r[w, "j"]
r <- 100 * noun + verb
