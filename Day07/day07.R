library(dplyr)

source("fun.R")

# phase 1
t1 <- read_program("test1.txt")
r1 <- run_one_phase(x = t1, phase = c(4, 3, 2, 1, 0))
r1 == 43210

t2 <- read_program("test2.txt")
r2 <- run_one_phase(x = t2, phase = c(0, 1, 2, 3, 4))
r2 == 54321

t3 <- read_program("test3.txt")
r3 <- run_one_phase(x = t3, phase = c(1, 0, 4, 3, 2))
r3 == 65210

inp <- read_program("input.txt")
rp1 <- find_max_signal(inp, ph = 0:4, fun = run_one_phase)

# phase 2
t4 <- read_program("test4.txt")
r4 <- run_feedback_loop_one_phase(x = t4, phase = c(9, 8, 7, 6, 5))
r4 == 139629729

t5 <- read_program("test5.txt")
r5 <- run_feedback_loop_one_phase(x = t5, phase = c(9, 7, 8, 5, 6))
r5 == 18216

rp2 <- find_max_signal(inp, ph = 5:9, fun = run_feedback_loop_one_phase)
