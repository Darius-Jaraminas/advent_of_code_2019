library(dplyr)

source("fun.R")

inp <- file("Input.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()

# part 1
run_program1(x = inp, input = 1)

# part 2
t1 <- c(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
t2 <- c(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
t5 <- c(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
t7 <- c(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 
        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 
        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

run_program2(x = t1, input = 8) # 1
run_program2(x = t1, input = 9) # 0

run_program2(x = t2, input = 8) # 0
run_program2(x = t2, input = 7) # 1

run_program2(x = t5, input = 0) # 0
run_program2(x = t5, input = -2) # 1

run_program2(x = t7, input = -9) # 999
run_program2(x = t7, input = 8) # 1000
run_program2(x = t7, input = 16) # 1001

run_program2(x = inp, input = 5)
