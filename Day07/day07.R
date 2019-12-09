library(dplyr)

source("fun.R")

t1 <- file("test1.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()
r1 <- run_one_phase(x = t1, phase = c(4, 3, 2, 1, 0))
r1 == 43210

t2 <- file("test2.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()
r2 <- run_one_phase(x = t2, phase = c(0, 1, 2, 3, 4))
r2 == 54321

t3 <- file("test3.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()
r3 <- run_one_phase(x = t3, phase = c(1, 0, 4, 3, 2))
r3 == 65210


inp <- file("input.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()
all_combs <- expand.grid(0:4, 0:4, 0:4, 0:4, 0:4)
w <- apply(all_combs, 1, function(x){all(0:4 %in% x)})
all_combs <- all_combs[w, ]
thr <- numeric(length(all_combs))
for (i in 1:nrow(all_combs)){
  phase_i <- as.numeric(all_combs[i, ])
  thr[i] <- run_one_phase(x = inp, phase = phase_i)
}
r <- max(thr)

# phase 2

t4 <- file("test4.txt") %>%
  readLines() %>% 
  strsplit(",") %>% 
  `[[`(1) %>% 
  as.numeric()


o1 <- run_program3(x = t4, input = c(9, 0))
o2 <- run_program3(x = t4, input = c(8, unique(o1$out)))
o3 <- run_program3(x = t4, input = c(7, unique(o2$out)))
o4 <- run_program3(x = t4, input = c(6, unique(o3$out)))
o5 <- run_program3(x = t4, input = c(5, unique(o4$out)))

for (i in 1:5){
  o1 <- run_program3(x = t4, input = c(9, unique(o5$out)))
  o2 <- run_program3(x = t4, input = c(8, unique(o1$out)))
  o3 <- run_program3(x = t4, input = c(7, unique(o2$out)))
  o4 <- run_program3(x = t4, input = c(6, unique(o3$out)))
  o5 <- run_program3(x = t4, input = c(5, unique(o4$out)))
  print(o5)
}



https://www.reddit.com/r/adventofcode/comments/e7aqcb/2019_day_7_part_2_confused_with_the_question/

r3 <- run_one_phase(x = t3, phase = c(9, 8, 7, 6, 5))
r3 == 65210



