
# part 1
fnm_input <- "input01.txt"
input <- read.csv(fnm_input, header = FALSE)
# test
test <- c(12, 14, 1969, 100756)
floor(test / 3) - 2
# solve
input <- input[[1]]
sum(floor(input / 3) - 2)

# part 2
fnm_input <- "input01.txt"
input <- read.csv(fnm_input, header = FALSE)
# test
test <- c(14, 1969, 100756)
source("fun.R")
calculate_total_fuel(test)
# solve
r2 <- sum(calculate_total_fuel(input))
