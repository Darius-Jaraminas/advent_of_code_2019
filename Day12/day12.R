library(dplyr)
library(tidyr)

source("fun.R")

# part 1
t1 <- read_moons(fnm = "test1.txt")
r1 <- run_time(pos = t1, t = 1)
e1 <- calculate_energy(x = r1)
ch1_pos10 <- data.frame(
  x = c(2, 1, 3, 2),
  y = c(1, -8, -6, 0),
  z = c(-3, 0, 1, 4)
)
ch1_vel10 <- data.frame(
  x = c(-3, -1, 3, 1),
  y = c(-2, 1, 2, -1),
  z = c(1, 3, -3, -1)
)
print(all(r1$pos == ch1_pos10))
print(all(r1$vel == ch1_vel10))
print(e1 == 179)

t2 <- read_moons(fnm = "test2.txt")
r2 <- run_time(pos = t2, t = 100)
e2 <- calculate_energy(x = r2)
print(e2 == 1940)

inp <- read_moons(fnm = "input.txt")
rp1 <- run_time(pos = inp, t = 1000)
ep1 <- calculate_energy(x = rp1)

# part 2
r1 <- run_time_keep_all(pos = t1, t = 1000)
all_ts <- extract_time_series(x = r1)
ptrn <- find_pattern_length(x = all_ts)
ptrn <- expand.grid(ptrn)
lcm <- apply(ptrn, 1, find_orbit_length)

y_ptrn <- ptrn[paste0("y", 1:4)]
y_ptrn <- expand.grid(y_ptrn)
y_lcm <- apply(y_ptrn, 1, find_orbit_length)

z_ptrn <- ptrn[paste0("z", 1:4)]
z_ptrn <- expand.grid(z_ptrn)
z_lcm <- apply(z_ptrn, 1, find_orbit_length)

eg <- expand.grid(unique(x_lcm), unique(y_lcm), unique(z_lcm))
lcm <- apply(eg, 1, find_orbit_length)




