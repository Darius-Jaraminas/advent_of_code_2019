library(dplyr)
library(tidyr)

source("fun.R")

t1 <- read.table("test1.txt", stringsAsFactors = FALSE)
t1 <- separate(t1, "V1", into = c("orbits", "obj"), sep = "\\)")

ochs <- orbit_checksum(t1)
ochs["D"] == 3
ochs["L"] == 7
sum(ochs) == 42

inp <- read.table("input.txt", stringsAsFactors = FALSE)
inp <- separate(inp, "V1", into = c("orbits", "obj"), sep = "\\)")
oc <- orbit_checksum(inp)
sum(oc)

# part 2
t2 <- read.table("test2.txt", stringsAsFactors = FALSE)
t2 <- separate(t2, "V1", into = c("orbits", "obj"), sep = "\\)")
min_path(x = t2, from = "YOU", to = "SAN") == 4

min_path(x = inp, from = "YOU", to = "SAN")
