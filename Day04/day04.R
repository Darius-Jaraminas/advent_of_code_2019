
source("fun.R")

from <- 168630
to <- 718098

# part 1
check_rules(111111)
check_rules(223450)
check_rules(123789)

psw <- from:to
meet <- numeric(length(psw))
for (i in seq_along(psw)){
  meet[i] <- check_rules(psw[i])
}
sum(meet)

# part 2
check_rules_part_two(112233)
check_rules_part_two(123444)
check_rules_part_two(111122)

meet2 <- numeric(length(psw))
for (i in seq_along(psw)){
  meet2[i] <- check_rules_part_two(psw[i])
}
sum(meet2)
