library(dplyr)
library(tidyr)

source("fun.R")

# part 1
inp <- read_program("input.txt")
hull1 <- run_robot(inp = inp, first_col = "black")
rp1 <- n_distinct(hull1[, c("x", "y")])
print(rp1)

# part 2
hull2 <- run_robot(inp = inp, first_col = "white")

h <- hull2 %>% 
  group_by(x, y) %>% 
  filter(row_number() == n()) %>%
  select(-dir) %>% 
  mutate(
    col = replace(col, col == "black", "."),
    col = replace(col, col == "white", "#")
  ) %>% 
  pivot_wider(names_from = "x" , values_from = "col")

cols <- h %>% 
  colnames() %>% 
  setdiff("y") %>% 
  as.numeric() %>% 
  sort() %>% 
  as.character()

rp2 <- h %>% 
  select(y, cols) %>% 
  arrange(desc(y))

View(rp2)
