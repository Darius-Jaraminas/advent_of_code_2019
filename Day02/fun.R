run_program <- function(x){
  fours <- seq(1, length(x), by = 4)
  for (i in fours){
    opcode <- x[i]
    if (opcode == 99){
      return(x)
    }
    positions <- x[c(i + 1, i + 2)] + 1
    output_pos <- x[i + 3] + 1
    if (opcode == 1){
      x[output_pos] <- sum(x[positions])
    }
    if (opcode == 2){
      x[output_pos] <- x[positions[1]] * x[positions[2]]
    }
  }
  return(x)
}

test_program <- function(x, y){
  r1 <- run_program(x)
  all(r1 == y)
}