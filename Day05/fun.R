run_program1 <- function(x, input){
  at <- 1
  halt <- FALSE
  while (!halt){
    opvalue <- x[at]
    opcode <- opvalue - floor(opvalue / 100) * 100
    mode1 <- floor(opvalue / 100)  - floor(opvalue / 1000) * 10
    mode2 <- floor(opvalue / 1000)
    pos1 <- ifelse(mode1 == 0, x[at + 1] + 1, at + 1)
    pos2 <- ifelse(mode2 == 0, x[at + 2] + 1, at + 2)
    if (opcode == 1){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- x[pos1] + x[pos2]
      at <- at + 4
    }
    if (opcode == 2){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- x[pos1] * x[pos2]
      at <- at + 4
    }
    if (opcode == 3){
      x[pos1] <- input
      at <- at + 2
    }
    if (opcode == 4){
      print(x[pos1])
      at <- at + 2
    }
    if (opcode == 99){
      halt <- TRUE
    }
  }
  return(invisible(NULL))
}

run_program2 <- function(x, input){
  at <- 1
  halt <- FALSE
  while (!halt){
    opvalue <- x[at]
    opcode <- opvalue - floor(opvalue / 100) * 100
    mode1 <- floor(opvalue / 100)  - floor(opvalue / 1000) * 10
    mode2 <- floor(opvalue / 1000)
    pos1 <- ifelse(mode1 == 0, x[at + 1] + 1, at + 1)
    pos2 <- ifelse(mode2 == 0, x[at + 2] + 1, at + 2)
    
    if (opcode == 1){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- x[pos1] + x[pos2]
      at <- at + 4
    }
    if (opcode == 2){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- x[pos1] * x[pos2]
      at <- at + 4
    }
    if (opcode == 3){
      x[pos1] <- input
      at <- at + 2
    }
    if (opcode == 4){
      print(x[pos1])
      at <- at + 2
    }
    if (opcode == 5){
      if (x[pos1] != 0){
        at <- x[pos2] + 1
      } else{
        at <- at + 3
      }
    }
    if (opcode == 6){
      if (x[pos1] == 0){
        at <- x[pos2] + 1
      } else{
        at <- at + 3
      }
    }
    if (opcode == 7){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- ifelse(x[pos1] < x[pos2], 1, 0)
      at <- at + 4
    }
    if (opcode == 8){
      pos_out <- x[at + 3] + 1
      x[pos_out] <- ifelse(x[pos1] == x[pos2], 1, 0)
      at <- at + 4
    }
    if (opcode == 99){
      halt <- TRUE
    }
  }
  return(invisible(NULL))
}
