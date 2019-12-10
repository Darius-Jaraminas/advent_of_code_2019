read_program <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>%
    strsplit(",") %>%
    `[[`(1) %>%
    as.numeric()
  close(con)
  return(out)
}

intcode_computer <- function(x, at, input){
  names(x) <- paste0(1:length(x))
  halt <- FALSE
  input_counter <- 1
  relative_base <- 0
  out <- numeric()
  while (!halt){
    opvalue <- x[paste(at)]
    opcode <- opvalue - floor(opvalue / 100) * 100
    mode3 <- floor(opvalue / 10000)
    mode2 <- floor(opvalue / 1000) - mode3 * 10
    mode1 <- floor(opvalue / 100)  - floor(opvalue / 1000) * 10

    pos1 <- get_position(x = x, at = at, m = mode1, p = 1, rb = relative_base)
    pos2 <- get_position(x = x, at = at, m = mode2, p = 2, rb = relative_base)
    pos3 <- get_position(x = x, at = at, m = mode3, p = 3, rb = relative_base)
    
    x1 <- ifelse(is.na(x[paste(pos1)]), 0, x[paste(pos1)])
    x2 <- ifelse(is.na(x[paste(pos2)]), 0, x[paste(pos2)])
    if (opcode == 1){
      x[paste(pos3)] <- x1 + x2
      at <- at + 4
    }
    if (opcode == 2){
      x[paste(pos3)] <- x1 * x2
      at <- at + 4
    }
    if (opcode == 3){
      if (input_counter == 1){
        x[paste(pos1)] <- input[input_counter]
      } else{
        x[paste(pos1)] <- input[2]
      }
      input_counter <- input_counter + 1
      at <- at + 2
    }
    if (opcode == 4){
      out <- c(out, x1)
      at <- at + 2
    }
    if (opcode == 5){
      if (x1 != 0){
        at <- x2 + 1
      } else{
        at <- at + 3
      }
    }
    if (opcode == 6){
      if (x1 == 0){
        at <- x2 + 1
      } else{
        at <- at + 3
      }
    }
    if (opcode == 7){
      x[paste(pos3)] <- ifelse(x1 < x2, 1, 0)
      at <- at + 4
    }
    if (opcode == 8){
      x[paste(pos3)] <- ifelse(x1 == x2, 1, 0)
      at <- at + 4
    }
    if (opcode == 9){
      relative_base <- relative_base + x1
      at <- at + 2
    }
    if (opcode == 99){
      halt <- TRUE
    }
    if (anyNA(x)){
      x[is.na(x)] <- 0
    }
  }
  return(list(x = x, at = at, out = out, halt = TRUE))
}

get_position <- function(x, at, m, p, rb){
  case_when(
    m == 0 ~ x[paste(at + p)] + 1,
    m == 1 ~ at + p,
    m == 2 ~ x[paste(at + p)] + 1 + rb
  )
}
