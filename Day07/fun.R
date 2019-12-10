run_program2 <- function(x, input){
  at <- 1
  halt <- FALSE
  input_counter <- 1
  out <- numeric()
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
      x[pos1] <- input[input_counter]
      input_counter <- input_counter + 1
      at <- at + 2
    }
    if (opcode == 4){
      out <- c(out, x[pos1])
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
  return(out)
}

run_program3 <- function(x, at, input){
  halt <- FALSE
  input_counter <- 1
  out <- numeric()
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
      if (input_counter == 1){
        x[pos1] <- input[input_counter]
      } else{
        x[pos1] <- input[2]
      }
      input_counter <- input_counter + 1
      at <- at + 2
    }
    if (opcode == 4){
      out <- c(out, x[pos1])
      at <- at + 2
      return(list(x = x, at = at, out = out, halt = FALSE))
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
  return(list(x = x, at = at, out = out, halt = TRUE))
}

run_one_phase <- function(x, phase){
  o1 <- run_program2(x = x, input = c(phase[1], 0))
  o2 <- run_program2(x = x, input = c(phase[2], o1))
  o3 <- run_program2(x = x, input = c(phase[3], o2))
  o4 <- run_program2(x = x, input = c(phase[4], o3))
  o5 <- run_program2(x = x, input = c(phase[5], o4))
  return(o5)
}

run_feedback_loop_one_phase <- function(x, phase){
  aa <- run_program3(x = x, at = 1, input = c(phase[1], 0))
  bb <- run_program3(x = x, at = 1, input = c(phase[2], aa$out))
  cc <- run_program3(x = x, at = 1, input = c(phase[3], bb$out))
  dd <- run_program3(x = x, at = 1, input = c(phase[4], cc$out))
  ee <- run_program3(x = x, at = 1, input = c(phase[5], dd$out))
  while (!ee$halt){
    aa <- run_program3(x = aa$x, at = aa$at, input = ee$out)
    bb <- run_program3(x = bb$x, at = bb$at, input = aa$out)
    cc <- run_program3(x = cc$x, at = cc$at, input = bb$out)
    dd <- run_program3(x = dd$x, at = dd$at, input = cc$out)
    ee <- run_program3(x = ee$x, at = ee$at, input = dd$out)
    if (!ee$halt){
      last_ee <- ee$out
    }
  }
  return(last_ee)
}

find_max_signal <- function(inp, ph, fun){
  all_combs <- expand.grid(ph, ph, ph, ph, ph)
  w <- apply(all_combs, 1, function(x, y){all(y %in% x)}, y = ph)
  all_combs <- all_combs[w, ]
  ncomb <- nrow(all_combs)
  thr <- numeric(ncomb)
  for (i in 1:ncomb){
    phase_i <- as.numeric(all_combs[i, ])
    thr[i] <- fun(x = inp, phase = phase_i)
  }
  return(max(thr))
}

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
