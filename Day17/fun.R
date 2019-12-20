read_program <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>%
    strsplit(",") %>%
    `[[`(1) %>%
    as.numeric()
  names(out) <- paste0(1:length(out))
  close(con)
  return(out)
}

intcode_computer <- function(x, at, input, rb, output_counter = 0){
  halt <- FALSE
  input_counter <- 1
  out <- numeric()
  while (!halt){
    opvalue <- x[paste(at)]
    opcode <- opvalue - floor(opvalue / 100) * 100
    mode3 <- floor(opvalue / 10000)
    mode2 <- floor(opvalue / 1000) - mode3 * 10
    mode1 <- floor(opvalue / 100)  - floor(opvalue / 1000) * 10
    
    pos1 <- get_position(x = x, at = at, m = mode1, p = 1, rb = rb)
    pos2 <- get_position(x = x, at = at, m = mode2, p = 2, rb = rb)
    pos3 <- get_position(x = x, at = at, m = mode3, p = 3, rb = rb)
    
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
      if (input_counter > length(input)){
        return(list(x = x, at = at, out = out, rb = rb, halt = halt))
      }
      x[paste(pos1)] <- input[input_counter]
      input_counter <- input_counter + 1
      at <- at + 2
    }
    if (opcode == 4){
      out <- c(out, x1)
      at <- at + 2
      if (output_counter > 0){
        if (length(out) == output_counter){
          return(list(x = x, at = at, out = out, rb = rb, halt = halt))
        }
      }
    }
    if (opcode == 5){
      at <- ifelse(x1 != 0, x2 + 1, at + 3)
    }
    if (opcode == 6){
      at <- ifelse(x1 == 0, x2 + 1, at + 3)
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
      rb <- rb + x1
      at <- at + 2
    }
    if (opcode == 99){
      halt <- TRUE
    }
    if (anyNA(x)){
      x[is.na(x)] <- 0
    }
  }
  return(list(x = x, at = at, out = out, rb = rb, halt = halt))
}

get_position <- function(x, at, m, p, rb){
  case_when(
    m == 0 ~ x[paste(at + p)] + 1,
    m == 1 ~ at + p,
    m == 2 ~ x[paste(at + p)] + 1 + rb
  )
}

make_map <- function(x){
  x <- x$out
  lb <- which(x == 10)
  dlb <- unique(diff(lb))
  dlb <- setdiff(dlb, 1)
  if (length(dlb) > 1){
    stop("line length not the same!")
  }
  x <- x[x != 10]
  m <- matrix(x, ncol = dlb - 1, byrow = TRUE)
  mode(m) <- "character"
  m[m == "46"] <- "."
  m[m == "35"] <- "#"
  m[m == "94"] <- "^"
  return(m)
}

find_intersections <- function(m){
  four_sides <- matrix(c(0, 0, -1, 1, 1, -1, 0, 0), ncol = 2)
  w <- which(m == "#", arr.ind = TRUE)
  is_int <- logical(nrow(w))
  for (i in 1:nrow(w)){
    adj <- t(w[i, ] + t(four_sides))
    on_edge <- any(adj[, 1] > nrow(m) | adj[, 1] < 1 |
                     adj[, 2] > ncol(m) | adj[, 2] < 1)
    is_int[i] <- ifelse(on_edge, FALSE, all(m[adj] == "#"))
  }
  int <- w[is_int, ]
  return(int)
}
