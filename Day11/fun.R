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

intcode_computer <- function(x, at, input, rb){
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
      x[paste(pos1)] <- ifelse(input_counter == 1, input[1], input[2])
      input_counter <- input_counter + 1
      at <- at + 2
    }
    if (opcode == 4){
      out <- c(out, x1)
      at <- at + 2
      if (length(out) == 2){
        return(list(x = x, at = at, out = out, rb = rb, halt = halt))
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

paint_one_tile <- function(h, out){
  N <- nrow(h)
  h[N, "col"] <- ifelse(out[1] == 0, "black", "white")
  curr_dir <- h[N, "dir"]
  turn <- ifelse(out[2] == 0, "left", "right")
  next_dir <- case_when(
    curr_dir == "^" & turn == "left" ~ "<",
    curr_dir == "^" & turn == "right" ~ ">",
    curr_dir == "<" & turn == "left" ~ "v",
    curr_dir == "<" & turn == "right" ~ "^",
    curr_dir == "v" & turn == "left" ~ ">",
    curr_dir == "v" & turn == "right" ~ "<",
    curr_dir == ">" & turn == "left" ~ "^",
    curr_dir == ">" & turn == "right" ~ "v"
  )
  curr_xy <- as.numeric(h[N, c("x", "y")])
  next_xy <- case_when(
    curr_dir == "^" & next_dir == "<" ~ curr_xy + c(-1, 0),
    curr_dir == "^" & next_dir == ">" ~ curr_xy + c(1, 0),
    curr_dir == "<" & next_dir == "v" ~ curr_xy + c(0, -1),
    curr_dir == "<" & next_dir == "^" ~ curr_xy + c(0, 1),
    curr_dir == "v" & next_dir == ">" ~ curr_xy + c(1, 0),
    curr_dir == "v" & next_dir == "<" ~ curr_xy + c(-1, 0),
    curr_dir == ">" & next_dir == "^" ~ curr_xy + c(0, 1),
    curr_dir == ">" & next_dir == "v" ~ curr_xy + c(0, -1),
  )
  tiles <- h[h$x == next_xy[1] & h$y == next_xy[2], ]
  next_col <- ifelse(nrow(tiles) > 0, tiles[nrow(tiles), "col"], "black")
  next_tile <- data.frame(x = next_xy[1], y = next_xy[2],
                          col = next_col, dir = next_dir,
                          stringsAsFactors = FALSE)
  h <- bind_rows(h, next_tile)
  return(h)
}

run_robot <- function(inp, first_col){
  hull <- data.frame(x = 0, y = 0, col = first_col, dir = "^",
                     stringsAsFactors = FALSE)
  first_col <- ifelse(first_col == "black", 0, 1)
  r <- intcode_computer(x = inp, at = 1, input = first_col, rb = 0)
  hull <- paint_one_tile(h = hull, out = r$out)
  while (!r$halt){
    cur_col <- hull[nrow(hull), "col"]
    cur_col <- ifelse(cur_col == "black", 0, 1)
    r <- intcode_computer(x = r$x, at = r$at, input = cur_col, rb = r$rb)
    if (!r$halt){
      hull <- paint_one_tile(h = hull, out = r$out)
    }
  }
  return(hull)
}
