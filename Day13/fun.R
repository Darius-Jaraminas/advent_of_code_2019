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
      x[paste(pos1)] <- ifelse(input_counter == 1, input[1], input[2])
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

tile_to_df <- function(x){
  tile_i <- matrix(x$out, ncol = 3, byrow = TRUE)
  tile_i <- data.frame(tile_i, stringsAsFactors = FALSE)
  colnames(tile_i) <- c("x", "y", "tile_id")
  return(tile_i)
}

initial_tiles <- function(x){
  tile <- intcode_computer(x = x, at = 1, rb = 0, output_counter = 3)
  g <- tile_to_df(tile)
  score_tile <- FALSE
  while (!score_tile){
    tile <- intcode_computer(x = tile$x, at = tile$at, rb = tile$rb,
                             output_counter = 3)
    tile_i <- tile_to_df(tile)
    g <- bind_rows(g, tile_i)
    score_tile <- tile_i$x == -1 & tile_i$y == 0
  }
  return(list(game = g, tile = tile))
}

play_one_bounce <- function(g, x){
  ball_x <- next_ball_position(g = g, x = x, ball_y = 19)
  bx <- ball_x$bx
  t <- x
  pad <- g[g$tile_id == 3, ]
  pad_x <- pad[nrow(pad), "x"]
  input <- case_when(
    pad_x == bx ~ 0,
    pad_x < bx ~ 1,
    pad_x > bx ~ -1
  )
  while (pad_x != bx){
    t <- intcode_computer(x = t$x, at = t$at, input = input, rb = t$rb,
                          output_counter = 3)
    tile_i <- tile_to_df(t)
    g <- bind_rows(g, tile_i)
    pad <- g[g$tile_id == 3, ]
    pad_x <- pad[nrow(pad), "x"]
  }
  r1 <- next_ball_position(g = g, x = t, ball_y = 19)
  r2 <- next_ball_position(g = r1$game, x = r1$tile, ball_y = 18)
  return(list(game = r2$game, tile = r2$tile))
}

play_full_game <- function(r){
  nblocks <- nrow(filter(r$game, tile_id == 2))
  while (nblocks > 0){
    r <- play_one_bounce(g = r$game, x = r$tile)
    nblocks <- check_blocks(g = r$game)
    print(nblocks)
  }
  score_tile <- filter(r$game, x == -1)
  score <- score_tile[nrow(score_tile), "tile_id"]
  return(score)
}

check_blocks <- function(g){
  blocks <- g[g$tile_id == 2, ]
  empty <- g[g$tile_id == 0, ]
  nbl <- nrow(anti_join(blocks, empty, by = c("x", "y")))
  return(nbl)
}

next_ball_position <- function(g, x, ball_y){
  t <- x
  ball <- g[g$tile_id == 4, ]
  ball_y_19 <- ball[nrow(ball), "y"] == ball_y
  while (!ball_y_19){
    t <- intcode_computer(x = t$x, at = t$at, input = 0, rb = t$rb,
                          output_counter = 3)
    if (t$halt){
      break
    }
    tile_i <- tile_to_df(t)
    g <- bind_rows(g, tile_i)
    ball <- g[g$tile_id == 4, ]
    ball_y_19 <- ball[nrow(ball), "y"] == ball_y
  }
  bx <- ball[nrow(ball), "x"]
  return(list(game = g, tile = t, bx = bx))
}
