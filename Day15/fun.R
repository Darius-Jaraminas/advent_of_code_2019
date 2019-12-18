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

run_robot <- function(x){
  r0 <- list(x = x, at = 1, rb = 0)
  r0 <- list(intcode = r0, path = NULL)
  r <- path_objects(p = r0, dir = 1:4)
  paths <- look_around(x = r)
  coord <- data.frame(x = 0, y = 0, out = 1)
  coord <- add_coordinates(coord = coord, paths = paths)
  oxygen_found <- 2 %in% coord$out
  while (!oxygen_found){
    good_paths <- drop_wall_paths(x = paths)
    paths <- add_directions(paths = good_paths, coord = coord)
    paths <- look_around(x = paths)
    coord <- add_coordinates(coord = coord, paths = paths)
    oxygen_found <- 2 %in% coord$out
  }
  w <- lapply(paths, function(y){
    y$path[nrow(y$path), "out"] == 2
  })
  path <- paths[[which(unlist(w))]]
  return(list(coord = coord, path = path))
}

run_robot_fully <- function(x){
  r0 <- list(x = x, at = 1, rb = 0)
  r0 <- list(intcode = r0, path = NULL)
  r <- path_objects(p = r0, dir = 1:4)
  paths <- look_around(x = r)
  coord <- data.frame(x = 0, y = 0, out = 1)
  coord <- add_coordinates(coord = coord, paths = paths)
  fully_mapped <- length(paths) == 0
  while (!fully_mapped){
    good_paths <- drop_wall_paths(x = paths)
    paths <- add_directions(paths = good_paths, coord = coord)
    paths <- look_around(x = paths)
    coord <- add_coordinates(coord = coord, paths = paths)
    fully_mapped <- length(paths) == 0
  }
  return(coord = coord)
}

look_around <- function(x){
  o <- list()
  intcode <- list()
  for (i in seq_along(x)){
    icd <- x[[i]]$intcode
    dir <- x[[i]]$path$path
    dir <- dir[length(dir)]
    intcode[[i]] <- intcode_computer(x = icd$x, at = icd$at, input = dir,
                                     rb = icd$rb)
    o[[i]] <- intcode[[i]]$out
  }
  d <- mapply(function(a, b, c){
    a$path[nrow(a$path), "out"] <- b
    a$intcode <- c
    return(a)
  }, x, o, intcode, SIMPLIFY = FALSE)
  return(d)
}

add_coordinates <- function(coord, paths){
  add <- lapply(paths, function(x){
    xy <- get_coord(dirs = x$path$path)
    xy <- data.frame(x = xy[1], y = xy[2], out = x$path$out[nrow(x$path)])
  })
  add <- bind_rows(add)
  coord <- bind_rows(coord, add)
  return(coord)
}

get_coord <- function(dirs){
  xy <- c(0, 0)
  for (i in seq_along(dirs)){
    xy <- case_when(
      dirs[i] == 1 ~ xy + c(0, 1),
      dirs[i] == 2 ~ xy + c(0, -1),
      dirs[i] == 3 ~ xy + c(-1, 0),
      dirs[i] == 4 ~ xy + c(1, 0)
    )
  }
  return(xy)
}

drop_wall_paths <- function(x){
  walls <- lapply(x, function(y){
    y$path[nrow(y$path), "out"] == 0
  })
  walls <- unlist(walls)
  x <- x[!walls]
  return(x)
}

add_directions <- function(paths, coord){
  four_dirs <- data.frame(x = c(0, 0, -1, 1), y = c(1, -1, 0, 0),
                          dir = 1:4, stringsAsFactors = FALSE)
  new_paths <- list()
  for (p in seq_along(paths)){
    at <- get_coord(dirs = paths[[p]]$path$path)
    new_dir <- four_dirs
    new_dir$x <- new_dir$x + at[1]
    new_dir$y <- new_dir$y + at[2]
    new_dir <- anti_join(new_dir, coord, by = c("x", "y"))
    coord <- bind_rows(coord, new_dir)
    paths_i <- path_objects(p = paths[[p]], dir = new_dir$dir)
    new_paths <- append(new_paths, paths_i)
  }
  return(new_paths)
}

path_objects <- function(p, dir){
  paths_i <- lapply(dir, function(x, pi){
    add_p <- data.frame(path = x, out = NA)
    pi$path <- bind_rows(pi$path, add_p)
    return(pi)
  }, pi = p)
  return(paths_i)
}

build_map <- function(coord){
  map <- pivot_wider(coord, names_from = "x", values_from = "out")
  cnm <- map %>% 
    colnames() %>% 
    setdiff("y") %>% 
    as.numeric() %>% 
    sort() %>% 
    as.character()
  map <- map %>% 
    arrange(desc(y)) %>% 
    select(y, cnm)
  map2 <- map %>% 
    select(-y) %>% 
    as.matrix()
  mode(map2) <- "character"
  rownames(map2) <- map$y
  map2[is.na(map2)] <- ""
  map2[map2 == "0"] <- "#"
  map2[map2 == "1"] <- "."
  map2[map2 == "2"] <- "O"
  return(map2)
}

release_oxygen <- function(x){
  map <- build_map(coord = x)
  four_sides <- matrix(c(0, 0, -1, 1, 1, -1, 0, 0), ncol = 2)
  m <- 0
  filled <- sum(map == ".") == 0
  while (!filled){
    w <- which(map == "O", arr.ind = TRUE)
    for (i in 1:nrow(w)){
      adj <- t(w[i, ] + t(four_sides))
      smb <- map[adj]
      if (any(smb == ".")){
        w_empty <- adj[smb == ".", ,drop = FALSE]
        map[w_empty] <- "O"
      }
    }
    m <- m + 1
    filled <- sum(map == ".") == 0
  }
  return(m)
}
