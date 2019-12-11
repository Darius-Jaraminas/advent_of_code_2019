read_map <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- lapply(out, function(x){
    substring(x, seq(1, nchar(x)),seq(1, nchar(x)))
  })
  N <- length(out[[1]])
  out <- out %>% 
    unlist() %>% 
    matrix(ncol = N, byrow = TRUE)
  colnames(out) <- paste(0:(ncol(out) - 1))
  rownames(out) <- paste(0:(nrow(out) - 1))
  return(out)
}

relative_coord <- function(x, w){
  cnm <- x %>%
    colnames() %>%
    as.numeric()
  cnm <- cnm - w[2]
  colnames(x) <- paste(cnm)
  rnm <- x %>%
    rownames() %>%
    as.numeric()
  rnm <- rnm - w[1]
  rownames(x) <- paste(rnm)
  return(x)
}

all_asteroids <- function(x){
  cnm <- x %>%
    colnames() %>%
    as.numeric()
  rnm <- x %>%
    rownames() %>%
    as.numeric()
  w <- which(x == "#")
  ast_col <- ceiling(w / nrow(x))
  ast_row <- w - (ast_col - 1) * nrow(x)
  ast_col <- cnm[ast_col]
  ast_row <- rnm[ast_row]
  ac <- data.frame(rows = ast_row, cols = ast_col, stringsAsFactors = FALSE)
  ac <- ac[!(ac$rows == 0 & ac$cols == 0), ]
  return(ac)
}

calculate_detected <- function(x){
  d <- expand.grid(-1:1, -1:1)
  colnames(d) <- c("rsign", "csign")
  d <- d[!(d$rsign == 0 & d$csign == 0), ]
  d$grsign <- 1:nrow(d)
  ac <- all_asteroids(x)
  ac$rsign <- sign(ac$rows)
  ac$csign <- sign(ac$cols)
  ac <- left_join(ac, d, by = c("rsign", "csign"))
  ac$ratio <- ac$rows / ac$cols
  ac$rank <- rank(ac$ratio, ties.method = "average")
  dr <- distinct(ac[, c("grsign", "rank")])
  dr$group <- 1:nrow(dr)
  ac <- left_join(ac, dr, by = c("grsign", "rank"))
  ac$manh <- abs(ac$rows) + abs(ac$cols)
  return(ac)
}

find_max_detected <- function(map){
  asteroids <- all_asteroids(map)
  N <- nrow(asteroids)
  detected <- numeric(N)
  for (i in 1:N){
    ast_i <- as.numeric(asteroids[i, ])
    map_i <- relative_coord(x = map, w = ast_i)
    stats <- calculate_detected(map_i)
    detected[i] <- n_distinct(stats$group)
  }
  asteroids$detected <- detected
  asteroids <- asteroids[order(-asteroids$detected), ]
  return(asteroids)
}

vaporization <- function(map, station){
  asteroids <- all_asteroids(map)
  map_i <- relative_coord(x = map, w = station)
  stats <- calculate_detected(map_i)
  stats <- rotation_order(x = stats)
  vap <- list()
  while (nrow(stats) > 0){
    rotation <- stats %>%
      group_by(rotgroup, ratio) %>% 
      summarise(manh = min(manh))
    vap[[length(vap) + 1]] <-
      inner_join(stats, rotation, by = c("ratio", "manh", "rotgroup"))
    stats <- anti_join(stats, rotation, by = c("ratio", "manh", "rotgroup"))
  }
  vap <- bind_rows(vap)
  vap$vaporder <- 1:nrow(vap)
  vap$rows <- vap$rows + station[1]
  vap$cols <- vap$cols + station[2]
  return(vap)
}

rotation_order <- function(x){
  cols <- x[, "cols"]
  rows <- x[, "rows"]
  xl <- list()
  # -, 0
  xl[[1]] <- x[rows < 0 & cols == 0, ]
  # -, +
  xl[[2]] <- x[rows < 0 & cols > 0, ]
  # 0, +
  xl[[3]] <- x[rows == 0 & cols > 0, ]
  # +, +
  xl[[4]] <- x[rows > 0 & cols > 0, ]
  # +, 0
  xl[[5]] <- x[rows > 0 & cols == 0, ]
  # +, -
  xl[[6]] <- x[rows > 0 & cols < 0, ]
  # 0, -
  xl[[7]] <- x[rows == 0 & cols < 0, ]
  # -, -
  xl[[8]] <- x[rows < 0 & cols < 0, ]
  xl <- lapply(xl, function(y){
    y[order(y$ratio), ]
  })
  for (i in seq_along(xl)){
    if (nrow(xl[[i]]) > 0){
      xl[[i]][, "rotgroup"] <- i
    }
  }
  xl <- bind_rows(xl)
  return(xl)
}
