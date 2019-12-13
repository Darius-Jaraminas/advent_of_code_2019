read_moons <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- lapply(out, function(x){
    x <- gsub("<|>|x|y|z|=| ", "", x)
    x <- x %>% 
      strsplit(",") %>%
      unlist() %>%
      as.numeric()
    return(x)
  })
  out <- do.call(rbind, out)
  colnames(out) <- c("x", "y", "z")
  return(out)
}

apply_gravity <- function(pos, vel){
  N <- nrow(pos)
  co <- data.frame(
    m1 = rep(1:N, each = N),
    m2 = rep(1:N, N),
    stringsAsFactors = FALSE
  )
  co <- co[co$m1 != co$m2, ]
  for (i in 1:ncol(pos)){
    nmi <- colnames(pos)[i]
    nm1 <- paste0(nmi, 1)
    nm2 <- paste0(nmi, 2)
    co[, nm1] <- pos[co[, 1], nmi]
    co[, nm2] <- pos[co[, 2], nmi]
    nm_vel <- paste0(nmi, "_vel")
    co[, nm_vel] <- case_when(
      co[, nm1] < co[, nm2] ~ 1,
      co[, nm1] > co[, nm2] ~ -1,
      co[, nm1] == co[, nm2] ~ 0
    )
  }
  v <- co %>% 
    select(m1, contains("vel")) %>% 
    group_by(m1) %>% 
    summarise_all(sum) %>% 
    select(-m1)
  v <- vel + v
  return(v)
}

run_time <- function(pos, t){
  vel <- matrix(0, nrow = nrow(pos), ncol = ncol(pos))
  for (i in 1:t){
    vel <- apply_gravity(pos = pos, vel = vel)
    pos <- pos + unlist(vel)
  }
  return(list(pos = pos, vel = vel))
}

run_time_keep_all <- function(pos, t){
  vel <- matrix(0, nrow = nrow(pos), ncol = ncol(pos))
  r <- list()
  r[[1]] <- list(pos = pos, vel = vel)
  for (i in 1:t){
    vel <- apply_gravity(pos = pos, vel = vel)
    pos <- pos + unlist(vel)
    r[[length(r) + 1]] <- list(pos = pos, vel = vel)
  }
  return(r)
}

calculate_energy <- function(x){
  sum(rowSums(abs(x$pos)) * rowSums(abs(x$vel)))
}

extract_time_series <- function(x){
  x1 <- x[[1]][["pos"]]
  nr <- nrow(x1)
  nc <- ncol(x1)
  tseries <- list()
  for (i in 1:nr){
    for (j in 1:nc){
      xij <- lapply(x, function(y){
        y$pos[i, j]
      })
      xij <- unlist(xij)
      names(xij) <- NULL
      nmij <- paste0(colnames(x1)[j], i)
      tseries[[nmij]] <- xij
    }
  }
  tseries <- bind_cols(tseries)
  return(tseries)
}

find_pattern_length <- function(x){
  pat_length <- list()
  for (i in 1:ncol(x)){
    k <- 5
    xik <- x[1:k, ][[i]]
    xi <- x[[i]]
    pattern_found <- FALSE
    pattern_length <- 1
    while (!pattern_found){
      pattern_length <- pattern_length + 1
      pattern_found <- all(xi[pattern_length:(pattern_length + k - 1)] == xik)
    }
    xi1 <- x[1, ][[i]]
    xi <- x[[i]]
    gr <- which(xi == xi1)
    dgr <- diff(gr)
    pl <- cumsum(dgr)
    pl <- pl[pl <= pattern_length]
    pat_length[[colnames(x)[i]]] <- pl
  }
  return(pat_length)
}

prime_factorization <- function(x){
  n <- c()
  i <- 2
  r <- x
  while (prod(n) != x){
    if (!(r %% i)){
      n <- c(n, i)
      r <- r / i
      i <- 1
    }
    i <- i + 1
  }
  return(n)
}

find_orbit_length <- function(x){
  pf <- lapply(x, prime_factorization)
  max_rep <- lapply(pf, function(y){
    as.data.frame(table(y), stringsAsFactors = FALSE)
  })
  max_rep <- max_rep %>%
    bind_rows() %>%
    group_by(y) %>%
    summarise(max = max(Freq)) %>% 
    mutate(
      y = as.numeric(y)
    )
  lcm <- prod(max_rep[["y"]] ^ max_rep[["max"]])
  return(lcm)
}
