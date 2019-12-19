read_pattern<- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- substring(out, first = 1:nchar(out), last = 1:nchar(out))
  out <- as.integer(out)
  return(out)
}

fft <- function(x, k){
  ptrn <- make_patterns(x)
  for (i in 1:k){
    x <- one_phase(x = x, ptrn = ptrn)
  }
  x <- paste(x, collapse = "")
  return(x)
}

one_phase <- function(x, ptrn){
  y <- tcrossprod(x, ptrn)
  y <- as.numeric(y)
  y <- abs(y)
  y <- y %% 10
  y <- as.integer(y)
  return(y)
}

make_patterns <- function(x, base = c(0L, 1L, 0L, -1L)){
  N <- length(x)
  pt <- matrix(0L, nrow = N, ncol = N)
  for (i in seq_along(x)){
    p <- rep(base, each = i, length.out = N + 1)
    p <- p[-1]
    pt[i, ] <- p
  }
  return(pt)
}

simple_solution <- function(x){
  off <- x[1:7] %>%
    paste(collapse = "") %>%
    as.numeric()
  xi <- x[(off + 1):length(x)]
  for (i in 1:100){
    xi <- rev(cumsum(rev(xi)))
    xi <- xi %% 10
  }
  r <- paste(xi[1:8], collapse = "")
  return(r)
}
