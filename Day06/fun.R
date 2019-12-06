orbit_checksum <- function(x){
  uo <- unique(x$obj)
  nro <- numeric(length(uo))
  names(nro) <- uo
  for (o in uo){
    ch <- 1
    orb <- x[x$obj == o, "orbits"]
    while (orb != "COM"){
      ch <- ch + 1
      orb <- x[x$obj == orb, "orbits"]
    }
    nro[o] <- ch
  }
  return(nro)
}

get_path <- function(x, o){
  orb <- x[x$obj == o, "orbits"]
  p <- orb
  while (orb != "COM"){
    orb <- x[x$obj == orb, "orbits"]
    p[length(p) + 1] <- orb
  }
  return(p)
}

min_path <- function(x, from, to){
  p_you <- get_path(x, from)
  p_san <- get_path(x, to)
  w_you <- min(which(p_you %in% p_san))
  w_san <- which(p_san == p_you[w_you])
  pth <- c(p_you[1:w_you], p_san[(w_san - 1):1])
  min_pth <- length(pth) - 1
  return(min_pth)
}
