prep_wires <- function(x){
  w <- strsplit(x, ",")
  w <- lapply(w, function(y){
    data.frame(direction = substring(y, 1, 1),
               spaces = as.numeric(substring(y, 2, nchar(y))),
               stringsAsFactors = FALSE)
  })
  return(w)
}

draw_wire <- function(w){
  l <- list()
  at <- c(0L, 0L)
  for (i in 1:nrow(w)){
    direction <- w[i, "direction"]
    spaces <- w[i, "spaces"]
    rows <- case_when(
      direction == "R" ~ rep(at[1], spaces),
      direction == "U" ~ (at[1] - 1):(at[1] - spaces),
      direction == "L" ~ rep(at[1], spaces),
      direction == "D" ~ (at[1] + 1):(at[1] + spaces)
    )
    cols <- case_when(
      direction == "R" ~ (at[2] + 1):(at[2] + spaces),
      direction == "U" ~ rep(at[2], spaces),
      direction == "L" ~ (at[2] - 1):(at[2] - spaces),
      direction == "D" ~ rep(at[2], spaces)
    )
    at <- as.integer(c(rows[spaces], cols[spaces]))
    l[[length(l) + 1]] <- data.frame(rows = rows, cols = cols,
                                     stringsAsFactors = FALSE)
  }
  l <- bind_rows(l)
  return(l)
}

find_min_dist <- function(int){
  z <- data.frame(rows = 0, cols = 0, stringsAsFactors = FALSE)
  int <- bind_rows(z, int)
  di <- dist(int, method = "manhattan")
  di <- as.matrix(di)
  r <- di[, 1]
  r <- r[r > 0]
  r <- min(r)
  return(r)
}

path_length <- function(w1, w2){
  int <- inner_join(w1, w2, by = c("rows", "cols"))
  int[["intersection"]] <- TRUE
  w1 <- left_join(w1, int, by = c("rows", "cols"))
  w2 <- left_join(w2, int, by = c("rows", "cols"))
  
  wh1 <- which(w1$intersection)
  w1 <- w1[wh1, ]
  w1[, "pos1"] <- wh1
  w1$intersection <- NULL
  
  wh2 <- which(w2$intersection)
  w2 <- w2[wh2, ]
  w2[, "pos2"] <- wh2
  w2$intersection <- NULL
  
  int$intersection <- NULL
  int <- left_join(int, w1, by = c("rows", "cols"))
  int <- left_join(int, w2, by = c("rows", "cols"))
  int[["sum"]] <- int$pos1 + int$pos2
  return(int)
}