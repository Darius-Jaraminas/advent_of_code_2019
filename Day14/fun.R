read_reactions <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>% 
    strsplit(" => ")
  close(con)
  input <- lapply(out, process_reaction, k = 1)
  input <- bind_rows(input, .id = "reaction")
  input$io <- "input"
  
  output <- lapply(out, process_reaction, k = 2)
  output <- bind_rows(output, .id = "reaction")
  output$io <- "output"
  
  re <- bind_rows(input, output) %>% 
    arrange(reaction, io, C) %>%
    mutate(Q = as.numeric(Q))
  return(re)
}

process_reaction <- function(x, k){
  x <- gsub(",", "", x[[k]])
  x <- strsplit(x, " ")[[1]]
  x <- matrix(x, byrow = TRUE, ncol = 2)
  x <- data.frame(x, stringsAsFactors = FALSE)
  colnames(x) <- c("Q", "C")
  return(x)
}

get_one_side <- function(x, side, other_side_value, cols){
  re <- x[(x$C == other_side_value) & (x$io != side), "reaction"]
  ch <- x[(x$reaction %in% re) & (x$io == side), cols]
  return(ch)
}

trace_back_level1 <- function(x, chemical = "FUEL"){
  level1 <- "ORE"
  needed <- get_one_side(x = x, side = "input", other_side_value = "FUEL",
                         cols = c("Q", "C"))
  in_level1 <- needed$C %in% level1
  nd_full <- list()
  while (!all(in_level1)){
    not_level1 <- needed[!(in_level1), ]
    nr <- nrow(not_level1)
    if (nr > 0){
      nd <- list()
      for (i in 1:nr){
        nd[[i]] <- calculate_needed(x = x, l = not_level1[i, ])
      }
    }
    nd_ch <- bind_rows(nd)
    ch1 <- not_level1$C %in% nd_ch$C
    if (any(ch1)){
      nd <- nd[!ch1]
      in_level1 <- in_level1 | needed$C %in% not_level1$C[ch1]
    }
    nd_details <- bind_rows(nd)
    nd_full[[length(nd_full) + 1]] <- nd_details
    nd <- nd_details[, c("Q", "C")]
    needed <- needed[in_level1, ]
    needed <- bind_rows(needed, nd)
    needed <- needed %>%
      group_by(C) %>%
      summarise_all(sum)
    in_level1 <- needed$C %in% level1
  }
  nd_full <- bind_rows(nd_full)
  return(list(ore_needed = needed, details = nd_full))
}

calculate_needed <- function(x, l){
  ch <- l[["C"]]
  q_ch <- x[x$C == ch & x$io == "output", "Q"]
  ch_needed <- l[["Q"]]
  n_react <- ceiling(ch_needed / q_ch)
  rnr <- x[x$C == ch & x$io == "output", "reaction"]
  nd <- x[x$reaction == rnr & x$io == "input", c("Q", "C")]
  nd$reaction <- rnr
  nd$Q <- nd$Q * n_react
  nd$n_react <- n_react
  nd$needed <- ch_needed
  nd$produced <- q_ch * n_react
  return(nd)
}

trace_back_N <- function(x, N, chemical = "FUEL"){
  level1 <- "ORE"
  needed <- get_one_side(x = x, side = "input", other_side_value = "FUEL",
                         cols = c("Q", "C"))
  needed$Q <- needed$Q * N
  in_level1 <- needed$C %in% level1
  nd_full <- list()
  while (!all(in_level1)){
    not_level1 <- needed[!(in_level1), ]
    nr <- nrow(not_level1)
    if (nr > 0){
      nd <- list()
      for (i in 1:nr){
        nd[[i]] <- calculate_needed(x = x, l = not_level1[i, ])
      }
    }
    nd_ch <- bind_rows(nd)
    ch1 <- not_level1$C %in% nd_ch$C
    if (any(ch1)){
      nd <- nd[!ch1]
      in_level1 <- in_level1 | needed$C %in% not_level1$C[ch1]
    }
    nd_details <- bind_rows(nd)
    nd_full[[length(nd_full) + 1]] <- nd_details
    nd <- nd_details[, c("Q", "C")]
    needed <- needed[in_level1, ]
    needed <- bind_rows(needed, nd)
    needed <- needed %>%
      group_by(C) %>%
      summarise_all(sum)
    in_level1 <- needed$C %in% level1
  }
  nd_full <- bind_rows(nd_full)
  return(list(ore_needed = needed, details = nd_full))
}

search_N <- function(x, ores){
  gr <- 2^(1:30)
  gs <- grid_search_N(x = x, grid = gr)
  w1 <- gr[max(which(gs < ores))]
  w2 <- gr[min(which(gs > ores))]
  w <- c(w1, w2)
  bs <- binary_search_N(x = x, k = ores, start = w)
  return(bs)
}

grid_search_N <- function(x, grid){
  r <- list()
  for (i in grid){
    oi <- trace_back_N(x = x, N = i)
    r[[length(r) + 1]] <- oi$ore_needed$Q
  }
  r <- unlist(r)
  return(r)
}

binary_search_N <- function(x, k, start){
  gr <- c(start, ceiling(mean(start)))
  gr <- sort(gr)
  found <- FALSE
  while (!found){
    gs <- grid_search_N(x = x, grid = gr)
    w1 <- gr[max(which(gs < k))]
    w2 <- gr[min(which(gs > k))]
    w <- c(w1, w2)
    gr <- c(w, ceiling(mean(w)))
    gr <- sort(gr)
    if (w1 == w2 - 1){
      found <- TRUE
    }
  }
  return(w1)
}
