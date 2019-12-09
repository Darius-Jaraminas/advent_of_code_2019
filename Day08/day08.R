library(dplyr)

inp <- file("Input.txt") %>%
  readLines()
inp <- substring(inp, seq(1, nchar(inp)), seq(1, nchar(inp)))
inp <- as.numeric(inp)

nr <- 6
nc <- 25
N <- nr * nc

# part 1
layers <- rep(1:(length(inp) / N), each = N)
inp <- split(inp, layers)
zeros <- lapply(inp, function(x){
  sum(x == 0)
})
zeros <- unlist(zeros)
names(zeros) <- NULL
w <- which.min(zeros)
layer_x <- inp[[w]]
tb <- table(layer_x)
r <- tb["1"] * tb["2"]

# part 2
img <- rep(2, N)
for (i in seq_along(inp)){
  li <- inp[[i]]
  layer_not_transparent <- li != 2
  img_transparent <- img == 2
  visible <- layer_not_transparent & img_transparent
  img[visible] <- li[visible]
}
img <- matrix(img, nrow = nr, ncol = nc, byrow = TRUE)
