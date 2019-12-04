check_rules <- function(x){
  x <- substring(x, seq(1, nchar(x)), seq(1, nchar(x)))
  x <- as.numeric(x)
  ch1 <- any(x[-length(x)] == x[-1])
  ch2 <- all(x == cummax(x))
  ch <- ch1 & ch2
  return(ch)
}

check_rules_part_two <- function(x){
  x <- substring(x, seq(1, nchar(x)), seq(1, nchar(x)))
  x <- as.numeric(x)
  ch1 <- any(table(x) == 2)
  ch2 <- all(x == cummax(x))
  ch <- ch1 & ch2
  return(ch)
}



