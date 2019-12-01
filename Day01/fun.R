calculate_total_fuel <-function(x){
  ft <- numeric(length(x))
  for (i in seq_along(x)){
    fuel <- floor(x[i] / 3) - 2
    fuel_total <- 0
    while(fuel > 0){
      fuel_total <- fuel_total + fuel
      fuel <- floor(fuel / 3) - 2
    }
    ft[i] <- fuel_total
  }
  return(ft)
}