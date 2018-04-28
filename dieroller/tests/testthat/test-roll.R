check_times <- function(x){
  if(x %% 1 != 0 |x < 0 ){stop(" 'times' must be a positive integer")}
  FALSE
}

roll <- function(die, times =1){
  check_times(times)
  rolls <- sample(die$side, size = times, replace = TRUE, prob = die$prob)
  abc <- list(rolls = rolls, sides = die$side, prob = die$prob, total = times )
  class(abc) <- "roll"
  return(abc)
}

print.roll <- function(x){
  cat("object 'roll' \n\n")
  print(x$rolls)
