
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

check_sides <- function(sides = c(1,2,3,4,5,6)){
  if(length(sides) != 6){stop("\n'object' must be of sides of 6")}
  return('TRUE')
}

check_prob <- function(prob = rep(1/6,6)){
  if(length(prob) != 6 | !is.numeric(prob) ){stop("\n 'probability' must be a numeric vector of length 6") }
  if(any(prob) < 0 | any(prob > 1)){stop("\n value of 'probability' must be inbetween 0 and 1")}
  if(sum(prob) != 1){stop("\n 'probability' must add up to 1")}
  return("TRUE")
}

die <- function(sides = c(1,2,3,4,5,6), prob = rep(1/6,6)){
  check_sides(sides)
  check_prob(prob)
  res <- list(side = sides, prob = prob)
  class(res) <- "die"
  return(res)
}

print.die<- function(x){
  cat('object "die" \n\n')
  cd <- data.frame(
    side = x$side, prob = x$prob)
  print(cd)
}

die()

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
}
