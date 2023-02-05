# a
mdnorm <- function(x, mean, standardDeviation){
  stopifnot( "Standard deviation should be nonnegative" = standardDeviation >= 0)
  
  (1/sqrt(2*pi) * standardDeviation)* exp(-(((x - mean)^2)/(2 * (standardDeviation)^2)))
}

# b
mdnorm2 <- function(x, mean = 0, standardDeviation = 1){
  stopifnot( "Standard deviation should be nonnegative" = standardDeviation >= 0)
  
  (1/sqrt(2*pi) * standardDeviation)* exp(-(((x - mean)^2)/(2 * (standardDeviation)^2)))
}

# c
mdnorm3 <- function(x, mean = 0, standardDeviation = 1, log = F){
  stopifnot( "Standard deviation should be nonnegative" = standardDeviation >= 0)
  
  if (log == T) {
    return (log( (1/sqrt(2*pi) * standardDeviation)* exp(-(((x - mean)^2)/(2 * (standardDeviation)^2)))))
  }
  
  (1/sqrt(2*pi) * standardDeviation)* exp(-(((x - mean)^2)/(2 * (standardDeviation)^2)))
}
