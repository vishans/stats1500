# (c)
bacteria <- read.csv('bacteria.csv')

mPrime <- function(beta){
  
  e <- function(i){
    (i*bacteria[i,2]) - (i * exp(10*i*beta))
  }
  
  
  10*sum(sapply(1:144, e))
}


mPrimePrime <- function(beta){
  e <- function(i){
    -100 * ((i)^2) * exp(10*i*beta)
  }
  sum(sapply(1:144, e))
}

m <- function(beta){
  e <- function(i){
    (10 * i * beta * bacteria[i,2]) - exp(10*i*beta) - log(factorial(bacteria[i,2]))
  }
  
  sum(sapply(1:144, e))
}


newton <- function(f, fprime, a, b, tol, maxIter = 500){
  stopifnot('a should be strictly less than b'=a < b)
  
  xi = (a+b)/2  # choosing first best guess at the midpoint of the interval
  xseq = c()
  iterCount = 0
  repeat {
    iterCount = iterCount + 1
    xnext = xi - (f(xi)/fprime(xi))
    print(xnext)
    xseq <- append(xseq, xnext)
    if ((abs(f(xnext)) < tol) || iterCount > maxIter){
      l = list()
      l$root = xnext
      l$xseq = xseq
      return (l)
      
    }
    
    xi <- xnext
  }
  
  
  
}