# (a)
f <- function(x){
  2*sin(sqrt(x)) - x
}

x <- seq(1,3,length.out = 100)
par(mfrow=c(1,1))
plot(x, f(x), type = 'l')

# (b)
# bisection (recursive)
bisection <- function(f, a, b, tol, xseq = c()){  
  stopifnot('a should be strictly less than b'=a < b)
  
  midPoint = (a+b)/2
  xseq <- append(xseq, midPoint)
  
  if(abs(f(midPoint)) < tol){
    print(xseq)
    return (midPoint)
  }
  else if( f(midPoint) > 0){
    bisection(f, midPoint, b, tol, xseq)
  }
  else if( f(midPoint) < 0){
    bisection(f, a, midPoint, tol, xseq)
  }
  
}


bisectionIterative <- function(f, a, b, tol){
  stopifnot('a should be strictly less than b'=a < b)
  xseq = c()
  
  midPoint = (a+b)/2
 
  while( (abs(f(midPoint)) >= tol)){
    
    xseq <- append(xseq, midPoint)
    
    
    if( f(midPoint) > 0){
      a = midPoint
      
    }
    else if( f(midPoint) < 0){
      b = midPoint
    }
    
    midPoint = (a+b)/2
  
  }
  l = list()
  l$root = midPoint
  l$xseq = xseq
  return (l)
  
}


# (c)
# newton 
newton <- function(f, a, b, tol){
  stopifnot('a should be strictly less than b'=a < b)
  functionExpression <- parse(text =deparse(f)[3])
  derivative <- D(functionExpression, 'x')
  
      xi = (a+b)/2  # choosing first best guess at the midpoint of the interval
      xseq = c()
  repeat {
  
      x <- xi
      derivativeAtXi <- eval(derivative)
      
      xnext = xi - (f(xi)/derivativeAtXi)
      xseq <- append(xseq, xnext)
      if (abs(f(xnext)) < tol){
        l = list()
        l$root = xnext
        l$xseq = xseq
        return (l)
        
      }
      
      xi <- xnext
  }
  

  
}

# (f)
graphsOfConvergence <- function(){
    a <- 1
    b <- 3
    tol <- 1e-10
    bisecX <- bisectionIterative(f, a, b, tol)
    newtonX <- newton(f, a, b, tol)
    par(mfrow=c(2,1))
    plot(1:length(bisecX$xseq), abs(bisecX$root - bisecX$xseq), type = 'l', main = 'Bisection Algorithm Covergence', xlab = 'i', ylab = '|x* - xi|', lwd=2)
    plot(1:length(newtonX$xseq), abs(newtonX$root - newtonX$xseq), type = 'l', main = "Newton's Method Algorithm Convergence",xlab = 'i', ylab = '|x* - xi|', lwd = 2)
}
# run this fn if you want to see the graphs of convergence
# for both the bisection algo (iter) and newton's method algo
# from these 2 pictures, 
# it looks like Newton's method has a much faster convergence rate
