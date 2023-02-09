secant<-function(f,x0,x1=x0+0.001,eps=1e-8) {
  fx0<-f(x0)
  fx1<-f(x1)
  
  while (TRUE) {
    xstar<-x1-fx1*(x1-x0)/(fx1-fx0)
    fxstar<-f(xstar)
    
    if (abs(fxstar) < eps) {
      break
    }
    
    x0<-x1
    x1<-xstar
    fx0<-fx1
    fx1<-fxstar
  }
  
  xstar
}

myFunction <- function(x) {
  (1 / (1 + exp(-(x-1)))) - 0.5
  }

print(secant(myFunction, 0.9, 1.1))