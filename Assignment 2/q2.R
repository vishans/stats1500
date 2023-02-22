# (a)
f <- function(x, y, p){
  stopifnot('P has to be in the range (-1, 1)' = (p > -1 && p < 1))
  
  (1/(2 * pi * sqrt(1 - p^2)))* exp((-1/(2*(1 - p^2))) * (x^2 - 2*p*x*y + y^2))
}

# (b)
x <- seq(-3,3, length.out = 100)
y <- seq(-3,3, length.out = 100)
z <- outer(x,y, f, 0)
persp(x,y,z, col = 'lightblue', phi = 30, theta = 30, zlab = 'f(x,y)')

# (c)
for(i in seq(0,100,10)){
  persp(x,y,z, col = 'lightblue', phi = i, theta = i, zlab = 'f(x,y)')
  Sys.sleep(1)
}

# (d)
for(j in c(-0.8,-0.5,0.0,0.5,0.8)){
  z <- outer(x,y, f, j)
  persp(x,y,z, col = 'lightblue', phi = 45, theta = 30, zlab = paste('p = ', j))
  Sys.sleep(1)
}

# (e)
x <- seq(-3,3, length.out = 100)
plot(x,f(x,1,0))
# Looks like a normal distribution with mean = 0 and variance = 1


# (f)
y <- seq(-3,3, length.out = 100)
plot(x,f(1,y,0))
