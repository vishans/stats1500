# (a)
f <- function(x, y, p){
  stopifnot('P has to be in the range (-1, 1)' = (p > -1 && p < 1))
  
  (1/(2 * pi * sqrt(1 - (p^2))))* exp((-1/(2*(1 - (p^2)))) * ((x^2) - 2*p*x*y + (y^2)))
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
  persp(x,y,z, col = 'lightblue', phi = 30, theta = 30, zlab = 'f(x,y)')
  Sys.sleep(10)
}
# When p = -0.8, the correlation between the two variables is negative, meaning that they tend to move in opposite directions. The resulting 3D plot would show a surface that is elongated in one direction and compressed in the other direction, with a valley running along the line where the two variables are equal to zero.
# 
# When p = -0.5, the correlation is still negative, but not as strong. The resulting 3D plot would show a surface that is more symmetric and less elongated, with a shallow valley running along the line where the two variables are equal to zero.
# 
# When p = 0.0, there is no correlation between the two variables, and they are independent. The resulting 3D plot would show a surface that is roughly symmetric and bell-shaped, with no clear elongation or compression in any direction.
# 
# When p = 0.5, the correlation is positive, meaning that the two variables tend to move together. The resulting 3D plot would show a surface that is again elongated in one direction and compressed in the other direction, but with a peak running along the line where the two variables are equal to zero.
# 
# When p = 0.8, the correlation is even stronger, and the resulting 3D plot would be similar to the p = -0.8 case, but with the direction of elongation and compression reversed.

# (e)
x <- seq(-3,3, length.out = 100)
plot(x,f(x,1,0))
# Looks like a normal distribution with mean = 0 and variance = 1


# (f)
y <- seq(-3,3, length.out = 100)
plot(x,f(1,y,0))
