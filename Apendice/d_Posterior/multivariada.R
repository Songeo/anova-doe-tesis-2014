# lets first simulate a bivariate normal sample
library(MASS)
library(graphics)

mu1 <- 1  # expected value of x
mu2 <- 0	# expected value of y
sig1 <- 1	# variance of x
sig2 <- 1	# variance of y
rho <- 0	# corr(x, y)

# Some additional variables for x-axis and y-axis 
xm <- -4
xp <- 4
ym <- -4
yp <- 4

x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10))  # vector series x
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10))  # vector series y

# Core function
bivariate <- function(x,y){
  term1 <- 1 / (2 * pi * sig1 * sig2 * sqrt(1 - rho^2))
  term2 <- (x - mu1)^2 / sig1^2
  term3 <- -(2 * rho * (x - mu1)*(y - mu2))/(sig1 * sig2)
  term4 <- (y - mu2)^2 / sig2^2
  z <- term2 + term3 + term4
  term5 <- term1 * exp((-z / (2 *(1 - rho^2))))
  return(term5)
}
# Computes the density values
z <- outer(x,y,bivariate)

# Plot
persp(x, y, z, main = "Distribución Posterior",
      col="white", 
      theta = -70, phi = 25, r = 5, d = 0.2, 
      expand = 0.9,
      ltheta = 500, lphi = 500, shade = 0.1,
      ticktype = "simple",
      xlab = expression(theta_1),
      ylab = expression(theta_2),
      zlab= expression(P(t1, t2)),
      axes=T, border=T, box = T)

