# Maryam Alipour 9612037
# alpha = 3
# Beta = 7
# f(x) = 250x^2(1-x)^6   0<X<1
# g(x) = 1
# f(x)/g(x) =  3(1-x)^2
# d/dx = 0
# f(x)/g(x)<=3 -> c=3
# f(x)/c*g(x) = 83x^2(1-x)^6 > u

n <- 1000 #need almost cn=3000 recurrence
k <- 0
j <- 0
y <-rep(0,n)

while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1) #random variate from g
  if ( 83*x^2*(1-x)^6 > u) {
    #we accept x
    k <- k + 1
    y[k] <- x
  }
}

# evaluation

p <- seq(0.1, 0.9, 0.1)

Qhat <- quantile(y, p) # quantiles of sample

Q <- qbeta(p,3,7) # theoretical quantiles

mse <- sqrt(((Qhat-Q)^2)/n)

round(rbind(Qhat, Q,mse), 5)

qqplot(rbeta(1000,3,7),y) # almost a straight line, so we can fairly assume that generated data comes from beta distribution
