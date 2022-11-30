n=1000 # sample size
u=runif(n)
u1=1-u
e1=-log(u1)
hist(e1,probability =T) # Use option probability =T to plot relative frequency histogram
x=seq(0,10,0.1)
lines(x,dexp(x,7))


# My code:

N = seq(1000,200000, 1000) # Different sample size, N = 1000, 2000, 3000, ..., 200000
U = lapply(N, runif)

MyU1 = function(x){
  lapply(x, function(x)1-x)
}
U1 = lapply(U, MyU1)

MyE1 = function(x){
  lapply(x, function(x)-log(x))
}
E1 = lapply(U1, MyE1)

# plot average
average = lapply(E1, function(x)mean(unlist(x)))
plot(N, average, type = "b", pch = 19, col = "red", xlab = "N", ylab = "Average")

# plot variance
variance = lapply(E1, function(x)var(unlist(x)))
plot(N, variance, type = "b", pch = 19, col = "blue", xlab = "N", ylab = "Variance")
