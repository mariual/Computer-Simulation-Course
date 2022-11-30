# generate 1000 random variables, uniformly distributed between 0 and 7 
x = runif(1000, min = 0, max = 7)

x

# We approximate the distribution function for X∼U(0,7) and plot the results as a histogram.
hist(x, freq = FALSE, xlab = 'x', density = 20)

# uniform density distribution function for the interval [0,7], by applying the dunif() function.
curve(dunif(x, min = 0, max = 7), 
      from = -10, to = 10, 
      n = 100000, 
      col = "darkblue", 
      lwd = 2, 
      add = TRUE, 
      yaxt = "n",
      ylab = 'probability')
plot(ecdf(x))

# calculating variance
var = function(x) {
  m = mean(x)
  mean((m-x)^2)
}

var(x)

# calculating mean
expected = function(x){
  sum(x) / length(x)
}

expected(x)

# # generate 1000 random variables, uniformly distributed between 0 and 49
Y = runif(1000, min = 0, max = 49)

# We approximate the distribution function for Y∼U(0,49) and plot the results as a histogram.
hist(Y, freq = FALSE, xlab = 'x', density = 20)

# uniform density distribution function for the interval [0,7], by applying the dunif() function.
curve(dunif(Y, min = 0, max = 49), 
      from = -10, to = 10, 
      n = 100000, 
      col = "darkblue", 
      lwd = 2, 
      add = TRUE, 
      yaxt = "n",
      ylab = 'probability')

# calculating variance
var = function(x) {
  m = mean(x)
  mean((m-x)^2)
}

var(Y)

# calculating mean
expected = function(x){
  sum(x) / length(x)
}

expected(Y)
