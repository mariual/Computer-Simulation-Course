# Maryam Alipour | 9612037

# First Approach
n <- 16
a = rnorm(n, 7, 2)

new <- a

for(j in 1:n){  
  new[j] <- ((new[j]-1)/sqrt(2)) 
}

c = c(mean(new)-((1.96*2)/4), mean(new)+((1.96*2)/4))
UCL <- (15) * var(a) / qchisq(0.05, df=15)


R <- numeric(10000) 
for (b in 1:10000) { 
  i <- sample(a, size = 10, replace = TRUE) 
  R[b] <- mean(i) 
} 

R2 = sort(R) 
c= c(R2[10000 * 0.025], R2[10000 * 0.975])


R <- numeric(10000) 
for (b in 1:10000) { 
  i <- sample(a, size = 10, replace = TRUE) 
  R[b] <- var(i) 
} 

R2 = sort(R) 
c= c(R2[10000 * 0.025], R2[10000 * 0.975])









# Second Approach

# The CI formula when the standard deviation of the population is unknown:
CI_t <- function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  Margin_Error <- qt(ci + (1 - ci)/2, df = length(x) - 1) * sd(x)/sqrt(length(x))
  output <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )
  return(output)
}


# The CI formula when the standard deviation of the population is known:
CI_z <- function (x, ci = 0.95)
{
  `%>%` <- magrittr::`%>%`
  standard_deviation <- sd(x)
  sample_size <- length(x)
  Margin_Error <- abs(qnorm((1-ci)/2))* standard_deviation/sqrt(sample_size)
  output <- data.frame( sample_size=length(x), Mean=mean(x), sd=sd(x),
                        Margin_Error=Margin_Error,
                        'CI lower limit'=(mean(x) - Margin_Error),
                        'CI Upper limit'=(mean(x) + Margin_Error)) %>%
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:6 )
  return(output)
}

# Set seed to student number
set.seed(9612037)

x1 <- rnorm(16, mean=7, sd=2)
print(CI_z(x1, 0.95))

x2 <- rnorm(16, mean=7)
print(CI_t(x2, 0.95))


