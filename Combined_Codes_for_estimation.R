####Question 4.33
set.seed(123)

n_sim <- 100000
n <- 100

means <- numeric(n_sim)
medians <- numeric(n_sim)

for(i in 1:n_sim){
  x <- rnorm(n)
  means[i] <- mean(x)
  medians[i] <- median(x)
}

# MSE calculation
mse_mean <- mean(means^2)
mse_median <- mean(medians^2)

mse_mean
mse_median

# Plot
hist(means, main="Sampling Distribution of Mean")
hist(medians, main="Sampling Distribution of Median")
#####Question 4.34


set.seed(123)

n_sim <- 100000
n <- 100

means <- numeric(n_sim)
medians <- numeric(n_sim)

for(i in 1:n_sim){
  x <- runif(n, 0, 1)
  means[i] <- mean(x)
  medians[i] <- median(x)
}

# True value = 0.5
mse_mean <- mean((means - 0.5)^2)
mse_median <- mean((medians - 0.5)^2)

mse_mean
mse_median

