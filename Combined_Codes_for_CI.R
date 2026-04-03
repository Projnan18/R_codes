################################Question 4.12  
#a Boxplots
data <- read.table("Income.dat", header=TRUE)

boxplot(income ~ race, data=data,
        col=c("lightblue","lightgreen","lightpink"),
        xlab="Race",
        ylab="Income (in $1000s)",
        main="Income Distribution by Race")
# Split groups
black <- subset(data, race=="B")$income
white <- subset(data, race=="W")$income

# (b) Equal variance CI
t.test(black, white, var.equal=TRUE, conf.level=0.90)

# (c) Unequal variance CI (Welch)
t.test(black, white, var.equal=FALSE, conf.level=0.90)
###############4.16(b)
# Data
n1 <- 1949
x1 <- 955

n2 <- 327
x2 <- 5

p1 <- x1/n1
p2 <- x2/n2

diff <- p1 - p2

SE <- sqrt((p1*(1-p1)/n1) + (p2*(1-p2)/n2))

CI_lower <- diff - 1.96*SE
CI_upper <- diff + 1.96*SE

diff
CI_lower
CI_upper
##################################4.17
set.seed(123)
y <- rt(10000, df = 3)
hist(y, breaks = 50, main = "Histogram of t(3) Data", col = "lightblue")
qqnorm(y, main = "Normal Q-Q Plot for t(3) Data")
qqline(y, col = "red", lwd = 2)

#########################################4.50(a)


set.seed(123)

n <- 20
pi_true <- 0.06
z <- qnorm(0.975)

# Function to compute Wald CI
wald_ci <- function(x, n) {
  phat <- x/n
  se <- sqrt(phat * (1 - phat) / n)
  lower <- phat - z * se
  upper <- phat + z * se
  return(c(lower, upper))
}

# Simulation
nsim <- 1000
cover <- numeric(nsim)

for (i in 1:nsim) {
  x <- rbinom(1, n, pi_true)
  ci <- wald_ci(x, n)
  cover[i] <- (ci[1] <= pi_true && ci[2] >= pi_true)
}

mean(cover)

#############################################4.50(b)
nsim <- 10000
phat_vals <- rbinom(nsim, n, pi_true) / n

hist(phat_vals, breaks = 20, main = "Sampling Distribution of p-hat")

#################################################4.50(d)

wilson_ci <- function(x, n) {
  phat <- x/n
  z <- qnorm(0.975)
  
  center <- (phat + z^2/(2*n)) / (1 + z^2/n)
  half <- z * sqrt((phat*(1-phat)/n) + (z^2/(4*n^2))) / (1 + z^2/n)
  
  lower <- center - half
  upper <- center + half
  
  return(c(lower, upper))
}

# Simulation
nsim <- 1000
cover_wilson <- numeric(nsim)

for (i in 1:nsim) {
  x <- rbinom(1, n, pi_true)
  ci <- wilson_ci(x, n)
  cover_wilson[i] <- (ci[1] <= pi_true && ci[2] >= pi_true)
}

mean(cover_wilson)

