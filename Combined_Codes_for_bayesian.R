########## 4.77
data <- read.table("Library.dat", header = TRUE)

# Extract variable P
P <- data$P

# Sample size
n <- length(P)

# Compute a and b
a <- floor(n/2 - 1.96 * sqrt(n/4))
b <- ceiling(n/2 + 1.96 * sqrt(n/4))

a; b

# Sort data
P_sorted <- sort(P)

# Extract order statistics
lower <- P_sorted[a]
upper <- P_sorted[b]

lower
upper

# Confidence interval
c(lower, upper)
