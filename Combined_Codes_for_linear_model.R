
############################6.1
Races <- read.table("ScotsRaces.dat", header=TRUE)

# (a) Scatterplot
plot(Races$timeW, Races$timeM,
     xlab = "Women's Record Time (minutes)",
     ylab = "Men's Record Time (minutes)",
     main = "Men vs Women Record Times",
     pch = 19, col = "blue")

# Fit linear regression
fit <- lm(timeM ~ timeW, data = Races)
summary(fit)

# Add regression line
abline(fit, col = "red", lwd = 2)

# Prediction for Highland Fling
newdata <- data.frame(timeW = 490.05)
predict(fit, newdata)

# (b) Correlation
cor(Races$timeW, Races$timeM)

# (c) Model through origin
fit_origin <- lm(timeM ~ -1 + timeW, data = Races)
summary(fit_origin)

# Compare models
coef(fit)
coef(fit_origin)

################################### 6.3

d <- read.table("https://stat4ds.rwth-aachen.de/data/Firearms2.dat", header=TRUE)

names(d)
head(d)

#Scatterplot
plot(d$Ownership, d$Rate,
     xlab = "Gun Ownership (%)",
     ylab = "Firearm Death Rate",
     main = "Scatterplot of Ownership vs Death Rate")

#Fitting regression model
model <- lm(Rate ~ Ownership, data = d)
summary(model)

#Cook’s Distance
cooks <- cooks.distance(model)

plot(cooks, type="h",
     main="Cook's Distance",
     ylab="Cook's Distance")

#Identifing influential observation
i <- which.max(cooks)
i
d$State[i]

# Check threshold
n <- nrow(d)
4/n
max(cooks)

#Correlation BEFORE removing
cor_before <- cor(d$Ownership, d$Rate)
cor_before

#Remove influential observation
d_new <- d[-i, ]

#Correlation AFTER removing
cor_after <- cor(d_new$Ownership, d_new$Rate)
cor_after

########################################################## 6.5

covid <- read.table("Covid19.dat", header = TRUE)

# Scatterplot 1: Cases vs Day
plot(covid$day, covid$cases,
     main = "Cases vs Day",
     xlab = "Day", ylab = "Number of Cases",
     pch = 16)

# Scatterplot 2: Log(Cases) vs Day
plot(covid$day, log(covid$cases),
     main = "Log(Cases) vs Day",
     xlab = "Day", ylab = "Log(Number of Cases)",
     pch = 16)

cor(covid$day, covid$cases)

cor(covid$day, log(covid$cases))

model <- lm(log(cases) ~ day, data = covid)
summary(model)


####################################### 6.24
# Original data
x <- c(1,2,3,4,5)
y <- c(2,4,6,8,10)

cor(x,y)   # close to +1

# Add outlier
x2 <- c(x,6)
y2 <- c(y,-20)

cor(x2,y2)  # becomes negative

# Plot
plot(x2,y2, pch=16, main="Effect of Outlier on Correlation")
abline(lm(y2~x2), col="red")



