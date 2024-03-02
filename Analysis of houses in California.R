train <- read.csv('train.csv')
test <- read.csv('test.csv')
train
# longitude, latitude, housingMedianAge, totalRooms, totalBedrooms, population, households, medianIncome
# medianHouseValue


# Task 1
m <- lm(medianHouseValue ~ medianIncome, data=train)
summary(m)
# residual plot
plot(fitted(m), resid(m), xlab="fitted value", ylab="residual", main="medianIncome and medianHouseValue")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m))
qqline(resid(m), col="red")
# scatter plot
plot(train$medianIncome, train$medianHouseValue,
     xlab="medianIncome", ylab="medianHouseValue", main="medianIncome and medianHouseValue") # I(medianIncome^2)?

# 95% confidence interval
summary(m)
# Estimate: 43720.2, Std.Error: 1919.1, t-value: 22.78
alpha <- 0.05
n <- nrow(train)
p <- 1
t <- qt(1-alpha/2, n-p-1)
t
cat("[", 43720.2-1919.1*t, ", ", 43720.2+1919.1*t, "]\n", sep="") # [39958.33,47482.13]


# Task 2
m <- lm(medianHouseValue ~ ., data=train)
summary(m) # adjusted R^2 : 0.6064
# variance of residuals increases -> transformation needed
# residual plot
par(mfrow = c(1, 1))
plot(fitted(m), resid(m), xlab="fitted", ylab="residuals", main="medianHouseValue")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m))
qqline(resid(m), col="red")

# transformation1 : sqrt(y)
m1 <- lm(sqrt(medianHouseValue) ~ ., data=train)
summary(m1) # 0.6357
# residual plot
plot(fitted(m1), resid(m1), xlab="fitted", ylab="residuals", main="medianHouseValue")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m1))
qqline(resid(m1), col="red")

# transformation2 : log(y)
m2 <- lm(log(medianHouseValue) ~ ., data=train)
summary(m2) # 0.6379
# residual plot
plot(fitted(m2), resid(m2), xlab="fitted", ylab="residuals", main="medianHouseValue")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m2)) # better than m1
qqline(resid(m2), col="red")

# transformation3 : power transformation
y <- train$medianHouseValue
gm <- exp(mean(log(y)))
lambda <- seq(0.1, 1, length.out=101)
rss <- numeric(length(lambda))
for (i in seq_along(lambda)){
  li <- lambda[i]
  y.prime <- (y^li-1)/(li*gm^(li-1))
  mi <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
             households + medianIncome, data=train)
  rss[i] <- sum(resid(mi)^2)
}
plot(lambda, rss, type='l')

n <- nrow(train)
p <- ncol(train)-1
cutoff <- min(rss)*(1+qt(0.975, n-p-1)^2/(n-p-1))
abline(h=cutoff)
range(lambda[which(rss<=cutoff)]) #[0.199, 0.244]
#y.prime
lambda <- 0.22
y.prime <- (y^lambda-1)/(lambda*gm^(lambda-1))
m3 <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
           households + medianIncome, data=train)
summary(m3) # 0.6409
# residual plot
plot(fitted(m3), resid(m3), xlab="fitted", ylab="residuals", main="medianHouseValue")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m3)) # better than m1
qqline(resid(m3), col="red")

# Model chosen
m <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
          households + medianIncome, data=train)
summary(m) # 0.6409

# longitude and latitude
m <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
          households + medianIncome + I(longitude^2) + I(latitude^2), data=train)
summary(m)

# automatic model selection
library(leaps)
m <- regsubsets(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
                  households + medianIncome, data=train, method = "exhaustive", nvmax=10)
summary(m)
round(summary(m)$adjr2, 4) # highest with 8 variables
s <- summary(m)
s$which[which.max(s$adjr2),]

# Task 3
m <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
          households + medianIncome, data=train)
test
summary(test)

test_actual = test$medianHouseValue
test_preds = predict(m, test)

library(Metrics)
test_mse = mse(test_actual, test_preds)
test_mse # 273070576362

plot(test_actual, test_preds)

# The MSE of 273070576362:
# This shows that average deviation between the predicted house price made by the model 
# and the actual house price.
# The R-square of 0.64 tells us that the predictor variables in the dataset are able to explain 64% of the
# variation in the house prices.

# special role of longitude and latitude
m <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
          households + medianIncome, data=train)
# Multicollinearity
X <- model.matrix(m)
s <- svd(X, nu = 0)
s
s$d[1] / s$d[length(s$d)] # 506537.6
round(s$v[,9], 3) # longitude, latitude, medianIncome

summary(m) # 0.6458
# residual plot
plot(fitted(m), resid(m), xlab="fitted", ylab="residuals")
abline(h=0, col="red")
# Q-Q plot
qqnorm(resid(m))
qqline(resid(m), col="red")

m <- lm(y.prime ~ longitude + latitude + housingMedianAge + totalRooms + totalBedrooms + population + 
          households + medianIncome + I(longitude^2) + I(latitude^2), data=train)
summary(m)

