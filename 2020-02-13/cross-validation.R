# Testing LOOCV and k-fold cross validation
library(boot)
library(ISLR)
data("Auto")

# cannot do cross-validation with lm, using glm
model = glm(mpg ~ horsepower, data = Auto)
coef(model)

# Leave-one-out cross validation
loocvError = cv.glm(data = Auto, glmfit = model)
print(loocvError$delta[1]) # Mean squared error

# find out the minimum error for different degree polynomials
errors = c()
for (i in seq(10)) {
    model = glm(mpg ~ poly(horsepower, i), data = Auto)
    errors[i] = (cv.glm(model, data = Auto))$delta[1]
}

plot(seq(10), errors)
min(errors)

# now using k-fold cross-validation
K = 5
errors = c()
for (i in seq(10)) {
  model = glm(mpg ~ poly(horsepower, i), data = Auto)
  errors[i] = (cv.glm(model, data = Auto, K = K))$delta[1]
}

plot(seq(10), errors)
min(errors)

# higher the K, better the accuracy
# LOOCV is the most accurate estimate for mean squared error