library(MASS)
library(ISLR)

set.seed(12)

x = scale(seq(0, 10, 0.1), center = 0, scale = 10)
y = 2 + 3 * x + rnorm(length(x))
y = scale(y, center = min(y), scale = max(y) - min(y))

plot(x, y)

lines(x,
      scale(
        2 + 3 * x,
        center = min(2 + 3 * x),
        scale = max(2 + 3 * x) - min(2 + 3 * x)
      ),
      col = 'red',
      lwd = 2)

data = as.data.frame(cbind(x, y))
colnames(data) = c('x', 'y')
attach(data)

model = lm(y ~ x, data = data)

abline(model, col = 'blue')


for (i in seq(1, 10, 1)) {
  y = 2 + 3 * x + rnorm(length(x))
  y = scale(y, center = min(y), scale = max(y) - min(y))
  
  plot(x, y)
  
  lines(x,
        scale(
          2 + 3 * x,
          center = min(2 + 3 * x),
          scale = max(2 + 3 * x) - min(2 + 3 * x)
        ),
        col = "red",
        lwd = 2)
  
  data = as.data.frame(cbind(x, y))
  colnames(data) = c('x', 'y')
  attach(data)
  
  model = lm(y ~ x, data = data)
  
  abline(model, col = 'blue')
}

# now with a polynomial regression model

for (i in seq(1, 10, 1)) {
  y = 2 + 3 * x + rnorm(length(x))
  y = scale(y, center = min(y), scale = max(y) - min(y))
  
  
  plot(x, y)
  
  lines(x,
        scale(
          2 + 3 * x,
          center = min(2 + 3 * x),
          scale = max(2 + 3 * x) - min(2 + 3 * x)
        ),
        col = "red",
        lwd = 2)
  
  data = as.data.frame(cbind(x, y))
  colnames(data) = c('x', 'y')
  attach(data)
  
  model = lm(y ~ x + I(x ^ 2), data = data)
  cf = coef(model)
  lines(x, cf[1] + x * cf[2] + (x ^ 2) * cf[3], col = 'blue')
}

# predict(model, data.frame(x = c(5)))
