# An Introduction to Statistical Learning, Excercise 3.7.8
# Use the lm() function to perform a simple linear regression with mpg
# as the response and horsepower as the predictor. Use the summary()
# function to print the results. Comment on the output. 

# Data represents unknown values as `?`
data <- read.csv(header = TRUE, file = 'Auto.csv', na.strings = c('?'))
data$name <- NULL # don't care about the car model names
data <- na.omit(data) # omit NA values

model1 = lm(mpg ~ horsepower, data)
summary(model1)

# Plot the response and the predictor. Use the
# abline() function to display the least squares regression line.
plot(mpg ~ horsepower, data = data)
abline(model1, col = 'red')

plot(model1)


predict(model1, newdata = data.frame(horsepower = c(98)),
           interval = 'prediction', level = 0.99)
# we need interval = 'prediction', not interval = 'confidence'

# Excercise 3.7.9

pairs(data) # plot graphs of all pairs
cor(data) # finds the correlation between all columns in the data

