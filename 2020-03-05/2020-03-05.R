library(ISLR)

data('Smarket')
contrasts(Smarket$Direction) # Direction is a classificational variable

# Logistic regerssion requires binomial distribution.
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
           data = Smarket, family = 'binomial')

summary(fit)

probs <- predict(fit, type = 'response')
label.pred <- rep('Down', length(Smarket[[1]]))
label.pred[probs > 0.5] = 'Up'

confusion.matrix = table(Smarket$Direction, label.pred) # confusion matrix
accuracy = (confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix)
print(paste('Accuracy:', accuracy))

# Now train on a subset of the data and test on the rest
is.training.data = (Smarket$Year < 2005)
test.data = Smarket[!is.training.data,]
fit2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
            data = Smarket, family = 'binomial', subset = is.training.data)
probabilities <- predict(fit2, test.data, type = 'response')

predicted_test_data = rep("Down", dim(Smarket.2005)[1])
predicted_test_data[probabilities > 0.5] = 'Up'
confusion.matrix = table(predicted_test_data, Smarket.2005$Direction)
accuracy = (confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix)
print(paste('Accuracy:', accuracy))

# mean is equal to the accuracy
mean(predicted_test_data == Smarket.2005$Direction)

# Now only consider more statistically significant variables
fit3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = 'binomial',
            subset = is.training.data)

probs <- predict(fit3, Smarket.2005, type = 'response')
label.pred <- rep('Down', dim(Smarket.2005)[1])
label.pred[probs > 0.5] = 'Up'

confusion.matrix = table(Smarket.2005$Direction, label.pred) # confusion matrix
accuracy = (confusion.matrix[1, 1] + confusion.matrix[2, 2]) / sum(confusion.matrix)
print(paste('Accuracy:', accuracy))
