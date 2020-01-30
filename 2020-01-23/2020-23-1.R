print('Hello world')
data = read.csv('Advertising.csv')
data$Market = NULL

tv_model = lm(sales~data$TV, data = data)
radio_model = lm(sales~radio, data = data)
newspaper_model = lm(sales~newspaper, data = data)

plot(data$TV, data$sales)
abline(tv_model, col = 'blue')

plot(data$radio, data$sales)
abline(radio_model, col = 'red')

plot(data$newspaper, data$TV)
abline(newspaper_model, col = 'purple')

# Force R to ignore beta-0 and only predict beta-1
tv_model_2 = lm(sales~0+TV, data = data)
radio_model_2 = lm(sales~0+radio, data = data)
newspaper_model_2 = lm(sales~0+newspaper, data = data)

# predict(radio_model, 23)

plot(data$TV, data$sales)
abline(tv_model_2, col = 'blue')

plot(data$radio, data$sales)
abline(radio_model_2, col = 'red')

plot(data$newspaper, data$sales)
abline(newspaper_model_2, col = 'purple')

AIC(tv_model)
AIC(tv_model_2)
AIC(radio_model)
AIC(radio_model_2)
AIC(newspaper_model)
AIC(newspaper_model_2)

# special syntax to take into account all columns
# other than sales as inputs.
# equivalent to `lm(data$sales~data$TV+data$radio+data$newspaper`
model1 <- lm(data$sales~., data = data)
AIC(model1)

model2 <- lm(data$sales~data$newspaper+data$radio, data = data)
AIC(model2)

model2 <- lm(data$sales~data$newspaper+data$radio, data = data)
AIC(model2)

model3 <- lm(data$sales ~ data$TV + data$radio, data = data) # looks good
AIC(model3)

model4 <- lm(data$sales~data$newspaper+data$TV, data = data)
AIC(model4)

model5 <- lm(data$sales~0+data$TV+data$radio)
AIC(model5)

model1 <- lm(data$sales~., data = data)
AIC(model1)

summary(model1)

# data$dummy <- seq(1, 200)
data$dummy <- rep(1, 200)

AIC(model000)
summary(model000)
plot(model000)

data$dummy = NULL

library(ISLR)
data("Credit")
View(Credit)

Credit$ID = NULL
model1000 = lm(Credit$Balance~., data = Credit)
AIC(model1000)

model1001 = lm(Credit$Balance ~ Credit$Income + Credit$Age + Credit$Cards + Credit$Rating + Credit$Limit + Credit$Education, data = Credit)
AIC(model1000)
