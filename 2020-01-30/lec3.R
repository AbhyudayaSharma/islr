library(ISLR)
library(MASS)

Auto = read.csv("Auto.csv", header = T, na.strings = "?")
Auto=na.omit(Auto)
attach(Auto)

Auto$name=NULL

# part (a) of q8

model1 = lm(mpg~horsepower, data=Auto ) 

summary(model1)

plot( mpg ~ horsepower, Auto )
abline(model1,col='red')

predict(model1, data.frame(horsepower=c(98)), 
        interval = "prediction",level=0.65)

#=============================================

pairs(Auto) # almost too complicated to work with

fit = lm(mpg~., data=Auto )
summary(fit)

plot(fit)

cor(Auto)

# update function provides a shortcut to add extra variables to the model
#
summary( update( fit, . ~ . + horsepower:weight ) )


summary( update( fit, . ~ . + I(horsepower^2) ) )

summary(update(fit, .~.+log(horsepower)))

summary(update(fit, .~.+sqrt(horsepower)))

summary(update(fit, .~.+log(horsepower)*sqrt(horsepower)))

#============================================================

data("Carseats")
attach(Carseats)

fit_1 = lm( Sales ~ Price + Urban + US, data=Carseats )
summary( fit_1 )

fit_2 = update( fit, . ~ . - Urban )

confint( fit_2, level=0.95 )

