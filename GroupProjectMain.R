# Craig Kimball 12/5/2022
# Mark Matta
# Analuisa Lelis
# Juliana Patrone


gasPrices <- MonthlyGasPricesInMassachusetts$`Gas Prices`
crudeOilPrices <- MonthlyGasPricesInMassachusetts$`Crude Oil Prices`
massMinWage <- MonthlyGasPricesInMassachusetts$`Massachusetts Minimum Wage`
month <- MonthlyGasPricesInMassachusetts$Month

# Confidence Interval of Gas Price
gasPriceCritValue = qt(.975, length(gasPrices - 1))
gasPricesStdErr = sd(gasPrices)/sqrt(length(gasPrices))
marginErrorGasPrices = gasPriceCritValue * gasPricesStdErr

summary(gasPrices)

lowerBoundGasPrice = mean(gasPrices) - marginErrorGasPrices
upperBoundGasPrice = mean(gasPrices) + marginErrorGasPrices

# Regression Model of Gas Prices
model <- lm(gasPrices~crudeOilPrices + massMinWage)
pairs(~., data = MonthlyGasPricesInMassachusetts)

model1 <- lm(gasPrices~crudeOilPrices)
plot(crudeOilPrices, gasPrices)
abline(model1)

coef(model)
fitted(model)
predict(model)
predict(model, interval = "confidence")
residuals(model)
rstandard(model)


model2 <- lm(gasPrices~massMinWage)
plot(massMinWage, gasPrices)
abline(model2)

model3 <- lm(gasPrices~MonthlyGasPricesInMassachusetts$Month)
plot(month, gasPrices)
# abline(model3)

summary(model)

# Examining the residuals
qqnorm(model$residuals)
qqline(model$residuals)
hist(model$residuals)
plot(model)
plot(model1)
plot(model2)

cor(MonthlyGasPricesInMassachusetts[ , c(0, -1)])



