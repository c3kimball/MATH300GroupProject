# Craig Kimball 12/5/2022
# Mark Matta
# Analuisa Lelis
# Juliana Patrone


gasPrices <- MonthlyGasPricesInMassachusetts$`Gas Prices`
crudeOilPrices <- MonthlyGasPricesInMassachusetts$`Crude Oil Prices`
massMinWage <- MonthlyGasPricesInMassachusetts$`Massachusetts Minimum Wage`

# Confidence Interval of Gas Price
gasPriceCritValue = qt(.975, length(gasPrices - 1))
gasPricesStdErr = sd(gasPrices)/sqrt(length(gasPrices))
marginErrorGasPrices = gasPriceCritValue * gasPricesStdErr

lowerBoundGasPrice = mean(gasPrices) - marginErrorGasPrices
upperBoundGasPrice = mean(gasPrices) + marginErrorGasPrices

# Unpooled t-test of Gas Prices and Crude Oil Prices
t.test(crudeOilPrices, gasPrices, var.equal = FALSE)

# Unpooled t-test of Gas Prices and Minimum Wage
t.test(massMinWage, gasPrices, var.equal = FALSE)

# While running the unpooled t-tests we noticed that the p-values are the same
# This could be expected from the data, or being caused from user error

# Regression Model of Gas Prices
model <- lm(gasPrices~crudeOilPrices + massMinWage)
pairs(~., data = MonthlyGasPricesInMassachusetts)
pairs(~., data = MonthlyGasPricesInMassachusetts[ ,c(-1, -1)])
plot(crudeOilPrices, gasPrices) # We want to fit a line of best fit to this graph, but don't know how
plot(massMinWage, gasPrices)

summary(model)

# Examining the residuals
qqnorm(model$residuals)
qqline(model$residuals)
hist(model$residuals)
plot(model)
cor(MonthlyGasPricesInMassachusetts[ , c(0, -1)])
# Ask about the correct syntax of this function

