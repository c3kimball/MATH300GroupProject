# Craig Kimball 12/5/2022


gasPrices <- MonthlyGasPricesInMassachusetts$`Massachusetts All Grades All Formulations Retail Gasoline Prices Dollars per Gallon`
crudeOilPrices <- MonthlyGasPricesInMassachusetts$`Cushing OK WTI Spot Price FOB Dollars per Barrel`
massMinWage <- MonthlyGasPricesInMassachusetts$`Massachusetts Minimum Wage in Dollars`

# Confidence Interval of Gas Price
gasPriceCritValue = qt(.975, length(gasPrices - 1))
gasPricesStdErr = sd(gasPrices)/sqrt(length(gasPrices))
marginErrorGasPrices = gasPriceCritValue * gasPricesStdErr

lowerBoundGasPrice = mean(gasPrices) - marginErrorGasPrices
upperBoundGasPrice = mean(gasPrices) + marginErrorGasPrices
