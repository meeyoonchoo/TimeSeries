# the sales of new one-family houses in the USA, Jan 1973-Nov 1995.

autoplot(hsales) + ggtitle("Sales of New One-Family Houses, USA") + xlab("Years") +
  ylab("Numbers of Houses Sold")

#Split the data set into training and test set, where is test set is the last two years of the data.
hsales_train <- window(hsales,end=c(1993,12))
hsales_test <- window(hsales,start=c(1994,1))

#Used various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
autoplot(hsales_train) +
  autolayer(meanf(hsales_train, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(hsales_train, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(hsales_train, h=24,drift = TRUE),
            series="Drift Method", PI=FALSE) +
  autolayer(snaive(hsales_train, h=24),
            series="Snaive Method", PI=FALSE) +
  autolayer(hsales_test,
            series="Actuals") +
  ggtitle("Forecasts for Sales of new one-family houses") +
  xlab("Year") + ylab("Numbers of Houses Sold") +
  guides(colour=guide_legend(title="Forecast"))

hsales_snaive <- snaive(hsales_train,h=24)
hsales_drift <- rwf(hsales_train, h=24,drift = TRUE)
hsales_naive <- naive(hsales_train, h=24)
hsales_mean <- meanf(hsales_train, h=24)

#check accuracy
accuracy(hsales_snaive, hsales_test)
accuracy(hsales_naive, hsales_test)
accuracy(hsales_drift, hsales_test)
accuracy(hsales_mean, hsales_test)

#Check the residuals of the best method if there is white noise.
checkresiduals(hsales_snaive)
