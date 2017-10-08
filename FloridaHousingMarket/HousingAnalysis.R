library(xts)
library(lubridate)
library(forecast)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)
library(ggthemes)

market <- fread("Monthly.csv")
market[, Date := as.Date(market$Date, format = "%Y/%m/%d")]
names(market) <- c("Date", "Total", "NorthEast", "MideWest", "South", "West")

market <- melt(market, id.vars = "Date")
names(market) <- c("Date", "Region", "Closings")
market[, Closings := Closings * 1000]



ggplot(market, aes(Date, Closings, colour = Region)) + 
  geom_line()

testMarket <- market[Region == "Total"]
testMarket.xts <- xts(x = testMarket$Closings, order.by = testMarket$Date)

autoplot(testMarket.xts)

testMarket.start <- c(year(start(testMarket.xts)), month(start(testMarket.xts)))
testMarket.end <- c(year(end(testMarket.xts)), month(end(testMarket.xts)))
testMarket.xts <- ts(as.numeric(testMarket.xts), start = testMarket.start,
                   end = testMarket.end, frequency = 12)
rm(testMarket.start, testMarket.end)

testMarket.HW <- HoltWinters(testMarket.xts)
testMarket.arima <- auto.arima(testMarket.xts)
testMarket.forecast <- forecast(testMarket.xts, h = 12*4)
autoplot(testMarket.HW)
autoplot(testMarket.arima)
autoplot(testMarket.forecast)

testMarket.predict1 <-  predict(testMarket.HW, n.ahead=48)
ts.plot(testMarket.xts, testMarket.predict1, lty = c(1:2))

testMarket.predict2 <-  predict(testMarket.arima, n.ahead=48)
autoplot(testMarket.predict2)
