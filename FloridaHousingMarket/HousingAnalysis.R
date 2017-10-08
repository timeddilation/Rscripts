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
market <- market[, Date := as.Date(market$Date, format = "%Y/%m/%d")]
# market <- market[Date >= "2010/01/01"]
market$Total <- market$Total * 1000
market$NE <- market$NE * 1000
market$MW <- market$MW * 1000
market$S <- market$S * 1000
market$W <- market$W * 1000

marketPre1 <- market[, c("Date", "NE")]
names(marketPre1) <- c("Date", "Closings")
marketPre1 <- marketPre1[, Region := "NorthEast"]
marketPre2 <- market[, c("Date", "MW")]
names(marketPre2) <- c("Date", "Closings")
marketPre2 <- marketPre2[, Region := "MidWest"]
marketPre3 <- market[, c("Date", "S")]
names(marketPre3) <- c("Date", "Closings")
marketPre3 <- marketPre3[, Region := "South"]
marketPre4 <- market[, c("Date", "W")]
names(marketPre4) <- c("Date", "Closings")
marketPre4 <- marketPre4[, Region := "West"]
marketPre5 <- market[, c("Date", "Total")]
names(marketPre5) <- c("Date", "Closings")
marketPre5 <- marketPre5[, Region := "Total"]

market <- rbind(marketPre1, marketPre2, marketPre3, marketPre4, marketPre5)
rm(marketPre1, marketPre2, marketPre3, marketPre4, marketPre5)



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
