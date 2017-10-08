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

market <- rbind(marketPre1, marketPre2, marketPre3, marketPre4)
rm(marketPre1, marketPre2, marketPre3, marketPre4)

ggplot(market, aes(Date, Closings, colour = Region)) + 
  geom_line()

southMarket <- market[Region == "South"]
south.xts <- xts(x = southMarket$Closings, order.by = southMarket$Date)

autoplot(south.xts)

southMarket.start <- c(year(start(south.xts)), month(start(south.xts)))
southMarket.end <- c(year(end(south.xts)), month(end(south.xts)))
south.xts <- ts(as.numeric(south.xts), start = southMarket.start,
                   end = southMarket.end, frequency = 12)
rm(southMarket.end, southMarket.start)

south.HW <- HoltWinters(south.xts)
south.arima <- auto.arima(south.xts)
south.forecast <- forecast(south.xts, h = 12*4)
autoplot(south.HW)
autoplot(south.arima)
autoplot(south.forecast)

south.predict1 <-  predict(south.HW, n.ahead=48)
ts.plot(south.xts, south.predict1, lty = c(1:2))

south.predict2 <-  predict(south.arima, n.ahead=48)
ts.plot(south.xts, south.predict2, lty = c(1:2))
autoplot(south.predict2)
