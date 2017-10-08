library(data.table)
library(ggplot2)
library(ggthemes)
library(ggfortify)
library(grid)
library(gridExtra)
library(quantmod)


bitcoin <- fread("HourlyData/vwapHourlyBTCUSD.csv")
bitcoin <- bitcoin[, Date := as.POSIXct(Timestamp, origin="1970-01-01")]
#bitcoin <- bitcoin[Date >= "2017/04/01"]
bitcoin$Timestamp <- NULL
bitcoin$Volume <- NULL

bitcoin.xts <- xts(x = bitcoin$Price, order.by = bitcoin$Date)
# bitcoin.daily <- apply.daily(bitcoin.xts, mean)
# bitcoin.monthly <- apply.monthly(bitcoin.xts, mean)


# autoplot(bitcoin.xts) + 
#   geom_smooth(aes(x = bitcoin$Date, y = bitcoin$Price))
# 
# plot(bitcoin.xts)
# abline(reg=lm(bitcoin.xts~time(bitcoin.xts))) # add mean line
# 
# plot(aggregate(bitcoin.xts, FUN=mean, by = c(month, day)))

plot(HoltWinters(bitcoin.train))

bitcoin.daily <- apply.daily(bitcoin.xts, mean)

bitcoin.start <- c(year(start(bitcoin.daily)), month(start(bitcoin.daily)), day(start(bitcoin.daily)))
bitcoin.end <- c(year(end(bitcoin.daily)), month(end(bitcoin.daily)), day(end(bitcoin.daily)))
bitcoin.train <- ts(as.numeric(bitcoin.xts), start = bitcoin.start,
                   end = bitcoin.end, frequency = 364)

# bitcoin.start <- c(year(start(bitcoin.xts)), month(start(bitcoin.xts)), day(start(bitcoin.xts)), hour(start(bitcoin.xts)))
# bitcoin.end <- c(year(end(bitcoin.xts)), month(end(bitcoin.xts)), day(end(bitcoin.xts)), hour(end(bitcoin.xts)))
# bitcoin.train <- ts(as.numeric(bitcoin.xts), start = bitcoin.start,
#                    end = bitcoin.end, frequency = 24)

bitcoin.arima <- auto.arima(bitcoin.train)
bitcoin.forecast <- forecast(bitcoin.train, h = 10)
autoplot(bitcoin.train)
autoplot(bitcoin.forecast)
autoplot(bitcoin.arima)

plot.ts(bitcoin.forecast)
