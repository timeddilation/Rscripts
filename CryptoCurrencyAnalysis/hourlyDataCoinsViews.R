library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)

bitcoin <- fread("HourlyData/vwapHourlyBTCUSD.csv")
names(bitcoin) <- c("Timestamp", "BtcDate", "BtcPrice", "BtcVolume")

litecoin <- fread("HourlyData/vwapHourlyLTCUSD.csv")
names(litecoin) <- c("Timestamp", "LtcDate", "LtcPrice", "LtcVolume")

ethereum <- fread("HourlyData/vwapHourlyETHUSD.csv")
names(ethereum) <- c("Timestamp", "EtcDate", "EtcPrice", "EtcVolume")

merge1 <- merge(bitcoin, litecoin, by= "Timestamp")
coins <- merge(merge1, ethereum, by= "Timestamp")
rm(merge1)
coins2017 <- subset(coins, "Timestamp" >= 1483228800)

plot.my.biz1 <-  function(data){
  print(ggplot(data, aes(Timestamp)) +
          geom_line(aes(y=data$BtcPrice), color = "red") +
          geom_line(aes(y=data$LtcPrice), color = "blue") +
          geom_line(aes(y=data$EtcPrice), color = "green"))
}
plot.my.biz2 <-  function(data){
  ggplot(data, aes(Timestamp)) +
    geom_line(aes(y=scale(data$BtcPrice)), color = "red") +
    geom_line(aes(y=scale(data$LtcPrice)), color = "blue") +
    geom_line(aes(y=scale(data$EtcPrice)), color = "green")
}
plot.my.biz3 <-  function(data){  
  ggplot(data, aes(Timestamp)) +
    geom_smooth(aes(y=scale(data$BtcPrice)), color = "red") +
    geom_smooth(aes(y=scale(data$LtcPrice)), color = "blue") +
    geom_smooth(aes(y=scale(data$EtcPrice)), color = "green")
}

plot.my.biz1(coins)

plot.my.biz1(coins2017)
plot.my.biz2(coins2017)
plot.my.biz3(coins2017)

lastTrade <- max(coins$Timestamp)
lastTradeWeek <- lastTrade - 604800
lastTrade60 <- lastTrade - 5184000
lastTrade180 <- lastTrade - 15552000
first2017Trade <- 1483228800
first90d2017 <- 1483228800 + 7776000
first180d2017 <- 1483228800 + 15552000

coinsLastWeek <- subset(coins, Timestamp >= lastTradeWeek)
coinsLast60 <- subset(coins, Timestamp >= lastTrade60)
coinsLast180 <- subset(coins, Timestamp >= lastTrade180)
coinsFirst90d2017 <- subset(coins, Timestamp >= first2017Trade & Timestamp <= first90d2017)
coinsFirst180d2017 <- subset(coins, Timestamp >= first2017Trade & Timestamp <= first180d2017)

plot.my.biz1(coinsLastWeek)
plot.my.biz2(coinsLastWeek)
plot.my.biz3(coinsLastWeek)
  
plot.my.biz1(coinsLast60)
plot.my.biz2(coinsLast60)
plot.my.biz3(coinsLast60)

plot.my.biz1(coinsLast180)
plot.my.biz2(coinsLast180)
plot.my.biz3(coinsLast180)

plot.my.biz1(coinsFirst90d2017)
plot.my.biz2(coinsFirst90d2017)
plot.my.biz3(coinsFirst90d2017)

plot.my.biz1(coinsFirst180d2017)
plot.my.biz2(coinsFirst180d2017)
plot.my.biz3(coinsFirst180d2017)
