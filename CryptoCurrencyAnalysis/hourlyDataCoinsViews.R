# install.packages('ggthemes', dependencies = TRUE)

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)
library("ggthemes")

bitcoin <- fread("HourlyData/vwapHourlyBTCUSD.csv")
bitcoin$Date <- NULL
bitcoin$Volume <- NULL
names(bitcoin) <- c("Timestamp", "BtcPrice")

litecoin <- fread("HourlyData/vwapHourlyLTCUSD.csv")
litecoin$Date <- NULL
litecoin$Volume <- NULL
names(litecoin) <- c("Timestamp", "LtcPrice")

ethereum <- fread("HourlyData/vwapHourlyETHUSD.csv")
ethereum$Date <- NULL
ethereum$Volume <- NULL
names(ethereum) <- c("Timestamp", "EtcPrice")

coinsPre1 <- merge(bitcoin, litecoin, by= "Timestamp")
coinsPre2 <- merge(coinsPre1, ethereum, by= "Timestamp")
coins <- melt(coinsPre2, id="Timestamp")
coins[, ScaledPrice := scale(value), by = variable]
rm(coinsPre1, coinsPre2)

plot.my.biz1 <-  function(data){
  print(ggplot(data, aes(Timestamp, value, colour=variable)) +
          geom_line() +
          labs(title = "Raw Data", y = "Price") +
          theme_hc())
  
}
plot.my.biz2 <-  function(data){
  print(ggplot(data, aes(Timestamp, ScaledPrice, colour=variable)) +
          geom_line() +
          labs(title = "Scaled Data", y = "Price") +
          theme_hc())
}
plot.my.biz3 <-  function(data){  
  print(ggplot(data, aes(Timestamp, ScaledPrice, colour=variable)) +
          geom_smooth() +
          labs(title = "Scaled Data", subtitle = "Smooth", y = "Price") +
          theme_hc())
}

coins2017 <- subset(coins, Timestamp >= 1483228800)
coins2017 <- coins2017[, Scaled := scale(value), by = variable]

plot.my.biz1(coins)

plot.my.biz1(coins2017)
plot.my.biz2(coins2017)
plot.my.biz3(coins2017)

lastTrade <- max(coins$Timestamp)
lastTradeWeek <- lastTrade - 604800
lastTrade60 <- lastTrade - 5184000
lastTrade180 <- lastTrade - 15552000
first2017Trade <- 1483228800
first60d2017 <- 1483228800 + 5184000
first90d2017 <- 1483228800 + 7776000
first180d2017 <- 1483228800 + 15552000

plot.my.biz1(coins[Timestamp >= lastTradeWeek])
plot.my.biz2(coins[Timestamp >= lastTradeWeek])
plot.my.biz3(coins[Timestamp >= lastTradeWeek])
  
plot.my.biz1(coins[Timestamp >= lastTrade60])
plot.my.biz2(coins[Timestamp >= lastTrade60])
plot.my.biz3(coins[Timestamp >= lastTrade60])

plot.my.biz1(coins[Timestamp >= lastTrade180])
plot.my.biz2(coins[Timestamp >= lastTrade180])
plot.my.biz3(coins[Timestamp >= lastTrade180])

plot.my.biz1(coins[Timestamp >= first2017Trade & Timestamp <= first60d2017])
plot.my.biz2(coins[Timestamp >= first2017Trade & Timestamp <= first60d2017])
plot.my.biz3(coins[Timestamp >= first2017Trade & Timestamp <= first60d2017])

plot.my.biz1(coins[Timestamp >= first2017Trade & Timestamp <= first90d2017])
plot.my.biz2(coins[Timestamp >= first2017Trade & Timestamp <= first90d2017])
plot.my.biz3(coins[Timestamp >= first2017Trade & Timestamp <= first90d2017])

plot.my.biz1(coins[Timestamp >= first2017Trade & Timestamp <= first180d2017])
plot.my.biz2(coins[Timestamp >= first2017Trade & Timestamp <= first180d2017])
plot.my.biz3(coins[Timestamp >= first2017Trade & Timestamp <= first180d2017])
ggsave("plotTest.pdf", plot = last_plot(), device = "pdf", path = "plots/")
