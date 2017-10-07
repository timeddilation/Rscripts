library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)

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
          theme_classic())
  
}
plot.my.biz2 <-  function(data){
  print(ggplot(data, aes(Timestamp, Scaled, colour=variable)) +
          geom_line() +
          labs(title = "Scaled Data", y = "Price") +
          theme_classic())
}
plot.my.biz3 <-  function(data){  
  print(ggplot(data, aes(Timestamp, Scaled, colour=variable)) +
          geom_smooth() +
          labs(title = "Scaled Data", subtitle = "Smooth", y = "Price") +
          theme_classic())
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

coinsLastWeek <- subset(coins, Timestamp >= lastTradeWeek)
coinsLastWeek <- coinsLastWeek[, Scaled := scale(value), by = variable]

coinsLast60 <- subset(coins, Timestamp >= lastTrade60)
coinsLast60 <- coinsLast60[, Scaled := scale(value), by = variable]

coinsLast180 <- subset(coins, Timestamp >= lastTrade180)
coinsLast180 <- coinsLast180[, Scaled := scale(value), by = variable]

coinsFirst60d2017 <- subset(coins, Timestamp >= first2017Trade & Timestamp <= first60d2017)
coinsFirst60d2017 <- coinsFirst60d2017[, Scaled := scale(value), by = variable]

coinsFirst90d2017 <- subset(coins, Timestamp >= first2017Trade & Timestamp <= first90d2017)
coinsFirst90d2017 <- coinsFirst90d2017[, Scaled := scale(value), by = variable]

coinsFirst180d2017 <- subset(coins, Timestamp >= first2017Trade & Timestamp <= first180d2017)
coinsFirst180d2017 <- coinsFirst180d2017[, Scaled := scale(value), by = variable]

plot.my.biz1(coinsLastWeek)
plot.my.biz2(coinsLastWeek)
plot.my.biz3(coinsLastWeek)
  
plot.my.biz1(coinsLast60)
plot.my.biz2(coinsLast60)
plot.my.biz3(coinsLast60)

plot.my.biz1(coinsLast180)
plot.my.biz2(coinsLast180)
plot.my.biz3(coinsLast180)

plot.my.biz1(coinsFirst60d2017)
plot.my.biz2(coinsFirst60d2017)
plot.my.biz3(coinsFirst60d2017)

plot.my.biz1(coinsFirst90d2017)
plot.my.biz2(coinsFirst90d2017)
plot.my.biz3(coinsFirst90d2017)

plot.my.biz1(coinsFirst180d2017)
plot.my.biz2(coinsFirst180d2017)
plot.my.biz3(coinsFirst180d2017)
ggsave("plotTest.pdf", plot = last_plot(), device = "pdf", path = "plots/")
