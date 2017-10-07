# install.packages('ggthemes', dependencies = TRUE)

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)
library(ggthemes)

bitcoin <- fread("HourlyData/vwapHourlyBTCUSD.csv")
bitcoin <- bitcoin[, Currency := "Bitcoin"]
bitcoin$Date <- NULL

litecoin <- fread("HourlyData/vwapHourlyLTCUSD.csv")
litecoin <- litecoin[, Currency := "Litecoin"]
litecoin$Date <- NULL

ethereum <- fread("HourlyData/vwapHourlyETHUSD.csv")
ethereum <- ethereum[, Currency := "Ethereum"]
ethereum$Date <- NULL

coins <- rbind(bitcoin, litecoin, ethereum)
rm(bitcoin, litecoin, ethereum)
coins <- coins[, ScaledPrice := scale(Price), by = Currency]
coins <- coins[, MPrice := Price / max(Price), by = Currency]
coins <- coins[, WeekDay := weekdays(as.POSIXct(Timestamp, origin="1970-01-01"))]
coins$WeekDay <- factor(coins$WeekDay, levels= c("Sunday", "Monday", "Tuesday", 
                                                 "Wednesday", "Thursday", "Friday", "Saturday"))
agrCoinsPre1 <- coins[Timestamp >= lastTradeWeek]
agrCoinsPre2 <- aggregate(agrCoinsPre1[, 2:3], list(Currency = agrCoinsPre1$Currency, WeekDay = agrCoinsPre1$WeekDay), mean)
names(agrCoinsPre2) <- c("Currency", "WeekDay", "MeanPrice", "MeanVolume")
agrCoinsPre3 <- aggregate(agrCoinsPre1[, 2:3], list(Currency = agrCoinsPre1$Currency, WeekDay = agrCoinsPre1$WeekDay), sd)
names(agrCoinsPre3) <- c("Currency", "WeekDay", "SdPrice", "SdVolume")
agrCoins <- merge(agrCoinsPre2, agrCoinsPre3, by = c("Currency", "WeekDay"))
rm(agrCoinsPre1, agrCoinsPre2, agrCoinsPre3)

plot.my.biz1 <-  function(data){
  print(ggplot(data, aes(as.POSIXct(Timestamp, origin="1970-01-01"), Price, colour=Currency)) +
          geom_line() +
          labs(title = "Raw Data", 
               x = "Date",y = "Price") +
          scale_color_manual(values = c("#FF9900", "#000000","#C0C0C0")) +
          theme_gdocs())
  
}
plot.my.biz2 <-  function(data){
  print(ggplot(data, aes(as.POSIXct(Timestamp, origin="1970-01-01"), ScaledPrice, colour=Currency)) +
          geom_line() +
          labs(title = "Scaled Data", subtitle = "Default scale function", 
               x = "Date",y = "Price") +
          scale_color_manual(values = c("#FF9900", "#000000","#C0C0C0")) +
          theme_gdocs())
}
plot.my.biz3 <-  function(data){  
  print(ggplot(data, aes(as.POSIXct(Timestamp, origin="1970-01-01"), MPrice, colour=Currency)) +
          geom_line() +
          labs(title = "Scaled Data", subtitle = "Price / max(Price)", 
               x = "Date",y = "Price") +
          scale_color_manual(values = c("#FF9900", "#000000","#C0C0C0")) +
          theme_gdocs())
}

lastTrade <- max(coins$Timestamp)
lastTradeWeek <- lastTrade - 604800
lastTrade60 <- lastTrade - 5184000
lastTrade180 <- lastTrade - 15552000
first2017Trade <- 1483228800
first60d2017 <- 1483228800 + 5184000
first90d2017 <- 1483228800 + 7776000
first180d2017 <- 1483228800 + 15552000

plot.my.biz1(coins)

# Since beginning of 2017
plot.my.biz1(coins[Timestamp >= 1483228800])
plot.my.biz2(coins[Timestamp >= 1483228800])
plot.my.biz3(coins[Timestamp >= 1483228800])

plot.my.biz1(coins[Timestamp >= 1483228800])
plot.my.biz1(coins[Timestamp >= lastTradeWeek])
plot.my.biz1(coins[Timestamp >= lastTradeWeek & Currency == c("Litecoin", "Ethereum")])
plot.my.biz2(coins[Timestamp >= lastTradeWeek])
plot.my.biz3(coins[Timestamp >= lastTradeWeek & Currency == "Bitcoin"])
plot.my.biz3(coins[Timestamp >= lastTradeWeek & Currency == c("Litecoin", "Ethereum")])

  
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


ggplot(coins, aes(WeekDay, Volume, colour = Currency)) +
  geom_bar(stat = "Identity")
ggplot(coins[Currency == "Bitcoin" & Timestamp >= lastTrade180], aes(WeekDay, Volume)) +
  geom_point()

ggplot(agrCoins, aes(WeekDay, MeanVolume, colour = Currency)) +
  geom_point()
ggplot(agrCoins, aes(WeekDay, MeanVolume, colour = Currency)) +
  geom_point() +
  geom_errorbar(aes(ymin = MeanVolume-SdVolume, ymax = MeanVolume+SdVolume, width = 0.25))

ggplot(agrCoins[agrCoins$Currency == "Litecoin" | agrCoins$Currency == "Ethereum", ], aes(WeekDay, MeanPrice, colour = Currency)) +
  geom_point() +
  geom_errorbar(aes(ymin = MeanPrice-SdPrice, ymax = MeanPrice+SdPrice, width = 0.25))
