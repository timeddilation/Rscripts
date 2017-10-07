library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)

rm(bitcoin, ethereum, neo, litecoin, coins, coins2016, coins2017)

# Make sure to set your working dir to this file's location
# Session -> Set Wroking Directory -> To Source File Location

bitcoin <-
  fread("cryptocurrencypricehistory/bitcoin_price.csv")
ethereum <-
  fread("cryptocurrencypricehistory/ethereum_price.csv")
neo <-
  fread("cryptocurrencypricehistory/neo_price.csv")
litecoin <-
  fread("cryptocurrencypricehistory/litecoin_price.csv")

# bitcoin[, Date := as.Date(Date, format="%b %d, %Y")]
# ggplot(bitcoin, aes(Date, Close)) + geom_point()

bitcoin[, currency := "bitcoin"]
ethereum[, currency := "ethereum"]
neo[, currency := "neo"]
litecoin[, currency := "litecoin"]

coins <- rbind(bitcoin, ethereum, neo, litecoin)

coins[, MClose := Close / max(Close), by = currency]
coins[, scaled := scale(Close), by = currency]
coins[, Date := as.Date(Date, format="%b %d, %Y")]
coins[, MonthYear := format(coins$Date, "%b %Y")]
coins[, Volume := as.numeric(gsub(",", "", Volume))]
coins[, AvgMntYrVolume := mean(Volume), by = MonthYear]
names(coins) <- c("Date", "Open", "High", "Low", "Close", "Volume", "MarketCap", 
                  "currency", "MClose", "Scaled", "MonthYear", "AvgMntYrVolume")

# coins[, Date := as.Date(Date, format="%Y %m, %d")]
# coins2016 <- subset(coins, Date > as.Date("2016/01/01"))

ggplot(coins, aes(Date, Close, color = currency)) +
  geom_line()
ggplot(coins, aes(Date, MClose, color = currency)) +
  geom_smooth() +
  geom_line()
ggplot(coins, aes(Date, Scaled, color = currency)) +
  geom_smooth() +
  geom_line()

coins[, Date := as.Date(Date, format="%Y %m, %d")]
coins2017 <- subset(coins, Date > as.Date("2017/01/01"))
coins[, Date := as.Date(Date, format="%b %d, %Y")]


ggplot(coins2017, aes(Date, Close, color = currency)) +
  geom_line()
ggplot(coins2017, aes(Date, log(Close), color = currency)) +
  geom_line() +
  geom_smooth()
ggplot(coins2017, aes(Date, Scaled, color = currency)) +
  geom_smooth() +
  geom_line()
ggplot(coins2017, aes(Date, MClose, color = currency)) +
  geom_smooth() +
  geom_line()
ggplot(coins2017, aes(MonthYear, AvgMntYrVolume, color = currency)) +
  geom_bar(stat="identity")
ggplot(coins2017, aes(MonthYear, Volume, color = currency)) +
  geom_bar(data=subset(coins2017, currency=="bitcoin"), stat="identity")
