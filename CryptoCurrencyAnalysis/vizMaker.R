library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)

rm(bitcoin, ethereum, neo, litecoin, coins, coins2016)

bitcoin <-
  fread("cyptos/cryptocurrencypricehistory/bitcoin_price.csv")
ethereum <-
  fread("cyptos/cryptocurrencypricehistory/ethereum_price.csv")
neo <-
  fread("cyptos/cryptocurrencypricehistory/neo_price.csv")
litecoin <-
  fread("cyptos/cryptocurrencypricehistory/litecoin_price.csv")

# bitcoin[, Date := as.Date(Date, format="%b %d, %Y")]
# ggplot(bitcoin, aes(Date, Close)) + geom_point()

bitcoin[, currency := "bitcoin"]
ethereum[, currency := "ethereum"]
neo[, currency := "neo"]
litecoin[, currency := "litecoin"]

bitcoin[, scaled := scale(bitcoin$Close)]
ethereum[, scaled := scale(ethereum$Close)]
neo[, scaled := scale(neo$Close)]
litecoin[, scaled := scale(litecoin$Close)]

coins <- rbind(bitcoin, ethereum, neo, litecoin)

coins[, MClose := Close / max(Close), by = currency]
coins[, scaled := scale(Close), by = currency]
coins[, Date := as.Date(Date, format="%b %d, %Y")]

# coins[, Date := as.Date(Date, format="%Y %m, %d")]
# coins2016 <- subset(coins, Date > as.Date("2016/01/01"))

ggplot(coins, aes(Date, Close, color = currency)) +
  geom_line()
ggplot(coins, aes(Date, MClose, color = currency)) +
  geom_smooth() +
  geom_line()
ggplot(coins, aes(Date, scaled, color = currency)) +
  geom_smooth() +
  geom_line()