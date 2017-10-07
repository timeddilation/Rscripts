library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)
rm(bitcoinMarket, bitcoinChain, bitcoinHashes, btcDataSet, btcDataSet2)

bitcoinMarket <- fread("btcDataTables/bitcoin_price.csv")
bitcoinChain <- fread("btcDataTables/btcTransactionData.csv")
bitcoinHashes <- fread("btcDataTables/bitcoin_dataset.csv")

merge1 <- 
  merge(bitcoinChain, bitcoinHashes)
btcDataSet <- 
  merge(merge1, bitcoinMarket)
rm(merge1)

btcDataSet[, Date := as.Date(Date, format="%Y-%m-%d")]

ggplot(btcDataSet, aes(Date)) +
  geom_point(aes(y=btcDataSet$Transactions), color = "red") +
  geom_point(aes(y=btcDataSet$Close), color = "blue")
ggplot(btcDataSet, aes(Date)) +
  geom_smooth(aes(y=scale(btcDataSet$Transactions)), color = "red") +
  geom_smooth(aes(y=scale(btcDataSet$Close)), color = "blue")
ggplot(btcDataSet, aes(Date, Close)) +
  geom_line()
ggplot(btcDataSet, aes(Date)) +
  geom_smooth(aes(y=scale(btcDataSet$btc_hash_rate)), color = "red") +
  geom_smooth(aes(y=scale(btcDataSet$Close)), color = "blue")

sub2017.btc <- btcDataSet[Date >= as.Date("2017/01/01")]

ggplot(sub2017.btc, aes(Date)) +
  geom_line(aes(y=scale(sub2017.btc$Transactions)), color = "red") +
  geom_line(aes(y=scale(sub2017.btc$Transactions * sub2017.btc$Close)), color = "green") +
  geom_line(aes(y=scale(sub2017.btc$Close)), color = "blue")

ggplot(sub2017.btc, aes(Date)) +
  geom_line(aes(y=scale(sub2017.btc$btc_hash_rate)), color = "red") +
  geom_line(aes(y=scale(sub2017.btc$Close)), color = "blue")
ggplot(sub2017.btc, aes(Date)) +
  geom_smooth(aes(y=scale(sub2017.btc$btc_hash_rate)), color = "red") +
  geom_smooth(aes(y=scale(sub2017.btc$Close)), color = "blue")
