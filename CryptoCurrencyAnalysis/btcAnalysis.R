library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(quantmod)
rm(bitcoinMarket, bitcoinChain, bitcoinHashes, btcDataSet, btcDataSet2)

bitcoinMarket <-
  fread("cryptocurrencypricehistory/bitcoin_price.csv")
bitcoinChain <-
  fread("btcTransactionData.csv")
bitcoinHashes <-
  fread("cryptocurrencypricehistory/bitcoin_dataset.csv")

merge1 <- 
  merge(bitcoinChain, bitcoinHashes)
btcDataSet <- 
  merge(merge1, bitcoinMarket)
rm(merge1)

