library(rhdf5)
library(zoo)
library(TTR)

#change path to point to the dir
setwd("C:/Users/JiongF/Desktop/Code")

#Get list of tickers
#Get list from list of nodes in hdf5 cols, then convert to a horizontal list
tickers <- t(list(h5ls("ticks.20130423.h5")[2])[[1]])

a <- h5read("ticks.20130423.h5", "/ticks/MSFT", bit64conversion='double')
quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

#Bid_ask Spread
quotes$bid_ask = quotes$ask - quotes$bid

write.table(quotes,"quotes.csv")
write.table(trades,"trades.csv")

