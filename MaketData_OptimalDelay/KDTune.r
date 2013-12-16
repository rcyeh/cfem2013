
library(rhdf5)
library(zoo)
library(TTR)
library(ggplot2)
library(rJava)
library(scales)
library(iplots)
library(plyr)
#change path to point to the R dir
setwd("C:/cfem/trunk/RScripts")
TestExchange = 'N'

#Get list of tickers
#Get list from list of nodes in hdf5 cols, then convert to a horizontal l
tickers <- list(h5ls("ticks.20130424.h5")[2])[[1]]

#read in tickers to exchange
lookup <- read.table("Ticker-Exchange-Lookup.csv",header=T,sep=",",quote="\"")

#Left outer join the two tables
ticker_with_exchange = merge(x = tickers, y = lookup, by = "name")

#Aggregate the stocks by exchange
A_Listed <- subset(ticker_with_exchange, Exchange == 'A', select=c('name'))
N_Listed <- subset(ticker_with_exchange, Exchange == 'N', select=c('name'))
P_Listed <- subset(ticker_with_exchange, Exchange == 'P', select=c('name'))
Q_Listed <- subset(ticker_with_exchange, Exchange == 'Q', select=c('name'))
Z_Listed <- subset(ticker_with_exchange, Exchange == 'Z', select=c('name'))

#Remove Test Tickers out
A_Listed <- A_Listed[A_Listed!='ATEST']
N_Listed <- N_Listed[N_Listed!='NTEST']
P_Listed <- P_Listed[P_Listed!='IWM' & P_Listed!='PTEST'& P_Listed!='SPY'& P_Listed!='VXX']
Q_Listed <- Q_Listed[Q_Listed!='QQQ'& Q_Listed!='QQQC'& Q_Listed!='QQQX' & Q_Listed!='ZVZZT' & Q_Listed!='TQQQ' & Q_Listed!='ZWZZT' & Q_Listed!='ZXZZT']
Z_Listed <- Z_Listed[Z_Listed!='ZTEST']
Total = c(A_Listed,N_Listed,P_Listed,Q_Listed,Z_Listed)
testSymbol =sample(Total,300,FALSE)

#For each aggregate, perform analytics
#Change the A_Listed here to *_Listed to analyze on other data
#delay = c(-40,-30,-20,-10,-5,0,5,10,20,30,40,50,60)
delay = 0
d = c(1:length(delay)) 
outrange <- rep(0, length(delay))

for (k in d){
  atmid <- 0
  midaskOrbidmid <- 0
  atbidask <- 0
  outsideBidAsk <- 0
l = c(1:length(Total ))    #Change this line to change to your listing
  for(ticker in l) {
  symbol = Total [ticker]   #And change this line to change to your listing
  print(ticker)
  print(symbol)
  print(k)
  a <- h5read("ticks.20130425.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
  a <- subset(a, quals==0 , select=c(type,exchange_time,time, symbol, size, exchange, price,bid,ask))
  a$exchange_time[a$type == 'T'] <- a$exchange_time[a$type == 'T'] - delay[k]
  a <- a[order(a$exchange_time), ]
  if(sum(a$type == 'T')>0){
  # trade records have zero in the ask and bid price fields:
  if (a$ask[1] == 0) { a$ask[1] = a$ask[2]; }
  if (a$bid[1] == 0) { a$bid[1] = a$bid[2]; }
  # just grab the now-prior record
  # The while loop attempts to take care of cases when we have a run of Trade messages
  trade_idx <- which(a$ask == 0)
  repl_idx <- trade_idx - 1
  while(sum(repl_idx %in% trade_idx) > 0) {
    repl_idx <- ifelse(repl_idx %in% trade_idx, repl_idx - 1, repl_idx)
  }
  a$ask[trade_idx] <- a$ask[repl_idx]
  a$bid[trade_idx] <- a$bid[repl_idx]
  
  a$mid <- 0.5 * (a$bid + a$ask)
  a$halfSpread <- pmax(0.005, 0.5 * (a$ask - a$bid))
  # note I reflected prices about the mid-market. This is slightly different from what you're doing.
  a$effectiveHalfSpread <- abs(a$price - a$mid) / a$halfSpread
  #hist (a$effectiveHalfSpread[a$type == 'T'])
  atmid <-atmid+ sum(a$effectiveHalfSpread[a$exchange==TestExchange] < 0.0001)
  midaskOrbidmid <- midaskOrbidmid+ sum(a$effectiveHalfSpread[a$exchange==TestExchange] >= 0.0001 & a$effectiveHalfSpread[a$exchange==TestExchange] < 0.9999)
  atbidask <-atbidask+ sum(abs(a$effectiveHalfSpread[a$exchange==TestExchange] - 1) < 0.0001)
  outsideBidAsk <-outsideBidAsk+ sum(a$effectiveHalfSpread[a$exchange==TestExchange] >= 1.0001)
  }
}
outrange[k] = outsideBidAsk/(atmid+midaskOrbidmid+atbidask+outsideBidAsk)
}
optimized = delay[which.min(outrange)]
x <- c(atmid, midaskOrbidmid, atbidask, outsideBidAsk) 
names(x) <- c('at mid','between bid/ask and mid','at bid ask','outside range') 
barplot(x/(atmid+midaskOrbidmid+atbidask+outsideBidAsk), las = 1, space = 0) 
plot(outrange*100,type="o",xaxt = "n",xlab='Optimized delay in ms', ylab='percentage of trades outside bid/ask range')
axis(1,at=d, labels=delay)
