Rprof(memory.profiling=TRUE, line.profiling=TRUE)
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

#For each aggregate, perform analytics
#Change the A_Listed here to *_Listed to analyze on other data

strt <- as.POSIXct('24/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_start <- as.POSIXct('24/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_end <- as.POSIXct('24/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
ed <- as.POSIXct('24/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")
collect <- data.frame()

  l = c(1:length(Total))    #Change this line to change to your listing
l=1
  for(ticker in l) {
    symbol = Total[ticker]   #And change this line to change to your listing
    print(ticker)
    print(symbol)
    symbol='AAPL'
    a <- h5read("ticks.20130424.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
    a <- subset(a, quals==0 , select=c(type,exchange_time,time, symbol, size, exchange, price,bid,ask))
    a$time <- as.POSIXct(paste('24/04/2013',substr(as.character(a$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
    a$exchange_time[a$type == 'T'] <- a$exchange_time[a$type == 'T'] - 27.5
    a$timegrp[strt <= a$time & a$time < mid_day_start] = 'Early'
    a$timegrp[mid_day_start <= a$time & a$time < mid_day_end] = 'Midday'
    a$timegrp[mid_day_end <= a$time & a$time < ed] = 'Late'
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
      a$effectiveHalfSpread <- abs(a$price - a$mid) / a$halfSpread
      a$exchange[a$effectiveHalfSpread[a$exchange=='D'] < 0.0001] = 'D1'    
      a$exchange[a$exchange=='D'] = 'D2'
      df1<-aggregate(a$price, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),mean)
      df2<-aggregate(a$size, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),sum)
      df = merge(x = df1, y = df2, by =c("exchange","timegrp","symbol") )
      collect<-rbind(collect,df)

    }
  }

colnames(lookup) = c('symbol','primary exchange')
collect = merge(x = collect, y = lookup, by = "symbol")
colnames(collect) = c("symbol","exchange","timegrp","price","size",'primary exchange')
one<-subset(collect,timegrp=="Early"&exchange!='',select=c("exchange","symbol","price","size",'primary exchange'))
two<-subset(collect,timegrp=="Midday"&exchange!='',select=c("exchange","symbol","price","size",'primary exchange'))
three<-subset(collect,timegrp=="Late"&exchange!='',select=c("exchange","symbol","price","size",'primary exchange'))
write.csv(one, "early.csv")
write.csv(two, "midday.csv")
write.csv(three, "late.csv")

Rprof(NULL)
summaryRprof(lines="both")