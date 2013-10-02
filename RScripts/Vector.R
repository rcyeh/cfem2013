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
  
  #For each aggregate, perform analytics
  #Change the A_Listed here to *_Listed to analyze on other data
  l = c(1:length(A_Listed))    #Change this line to change to your listing
  
  collect <- data.frame()
  for(ticker in l) {
    symbol = A_Listed[ticker]   #And change this line to change to your listing
    print(ticker)
    print(symbol)
    a <- h5read("ticks.20130424.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
    trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
    
    if(dim(trades)[1]!=0) {
      #If the filtered trade data is zero, which will happen for some unpopular stocks
      #We will ignore those
      trades$time <- as.POSIXct(paste('24/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
      
      #break into three group 9:40 and 15:50, NYC time
      # or convert into UTC 13:40 and UTC 19:50 
      strt <- as.POSIXct('24/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_start <- as.POSIXct('24/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_end <- as.POSIXct('24/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
      ed <- as.POSIXct('24/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")
      trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
      trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
      trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'
      trades$cout = 1  #For debuggin only, feel free to ignore this
      trades$price=round(trades$price,digits=4)
      trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, symbol, size, exchange, timegrp, cout,price))
      if(dim(trades)[1]!=0)
      {
        df<-aggregate(trades$size, by=list(exchange=trades$exchange, timegrp=trades$timegrp, symbol=trades$symbol, price=trades$price),sum)
        collect<-rbind(df,collect)
        collect<-aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp, symbol=collect$symbol,price=collect$price),sum)
      }
      
    }
  }
  fi <- aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp, symbol=collect$symbol,price=collect$price),sum)
  one<-subset(fi,timegrp=="Early",select=c("exchange","price","symbol","x"))
  two<-subset(fi,timegrp=="Midday",select=c("exchange","price","symbol","x"))
  three<-subset(fi,timegrp=="Late",select=c("exchange","price","symbol","x"))
  write.table(one, "early.csv", sep = "\t")
  write.table(two, "midday.csv", sep = "\t")
  write.table(three, "late.csv", sep = "\t")
