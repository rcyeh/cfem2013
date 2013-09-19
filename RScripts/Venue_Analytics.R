library(rhdf5)
library(zoo)
library(TTR)
library(ggplot2)
library(rJava)
library(scales)
library(iplots)
library(plyr)

#change path to point to the R dir
setwd("C:/Users/JiongF/Desktop/Code")

#Flag to indicate whether we want to perform analysis on all tickers
alltickers = 0

if(alltickers == 0){
   #Change symbol to fetch different symbol data
   symbol = 'YHOO'

   a <- h5read("ticks.20130423.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
   quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
   trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

   #Bid_ask Spread
   quotes$bid_ask = quotes$ask - quotes$bid
   quotes$mid = (quotes$ask + quotes$bid)/2
   
   trades$time <- as.POSIXct(paste('23/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
   quotes$time <- as.POSIXct(paste('23/04/2013',substr(as.character(quotes$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
   
   
   #break into three group 9:40 and 15:50, NYC time
   # or convert into UTC 13:40 and UTC 19:50 
   strt <- as.POSIXct('23/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
   mid_day_start <- as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
   mid_day_end <- as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
   ed <- as.POSIXct('23/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")

   trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
   trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
   trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'

   trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, size, price, exchange, timegrp))

   #Make a plot of exchange vs timegrp vs (size of trades)
   #dev.new()
   #ggplot() +
   #  geom_point(aes(x = timegrp,y = exchange,size = size),data=trades) +
   #  scale_area(guide = guide_legend())
   
   time<-c(0,trades$time)
   mid = c()
   for(i in c(2:length(time)))
   {
     print(i)
     j = i-1
     if(time[i]==time[j])  #If trade time same as the previous trade result, just copy the result
     {
       mid <- c(mid, tail(mid,n=1)) 
     }else   {#If not, get the most prevailing bid and ask, and compute the mid from it
       tmp <- sort(table(quotes$bid[  time[j]<quotes$time & quotes$time<time[i]  ]),decreasing=TRUE)[1]
       if(is.na(tmp)){
         mid <- c(mid, tail(mid,n=1))   
       }else{
         prevail_bid <- as.numeric(names(tmp))
         prevail_ask <- as.numeric(names(tmp))
         mid <- c(mid, (prevail_bid+prevail_ask)/2)
       }
     }
   }
   trades<-cbind(trades, mid)
   trades$exchange[(trades$mid-0.0001)<trades$price & trades$price<(trades$mid+0.0001) & trades$exchange=='D'] = 'D1'
   aggregate(trades$size, by=list(exchange=trades$exchange, timegrp=trades$timegrp), sum)
}

if (alltickers == 1) {
  #Get list of tickers
  #Get list from list of nodes in hdf5 cols, then convert to a horizontal l
  tickers <- list(h5ls("ticks.20130423.h5")[2])[[1]]
  
  #read in tickers to exchange
  lookup <- read.table("Ticker-Exchange-Lookup.csv",header=T,sep=",",quote="\"")
  
  #Left outer join the two tables
  ticker_with_exchange = merge(x = tickers, y = lookup, by = "name", all.x=TRUE)
  
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
  l = c(1:length(P_Listed))    #Change this line to change to your listing

  collect <- data.frame()
  for(ticker in l) {
    symbol = P_Listed[ticker]   #And change this line to change to your listing
    print(ticker)
    print(symbol)
    a <- h5read("ticks.20130423.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
    trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
    
    if(dim(trades)[1]!=0) {
      #If the filtered trade data is zero, which will happen for some unpopular stocks
      #We will ignore those
      trades$time <- as.POSIXct(paste('23/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
    
      #break into three group 9:40 and 15:50, NYC time
      # or convert into UTC 13:40 and UTC 19:50 
      strt <- as.POSIXct('23/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_start <- as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_end <- as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
      ed <- as.POSIXct('23/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")
      trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
      trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
      trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'
      trades$cout = 1  #For debuggin only, feel free to ignore this
      trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, symbol, size, exchange, timegrp, cout))
      if(dim(trades)[1]!=0)
      {
        df<-aggregate(trades$size, by=list(exchange=trades$exchange, timegrp=trades$timegrp),sum)
        collect<-rbind(df,collect)
        collect<-aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp),sum)
      }
      
     }
  }
  aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp),sum)
}


