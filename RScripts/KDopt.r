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

smaller <- 0
atbid <- 0
bidmid <- 0
atmid <- 0
midask <- 0
atask <- 0
bigger <- 0

#For each aggregate, perform analytics
#Change the A_Listed here to *_Listed to analyze on other data
#l = c(1:length(A_Listed))    #Change this line to change to your listing
l=1
collect <- data.frame()
for(ticker in l) {
  symbol = A_Listed[ticker]   #And change this line to change to your listing
  print(ticker)
  print(symbol)
  symbol='ANV'
  a <- h5read("ticks.20130424.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
  quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
  trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
  
  if(dim(trades)[1]!=0) {
    trades$time <- as.POSIXct(paste('24/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
    quotes$time <- as.POSIXct(paste('24/04/2013',substr(as.character(quotes$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
    strt <- as.POSIXct('24/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
    mid_day_start <- as.POSIXct('24/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
    mid_day_end <- as.POSIXct('24/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
    ed <- as.POSIXct('24/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")
    trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
    trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
    trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'
    trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, symbol, size, exchange, timegrp,price))
    trades$price=round(trades$price,digits=6)
    
    
    if(dim(trades)[1]!=0)
    {   
      for(i in c(1:length(trades$time)))
      {
        if(trades$exchange[i]=="D"){ 
          price <- trades$price[i]
          #here the delay to match the quote is set at t=5s,goal is to optimize t
          timedif = difftime(quotes$time, trades$time[i]-5, units="secs")
          shift = max(timedif[timedif<=0])
          #match the quote
          #some stocks has very few quotes, thus need to add a shift to match the most recent quote with delay
          #also the time here is in second, somehow the millisecond is lost, may return multiple quotes at the same time, thus only take the first one
          bid <- quotes$bid[quotes$time==trades$time[i]-5+shift][1]
          ask <- quotes$ask[quotes$time==trades$time[i]-5+shift][1]
          mid <- (bid+ask)/2
          if(price<bid){smaller <- smaller+1}
          if(price==bid){atbid <- atbid+1}
          if(price>bid&price<mid-0.0001){bidmid <- bidmid+1}
          if(price>=mid-0.0001&price<=mid+0.0001){
              atmid <- atmid+1 
              trades$exchange[i]="D1"
          }
          if(price>mid+0.0001&price<ask){midask <- midask+1}
          if(price==ask){atask <- atask+1}
          if(price>ask){bigger <- bigger+1} 
        }
      
      }  
    }
  }
}
x <- c(smaller, atbid, bidmid, atmid, midask, atask,bigger) 
names(x) <- c('Smaller than bid','at bid','between bid and mid','at mid','between mid and ask','at ask','bigger than ask') 
barplot(x, las = 1, space = 0) 

Rprof(NULL)
summaryRprof(lines="both")