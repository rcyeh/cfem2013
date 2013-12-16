#Rprof(memory.profiling=TRUE, line.profiling=TRUE)
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
delay = 30

#dateCollection = c("19","18","17","16","15","11","12","10","09","08","05","04","03","02")
dateCollection = c("29","30")

for(date in dateCollection){
  x <- paste("ticks.201304",date,sep="")
  x <- paste(x,".h5",sep="")
  tickers <- list(h5ls(x)[2])[[1]]

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
#Total = tickers <- c('ABT','AAPL','ACN','AEP','AIG','ALL','AMGN','AMZN','APA','APC','AXP','BA','BAC','BAX','BK','BMY','BRK_dot_B','C','CAT','CL','CMCSA','COF','COP','COST','CSCO','CVS','CVX','DD','DELL','DIS','DOW','DVN','EBAY','EMC','EMR','EXC','F','FCX','FDX','GD','GE','GILD','GM','GOOG','GS','HAL','HD','HON','HPQ','IBM','INTC','JNJ','JPM','KO','LLY','LMT','LOW','MA','MCD','MDLZ','MDT','MET','MMM','MO','MON','MRK','MS','MSFT','NKE','NOV','NSC','NWSA','ORCL','OXY','PEP','PFE','PG','PM','QCOM','RTN','SBUX','SLB','SO','SPG','T','TGT','TWX','TXN','UNH','UNP','UPS','USB','UTX','V','VZ','WAG','WFC','WMB','WMT','XOM')
#use liquid stock to get start time
a <- h5read(x, paste("/ticks/",'BAC',sep='') , bit64conversion='double')
a <- subset(a, quals==0 , select=c(type,exchange_time,time, symbol, size, exchange, price,bid,ask))
  strt <- as.POSIXct(paste(date,"/04/2013 13:30:00",sep=""), format = "%d/%m/%Y %H:%M:%S")
  mid_day_start <- as.POSIXct(paste(date,"/04/2013 13:40:00",sep=""), format = "%d/%m/%Y %H:%M:%S")
  mid_day_end <- as.POSIXct(paste(date,"/04/2013 19:50:00",sep=""), format = "%d/%m/%Y %H:%M:%S")
  ed <- as.POSIXct(paste(date,"/04/2013 20:00:00",sep=""), format = "%d/%m/%Y %H:%M:%S")
  a$time <- as.POSIXct(paste(paste(date,"/04/2013",sep=""),substr(as.character(a$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
  a$timegrp[strt <= a$time & a$time < mid_day_start] = 'Early'
  a$timegrp[mid_day_start <= a$time & a$time < mid_day_end] = 'Midday'
  a$timegrp[mid_day_end <= a$time & a$time < ed] = 'Late'

#use this to get start time
strt = floor(min(a$exchange_time[a$timegrp=='Early'],na.rm=TRUE)/100)*100
mid_day_start = strt+600000
mid_day_end = mid_day_start + 22200000
ed = mid_day_end+600000
collect <- data.frame()


l = c(1:length(Total))    #Change this line to change to your listing
for(ticker in l) {
  symbol = Total[ticker]   #And change this line to change to your listing
  print(ticker)
  print(symbol)
  a <- h5read(x, paste("/ticks/",symbol,sep='') , bit64conversion='double')
  if(sum(a$type == 'T')>0){
    a <- subset(a, quals==0 , select=c(type,exchange_time,time, symbol, size, exchange, price,bid,ask))
    a$exchange_time[a$type == 'T'] <- a$exchange_time[a$type == 'T'] - delay
    a <- a[order(a$exchange_time), ]
    a$timegrp[strt <= a$exchange_time & a$exchange_time < mid_day_start] = 'Early'
    a$timegrp[mid_day_start <= a$exchange_time & a$exchange_time < mid_day_end] = 'Midday'
    a$timegrp[mid_day_end <= a$exchange_time & a$exchange_time < ed] = 'Late'
    
    if(sum(a$type == 'T')>2){
      # trade records have zero in the ask and bid price fields:
      if (a$ask[1] == 0) { a$ask[1] = a$ask[2]; }
      if (a$bid[1] == 0) { a$bid[1] = a$bid[2]; }
      # just grab the now-prior record
      # The while loop attempts to take care of cases when we have a run of Trade messages
      
      
      trade_idx <- which(a$ask == 0&a$bid == 0)
      repl_idx <- trade_idx - 1
      while(sum(repl_idx %in% trade_idx) > 0) {
        repl_idx <- ifelse(repl_idx %in% trade_idx, repl_idx - 1, repl_idx)
      }
      a$ask[trade_idx] <- a$ask[repl_idx]
      a$bid[trade_idx] <- a$bid[repl_idx]
      
      a$spread <- -a$bid + a$ask     
      a$mid <- 0.5 * (a$bid + a$ask)
      a$effectiveSpread <- abs(a$price - a$mid)*2
      a$exchange[(a$mid-0.0001)<a$price & a$price<(a$mid+0.0001) & a$exchange=='D'] = 'D1'  
      a$exchange[a$exchange=='D'] = 'D2'
      df1<-aggregate(a$spread, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),mean)
      df2<-aggregate(a$size, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),sum)
      df3<-aggregate(a$price, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),mean)
      df4<-aggregate(a$effectiveSpread, by=list(exchange=a$exchange, timegrp=a$timegrp, symbol=a$symbol),mean)
      df5 = merge(x = df1, y = df2, by =c("exchange","timegrp","symbol") )
      df6 = merge(x = df3, y = df5, by =c("exchange","timegrp","symbol") )
      df = merge(x = df4, y = df6, by =c("exchange","timegrp","symbol") )
      collect<-rbind(collect,df)
    }   
  }
}
if(dim(collect)[1]>0){
  colnames(lookup) = c('symbol','primary exchange')
  collect = merge(x = collect, y = lookup, by = "symbol")
  names = c("symbol","exchange","timegrp",'effectiveSpread','price',"spread",'size','primary exchange')
  colnames(collect) = names
  one<-subset(collect,timegrp=="Early"&(exchange=='D2'|exchange=='D1'),select=names)
  two<-subset(collect,timegrp=="Midday"&(exchange=='D2'|exchange=='D1'),select=names)
  three<-subset(collect,timegrp=="Late"&(exchange=='D2'|exchange=='D1'),select=names)
  write.csv(one, paste(date,"_early_spread_D.csv",sep=""))
  write.csv(two,paste(date,"_midday_spread_D.csv",sep=""))
  write.csv(two,paste(date,"_late_spread_D.csv",sep=""))
}
}

#Rprof(NULL)
#summaryRprof(lines="both")
