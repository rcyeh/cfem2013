library(rhdf5)
library(zoo)
library(TTR)
library(ggplot2)
library(rJava)
library(scales)
library(iplots)
library(plyr)

#change path to point to the R dir
setwd("C:/path/to/the/R/Dir")

#Flag to indicate whether we want to perform analysis on all tickers, or a custom list of tickers
alltickers = 0

if(alltickers == 0){
   #Read a custom list of tickers from a csv
   symbols <-read.delim("ticker_list.csv",header=F,sep=",")$V1
   #List of dates to analyze on
   dates <- c("0401","0402","0403","0404","0405","0408","0409","0410","0411","0412","0415","0416","0417","0418","0419","0422","0423","0424","0425","0426","0429","0430")
   total_D = rep(0,length(symbols))
   #day=dates[1]
   for (day in dates){
     Dvol = rep(0,length(symbols)) #D Volume
     tvol = rep(0,length(symbols)) #Total Volume
     tick_size = rep(0,length(symbols)) #Tick Size
     i = 0
     for (symbol in symbols)
     {
     print(paste(day,round(i/70,2),symbol)) #Show Progress as a percentage
     i=i+1
     result <- tryCatch({       #Catches error if the ticker is not found in the file
       a <- h5read(paste("ticks.2013",day,".h5",sep=''), paste("/ticks/",symbol,sep='') , bit64conversion='double')
       Dvol[i] = sum(a$size[a$type == 'T' & a$exchange=='D'])   #Reads off D Volume
       tvol[i] = sum(a$size[a$type == 'T'])                     #Reads off Total Volume
       bid_tick_size = min(setdiff(abs(diff(a$bid[a$type == 'Q'])),c(0)))  #Take the smallest bid change as tick size
       ask_tick_size = min(setdiff(abs(diff(a$ask[a$type == 'Q'])),c(0)))  #Take the smallest ask change as tick size
       tick_size[i] = min(bid_tick_size, ask_tick_size)                    #Tick size as the smallest bid/ask change
     }, error = function(e) {
       message(e)
       return(NA)
     })

     }
     write.table(cbind(Dvol, tvol, tick_size),paste(day,"_DVol_results.csv",sep=''))  #Writes results
   }   
}


if (alltickers == 1) {
  #Get list of tickers
  #Get list from list of nodes in hdf5 cols, then convert to a horizontal l
  tickers <- list(h5ls("ticks.20130418.h5")[2])[[1]]
  
  delay = 29  #Use optimal delay of 29ms
  collect <- data.frame()   #Store final result
  for(ticker in tickers) {
    symbol = ticker
    print(ticker)  #Print current ticker to track progress
    a <- h5read("ticks.20130418.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
    quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
    trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
    
    if(dim(trades)[1]!=0) {  
      #If the filtered trade data is zero, which will happen for some unpopular stocks
      #We will ignore those
      trades$exchange_time <- trades$exchange_time-delay    #Adjust for delay
      #Use quote as a lookup table, use time as a key, remove duplicates
      quotes<-quotes[!duplicated(quotes$exchange_time,fromLast=TRUE),]
      
      #break into three group 9:40 and 15:50, NYC time
      #or convert into UTC 13:40 and UTC 19:50 
	  #So group 1 is first ten minutes, group 2 is midday and group three is last 10 mins of trading
      trades$time <- as.POSIXct(paste('23/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
      strt <- as.POSIXct('23/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_start <- as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
      mid_day_end <- as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
      ed <- as.POSIXct('23/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")
      
	  #Assign Time group label
      trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
      trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
      trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'
      
	  #Compute bid/ask for each trade
      trades$timegrp[strt <= trades$exchange_time & trades$exchange_time < mid_day_start] = 'Early'
      trades$timegrp[mid_day_start <= trades$exchange_time & trades$exchange_time < mid_day_end] = 'Midday'
      trades$timegrp[mid_day_end <= trades$exchange_time & trades$exchange_time < ed] = 'Late'
      bid <- with(quotes, approx(exchange_time, bid, xout=trades$exchange_time,method = "constant"))$y
      ask <- with(quotes, approx(exchange_time, ask, xout=trades$exchange_time,method = "constant"))$y
      mid <- (ask+bid)/2
      trades<-cbind(trades,mid, bid, ask)
      
	  #Assign trade price relative to the quote bid/ask
	  #And place them into different buckets
      trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, symbol, size, mid, price, exchange, timegrp, bid, ask))
      trades$exchange[(trades$mid-0.0001)<trades$price & trades$price<(trades$mid+0.0001) & trades$exchange=='D'] = 'Dmid'
      trades$exchange[trades$ask<trades$price & trades$exchange=='D'] = 'Daskhi'   
      trades$exchange[trades$price<trades$bid & trades$exchange=='D'] = 'Dbidlo' 
      trades$exchange[trades$bid==trades$price & trades$exchange=='D'] = 'Dbid'   
      trades$exchange[trades$ask==trades$price & trades$exchange=='D'] = 'Dask'  
      trades$exchange[trades$mid<trades$price & trades$exchange=='D'] = 'Dmidhi'   
      trades$exchange[trades$mid>trades$price & trades$exchange=='D'] = 'Dmidlo' 
	  if(dim(trades)[1]!=0)
      {
	    #If there is trade data to aggregate
        df<-aggregate(trades$size, by=list(exchange=trades$exchange, timegrp=trades$timegrp),sum)
        collect<-rbind(df,collect)
        collect<-aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp),sum)
      }
      
     }
  }
  #Write result
  #Aggregate each individual ticker and write them into result file
  aggregate(collect$x, by=list(exchange=collect$exchange, timegrp=collect$timegrp),sum)
  write.table(collect,"A.csv")
}


