library(rhdf5)
symbol = 'AAPL'
setwd("C:/cfem/trunk/RScripts")
a = h5read("ticks.20130424.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
quotes = a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
trades = a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
quotes$bid_ask = quotes$ask - quotes$bid
quotes$mid = (quotes$ask + quotes$bid)/2

trades$time <- as.POSIXct(paste('24/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 
quotes$time <- as.POSIXct(paste('24/04/2013',substr(as.character(quotes$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 

#break into three group 9:40 and 15:50, NYC time
# or convert into UTC 13:40 and UTC 19:50 
strt <- as.POSIXct('24/04/2013 13:30:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_start <- as.POSIXct('24/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_end <- as.POSIXct('24/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
ed <- as.POSIXct('24/04/2013 20:00:00', format = "%d/%m/%Y %H:%M:%S")

trades$timegrp[strt <= trades$time & trades$time < mid_day_start] = 'Early'
trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'Midday'
trades$timegrp[mid_day_end <= trades$time & trades$time < ed] = 'Late'

trades <- subset(trades, quals==0 | quals==6 | quals==23 | quals==33 | quals==58 | quals==59, select=c(time, size, price, exchange, timegrp))

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
#here D means D2
attach(trades)
table1 = table(exchange,timegrp)





write.csv(trades,file = "trades.csv")