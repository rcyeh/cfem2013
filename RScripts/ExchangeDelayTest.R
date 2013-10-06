library(aod)
library(fBasics)
library(car)

setwd("/Users/JiaXu/Documents/FE project 2013/RScripts")
source("parser.R")
#setwd("/Users/JiaXu/Documents/FE project 2013/RScripts/SupportFiles")
#a <- read.csv("early_clus.csv",sep=";",head=TRUE)
#write.csv(a,file="early_clus_newformat.csv")

setwd("/Users/JiaXu/Documents/FE project")
exchanges <- c('A','B','C','J','K','M','N','P','Q','W','X','Y','Z')
tickers <- c('LNG','BDBD','LNG','UUP','UYG','A','AGN','QCLN','CLMT','MTU','ODP','PPC','BBG')
l_exchanges <- length(exchanges)
delays <- seq(-0.5,3,0.5)
l_delay <- length(delays)

for(i in 1:l_exchanges){
  exchg <- exchanges[i]
  tick <- tickers[i]
  a <- h5read("ticks.20130423.h5", paste("/ticks/",tick,sep=""), bit64conversion='double')
  for(j in 1:l_delay){
    delay <- delays[j]
    a$exchange_time[a$type == 'Q'] <- a$exchange_time[a$type == 'Q'] + delay/1000
    #quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
    #trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
    ask <- a[a$ask_exchange == exchg,]
    bid <- a[a$bid_exchange == exchg,]
    trades <- a[a$type == "T",]
    bidtrade <- rbind(bid,trades)
    bidtrade <- bidtrade[order(bidtrade$exchange_time),]
    asktrade <- rbind(ask,trades)
    asktrade <- asktrade[order(asktrade$exchange_time),]
    f_bt <- filter_trades_quotes3(bidtrade)
    f_st <- filter_trades_quotes3(asktrade)
    
    
  }
  
  
}
