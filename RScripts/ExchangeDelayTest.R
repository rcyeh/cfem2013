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
    quotes$exchange_time[quotes$type == 'Q'] <- quotes$exchange_time[quotes$type == 'Q'] + delay/1000
    quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
    bidd <- data.frame(price = quotes$bid[quotes$bid_exchange == exchg],exchange_time = quotes$exchange_time[quotes$bid_exchange == exchg], type = "Q")
    askd <- data.frame(price = quotes$ask[quotes$ask_exchange == exchg],exchange_time =  quotes$exchange_time[quotes$ask_exchange == exchg], type = "Q")
    trade <- data.frame(price = a$price[a$type == 'T'],exchange_time = a$exchange_time[a$type == 'T'],type = "T")
    bidtrade <- rbind(bidd,trade)
    bidtrade <- bidtrade[order(bidtrade$exchange_time),]
    asktrade <- rbind(askd,trade)
    asktrade <- asktrade[order(asktrade$exchange_time),]
    
    
    
  }
  
  
}
