library(aod)
library(fBasics)
library(car)

filter_trades <- function(a, volume_limit=10000){
  q1 <- a$quals %% 256
  q2 <- floor(a$quals / 256) %% 256
  q3 <- floor(a$quals / 256 / 256) %% 256
  q4 <- floor(a$quals / 256 / 256 / 256)
  #keep <- a$type == 'T' & a$size <= volume_limit & q1 != 32 & q2 != 32 & q3 != 32 & q4 != 32 & q1 != 59 & q2 != 59 & q3 != 59 & q4 != 59
  keep <- a$type == 'Q' & q1 != 32 & q2 != 32 & q3 != 32 & q4 != 32 & q1 != 59 & q2 != 59 & q3 != 59 & q4 != 59
  trades_quotes <- a[keep | c(keep[2:length(keep)], FALSE),]
  block_trades <- which(trades_quotes$size > volume_limit && trades_quotes$type == 'Q')
  trades_quotes$size[block_trades] = volume_limit
  return (trades_quotes)
}

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
score <- data.frame(exchange = c('A','B','C','J','K','M','N','P','Q','W','X','Y','Z'),delay = 0, score = 0)

for(i in 1:l_exchanges){
  exchg <- exchanges[i]
  tick <- tickers[i]
  a <- h5read("ticks.20130423.h5", paste("/ticks/",tick,sep=""), bit64conversion='double')
  a <- data.frame(a,key=c(1:dim(a)[1]))
  for(j in 1:l_delay){
    delay <- delays[j]
    a$exchange_time[a$type == 'Q'] <- a$exchange_time[a$type == 'Q'] + delay/1000
    #quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
    #trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
    ask <- a[a$ask_exchange == exchg,]
    bid <- a[a$bid_exchange == exchg,]
    trades <- a[a$type == "T" & a$exchange == exchg,]
    bidtrade <- rbind(bid,trades)
    bidtrade <- bidtrade[order(bidtrade$exchange_time),]
    asktrade <- rbind(ask,trades)
    asktrade <- asktrade[order(asktrade$exchange_time),]
    
    f_bt <- filter_trades(filter_trades_quotes3(bidtrade))
    f_st <- filter_trades(filter_trades_quotes3(asktrade))
    
    if(f_bt$type[1] == "T" & f_st$type[1] == "T"){
            f_bt <- f_bt[-c(1,dim(f_bt)[1]),]
            f_st <- f_st[-c(1,dim(f_st)[1]),]
    }
    if(f_bt$type[1] == "T" & f_st$type[1] == "Q"){
            f_bt <- f_bt[-c(1,dim(f_bt)[1]),]
            f_st <- f_st[-c(1,2),]
    }
    if(f_bt$type[1] == "Q" & f_st$type[1] == "T"){
            f_bt <- f_bt[-c(1,2),]
            f_st <- f_st[-c(1,dim(f_st)[1]),]
    }
    
    common <- intersect(f_bt$key[f_bt$type == "T"],f_st$key[f_st$type == "T"])
    ind1 <- which(f_bt$key[f_bt$type == "T"] %in% common)
    ind2 <- which(f_st$key[f_st$type == "T"] %in% common)
    ind1_b <- ind1-1
    ind2_a <- ind2-1
    ask_price <- f_st$ask[ind2_a]
    ask_price <- f_bt$bid[ind1_b]
    trade_price <- f_st$price[ind2]

    score$exchange[j+(i-1)*l_delay] = exchg
    score$delay[j+(i-1)*l_delay] = delay
    mini = apply(data.frame(absask = abs(trade_price - ask_price),absbid = abs(trade_price - bid_price)),1,min)
    score$score[j+(i-1)*l_delay] = mean(mini^2)
  }
}
