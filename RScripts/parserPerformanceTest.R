
a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')
a[["time"]] <- as.integer(as.POSIXct(strptime(a[["time"]],"%H:%M:%OS")))
quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]


L <- 50
time <- seq(30,150,15)
bucket <- seq(1000,15000,1000)
thres <- seq(1000,10000,3000)
l_time <- length(time)
l_bucket <- length(bucket)
l_thres <- length(thres)
R2_FUCK <- c()
tickers <- c('BA')#,'DFS','TIF','AGN','CAT')
l_tickers <-length(tickers)

for (m in 1:l_tickers){
  tick <- tickers[m]
  a <- h5read("ticks.20130423.h5", paste("/ticks/",tick,sep=""), bit64conversion='double')
  options(digits.secs=6)
  a$time<-strptime(a$time,"%H:%M:%OS")
  a$time <- a$time - a$latency*0.001
  trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
  s_trades <- trades[with(trades, order(time)), ]
  for(j in 1:l_time){
    for( k in 1:l_thres){
      for(i in 1:l_bucket){
        bucket_size <- bucket[i]
        time_bin <- time[j]
        thres1 <- thres[k]
        SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,s_trades[-which(s_trades$size>thres1),],bucket_size, F, L, T)     
        l_prices = length(SOI_buckets_delta_prices[,2])
        r2 <- summary(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]*SOI_buckets_delta_prices[,3]))$adj.r.squared
        print(paste(date(),bucket_size,"_",time_bin,"_",thres1,"_",tick, ": ",r2))
        R2_FUCK[paste(bucket_size,"_",time_bin,"_",thres1,"_",tick)] = r2
      }
    }
  }
}

R2_FUCK_FUCK <- c()
for (m in 1:l_tickers){
  tick <- tickers[m]
  a <- h5read("ticks.20130423.h5", paste("/ticks/",tick,sep=""), bit64conversion='double')
  options(digits.secs=6)
  a$time<-strptime(a$time,"%H:%M:%OS")
  a$time <- a$time - a$latency*0.001
  trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
  s_trades <- trades[with(trades, order(time)), ]
  for(j in 1:l_time){
    for( k in 1:l_thres){
      for(i in 1:l_bucket){
        bucket_size <- bucket[i]
        time_bin <- time[j]
        thres1 <- thres[k]
        SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,s_trades[-which(s_trades$size>thres1),],bucket_size, F, L, T)     
        l_prices = length(SOI_buckets_delta_prices[,2])
        r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-l_prices,1]*SOI_buckets_delta_prices[-l_prices,3]))$adj.r.squared
        print(paste(date(),bucket_size,"_",time_bin,"_",thres1,"_",tick, ": ",r2))
        R2_FUCK_FUCK[paste(bucket_size,"_",time_bin,"_",thres1,"_",tick)] = r2
      }
    }
  }
}


L <- 50
time <- seq(30,180,30)
thres <- seq(10000,20000,10000)
l_time <- length(time)
l_thres <- length(thres)
R2_finer <- c()

for(j in 1:l_time){
  for( k in 1:l_thres){
      time_bin <- time[j]
      thres1 <- thres[k]
      SOI_buckets_delta_prices <- calc_SOI(time_bin, trades[-which(trades$size>thres1),])  
      l_prices = length(SOI_buckets_delta_prices[,2])
      r2 <- summary(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]))$r.squared
      print(paste(date(),time[j],"_",thres[k], ": ",r2))
      R2_finer[paste(time[j],"_",thres[k])] = r2
  }
}
