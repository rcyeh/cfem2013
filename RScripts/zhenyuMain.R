#Please change your source file path
source("C:/cfem2013_WTF/RScripts/parser.R")

#a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')

#quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

#a[["time"]] <- as.integer(as.POSIXct(strptime(a[["time"]],"%H:%M:%OS")))
#quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
#trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]


L <- 50
#time <- seq(30,150,15)
time <- 135
#bucket <- seq(1000,5000,1000)
bucket <- 10000
#thres <- seq(1000,10000,3000) 
thres <- 2000 
delays <- seq(0,0.1,0.005)
ema_decay_rate <- seq(0.5,1.5,0.25)
l_ema <- length(ema_decay_rate)
l_delays <- length(delays)
l_time <- length(time)
l_bucket <- length(bucket)
l_thres <- length(thres)
R2s <- c()
R2s_pred <- c()

tickers <- c('AMZN', 'BA','TIF','AGN','CAT')
l_tickers <-length(tickers)

for (m in 1:l_tickers){
  tick <- tickers[m]

  a <- h5read("ticks.20130423.h5", paste("/ticks/",tick,sep=""), bit64conversion='double')
  
  for(j in 1:l_time){
    for( k in 1:l_thres){
      for(i in 1:l_bucket){
        for (l in 1:l_delays){
          for (m in 1:l_ema){
            bucket_size <- bucket[i]
            time_bin <- time[j]
            thres_h <- thres[k]
            q_delay <- delays[l]
            decay <- ema_decay_rate[m]
            key <- paste(tick,"_",bucket_size,"_",time_bin,"_",thres_h,"_",q_delay,"_",decay,sep="")
            trades_quotes <- filter_emaquotes_trades(delay_quotes_xms(a,q_delay),decay,thres_h)
            
            SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades_quotes,bucket_size, F, L, T)     
            l_prices = length(SOI_buckets_delta_prices[,2])

            r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-1,1]*SOI_buckets_delta_prices[-1,3]))$adj.r.squared
            r2p <- summary(lm(SOI_buckets_delta_prices[-c(1,2),2]~SOI_buckets_delta_prices[-c(1,l_prices),1]*SOI_buckets_delta_prices[-c(1,l_prices),3]))$adj.r.squared

            print(paste(key, ": ",r2,",",r2p))
            R2s_pred[key] = r2p
            R2s[key] = r2
          }
        }
      }
    }
    write.csv(R2s,file=paste(tick,".csv",sep=""))
    write.csv(R2s_pred,file=paste(tick,"_pred.csv",sep=""))
    R2s <- c()
    R2s_pred <-c()
  }
}