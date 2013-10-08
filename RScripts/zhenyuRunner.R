#Please change your source file path
source("C:/cfem2013_midterm/RScripts/parser.R")

#tickers <- c('AMZN', 'BA','TIF','AGN','CAT') #,'BAC','MSFT')
tickers <- c('CAT')
dates <- c('20130423','20130424','20130425','20130426','20130429','20130430')
am_vol <- 2270014 #AMZN volume, corresponding to 60s
thres_h <- 10000

windows()
par(mfrow=c(3,2))
l_tickers <-length(tickers)

for (m in 1:l_tickers){
  tick <- tickers[m]
  for (l in 1:length(dates)){
    date = dates[l]
    
    a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/",tick,sep=""), bit64conversion='double')
    trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
    total_volume <- trades$volume[length(trades[,1])]

    #delayed_a <- cal_quotes_EMA_bid_ask(delay_quotes_xms(a,q_delay),decay)
    trades_quotes <- filter_trades_quotes3(a, thres_h)
    time_bucket <- am_vol/total_volume * 60
  
    SOI_buckets_delta_prices <- calc_SOI(time_bucket, trades_quotes,T)
  
    l_prices = length(SOI_buckets_delta_prices[,2])
    # power transformation of ^0.45
    ind_var <- (SOI_buckets_delta_prices[-1,1]*SOI_buckets_delta_prices[-1,3])
    ind_var_pred <- (SOI_buckets_delta_prices[-l_prices,1]*SOI_buckets_delta_prices[-l_prices,3])
  
    ind_var_trans <- ind_var^0.45 
    #plot(ind_var_trans, SOI_buckets_delta_prices[-1,2])
     
    #ind_var_pred_trans <- ind_var_pred^0.45
    #plot(ind_var_pred_trans, SOI_buckets_delta_prices[-1,2])
    
  
    r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~ind_var_trans))$r.squared
    #r2p <- summary(lm(SOI_buckets_delta_prices[-1,2]~ind_var_pred_trans))$r.squared
    
    t1 <- paste(tick,"_",dates[l],"\nR^2=",round(r2,4),sep="")
    plot(ind_var_trans, SOI_buckets_delta_prices[-1,2]
         ,xlab="X(t) = power(SOI(t)·Var_Price(t),0.45)",ylab=" Quotes_mid")
    title(t1)
    abline(lm(SOI_buckets_delta_prices[-1,2]~ind_var_trans),col="red")
    #t2 <- paste("T+1_",tick,"_",dates[l],"_R^2=",round(r2p,4),sep="")
    #plot(ind_var_pred_trans, SOI_buckets_delta_prices[-1,2])
    #title(t2)
    
    #dev.copy2pdf(file = paste(tick,"_",date,".pdf",sep=""))
    #plot(SOI_buckets_delta_prices[-1,1], SOI_buckets_delta_prices[-1,2])
  
    #r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-1,1]))$r.squared
    #r2p <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-l_prices,1]))$r.squared
    #print(paste(key, ": ",r2,",",r2p))
  }
  dev.copy2pdf(file = paste(tick,".pdf",sep=""))
}


#time <- seq(30,150,15)
time <- 30
#bucket <- seq(1000,5000,1000)
bucket <- 10000
#thres <- seq(1000,10000,3000) 
thres <- 10000 
delays <- seq(0,0.1,0.005)
#delays <- 0.05
ema_decay_rate <- seq(0.5,2,0.25)
l_ema <- length(ema_decay_rate)
l_delays <- length(delays)
l_time <- length(time)
l_bucket <- length(bucket)
l_thres <- length(thres)
R2s <- c()
R2s_pred <- c()
tickers <- c('BAC','MSFT')

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
            #key <- paste(tick,"_",bucket_size,"_",time_bin,"_",thres_h,"_",q_delay,"_",decay,sep="")
            key <- paste(tick,"_",decay,sep="")
            delayed_a <- cal_quotes_EMA_bid_ask(delay_quotes_xms(a,q_delay),decay)
            trades_quotes <- filter_trades_quotes3(delayed_a, thres_h)
            
            SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades_quotes,bucket_size)
            calc_SOI(100, trades_quotes)
            l_prices = length(SOI_buckets_delta_prices[,2])
            r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-1,1]))$r.squared
            r2p <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-l_prices,1]))$r.squared
            print(paste(key, ": ",r2,",",r2p))
            R2s_pred[key] = r2p
            R2s[key] = r2
          }
        }
      }
    }
    
    write.csv(sort(R2s)[1:5],file=paste(tick,"_.csv",sep=""))
    write.csv(sort(R2s_pred)[1:5],file=paste(tick,"_.csv",sep=""))
    R2s <- c()
    R2s_pred <-c()
  }
}
