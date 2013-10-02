#Please change your source file path
source("C:/cfem2013_WTF/RScripts/parser.R")

#a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')

#quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

#a[["time"]] <- as.integer(as.POSIXct(strptime(a[["time"]],"%H:%M:%OS")))
#quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
#trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
dates <- c(20130423:20130426,20130429) 
l_dates <- length(dates)
tickers <- c('AMZN', 'BA','TIF','AGN','CAT')
l_tickers <-length(tickers)
for(j in 1:l_dates){
  date<-dates[j]
  setwd(paste("/Users/JiaXu/Documents/FE project/",date,sep=""))
  for(i in 1:l_tickers){
    tick <- tickers[i]
    concurrent <- read.csv(file=paste(tick,".csv",sep=""))
    names(concurrent) <- c("para","adjr2")
    predict <- read.csv(R2s_pred,file=paste(tick,"_pred.csv",sep=""))
    names(predict)<- c("para","adjr2")
    assign(paste(tick,"_",date, sep=""),data.frame(para = concurrent$para, con_adjr2 = concurrent$adjr2, pre_adjr2 = predict$adjr2))
  }
}

AGN_scores <- 0.25*(AGN_20130426[,2]+AGN_20130426[,3]+AGN_20130429[,2]+AGN_20130426[,3])
AGN_scores2 <- 0.25*(order(AGN_20130426[,2],decreasing=TRUE), )

AGN_20130426$con_rank[order(AGN_20130426[,2],decreasing=TRUE)] <- seq(1,105,1)
AGN_20130426$pre_rank[order(AGN_20130426[,3],decreasing=TRUE)] <- seq(1,105,1)
AGN_20130426$mean_rank <- 0.5*(AGN_20130426$con_rank+AGN_20130426$pre_rank)
AGN_20130426[order(AGN_20130426$mean_rank),][1:10,]

AGN_20130429$con_rank[order(AGN_20130429[,2],decreasing=TRUE)] <- seq(1,105,1)
AGN_20130429$pre_rank[order(AGN_20130429[,3],decreasing=TRUE)] <- seq(1,105,1)
AGN_20130429$mean_rank <- 0.5*(AGN_20130429$con_rank+AGN_20130429$pre_rank)
AGN_20130429[order(AGN_20130429$mean_rank),][1:10,]

AMZN_20130426$con_rank[order(AMZN_20130426[,2],decreasing=TRUE)] <- seq(1,105,1)
AMZN_20130426$pre_rank[order(AMZN_20130426[,3],decreasing=TRUE)] <- seq(1,105,1)
AMZN_20130426$mean_rank <- 0.5*(AMZN_20130426$con_rank+AMZN_20130426$pre_rank)
AMZN_20130426[order(AMZN_20130426$mean_rank),][1:10,]

AMZN_20130429$con_rank[order(AMZN_20130429[,2],decreasing=TRUE)] <- seq(1,105,1)
AMZN_20130429$pre_rank[order(AMZN_20130429[,3],decreasing=TRUE)] <- seq(1,105,1)
AMZN_20130429$mean_rank <- 0.5*(AMZN_20130429$con_rank+AMZN_20130429$pre_rank)
AMZN_20130429[order(AMZN_20130429$mean_rank),][1:10,]

BA_20130426$con_rank[order(BA_20130426[,2],decreasing=TRUE)] <- seq(1,105,1)
BA_20130426$pre_rank[order(BA_20130426[,3],decreasing=TRUE)] <- seq(1,105,1)
BA_20130426$mean_rank <- 0.5*(BA_20130426$con_rank+BA_20130426$pre_rank)
BA_20130426[order(BA_20130426$mean_rank),][1:10,]

BA_20130429$con_rank[order(BA_20130429[,2],decreasing=TRUE)] <- seq(1,105,1)
BA_20130429$pre_rank[order(BA_20130429[,3],decreasing=TRUE)] <- seq(1,105,1)
BA_20130429$mean_rank <- 0.5*(BA_20130429$con_rank+BA_20130429$pre_rank)
BA_20130429[order(BA_20130429$mean_rank),][1:10,]

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
ssss
            r2 <- summary(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]*SOI_buckets_delta_prices[,3]))$adj.r.squared
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