#Please change your source file path
source("C:/cfem2013/RScripts/parser.R")

tickers <- c('AMZN', 'BA','TIF','AGN','CAT') #,'BAC','MSFT')
#tickers <- c('CAT')
dates <- c('20130423','20130424','20130425','20130426','20130429','20130430')
am_vol <- 2270014 #AMZN volume, corresponding to 60s
thres_h <- 10000

windows()
par(mfrow=c(2,2))
l_tickers <-length(tickers)
counter <- 0

for (m in 1:l_tickers){
  tick <- tickers[m]
  for (l in 1:length(dates)){
    date = dates[l]
    counter <- counter + 1
    a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/",tick,sep=""), bit64conversion='double')
    trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
    total_volume <- trades$volume[length(trades[,1])]

    #delayed_a <- cal_quotes_EMA_bid_ask(delay_quotes_xms(a,q_delay),decay)
    trades_quotes <- filter_trades_quotes3(a, thres_h)
    time_bucket <- am_vol/total_volume * 60
  
    #SOI_buckets_delta_prices <- calc_SOI(time_bucket, trades_quotes,T)
    #SOI_buckets_delta_prices <- calc_OI_by_time_buckets(60, trades_quotes, 10000, T)
    SOI_buckets_delta_prices <- calc_OI_tick_time(10000, trades_quotes,T)
    
    l_prices = length(SOI_buckets_delta_prices[,2])
    # power transformation of ^0.45
    ind_var <- SOI_buckets_delta_prices[-1,1]#*sqrt(SOI_buckets_delta_prices[-1,3])
    #ind_var_pred <- (SOI_buckets_delta_prices[-l_prices,1]*SOI_buckets_delta_prices[-l_prices,3])
    lag <- SOI_buckets_delta_prices[-l_prices,2]
    cross_long <- SOI_buckets_delta_prices[-l_prices,1]#*sqrt(SOI_buckets_delta_prices[-l_prices,3])
    cross_short <- SOI_buckets_delta_prices[-l_prices,4]#*sqrt(SOI_buckets_delta_prices[-l_prices,5])
    forward <- SOI_buckets_delta_prices[-1,2]
    #plot(ind_var_trans, SOI_buckets_delta_prices[-1,2])
     
    #ind_var_pred_trans <- ind_var_pred^0.45
    #plot(ind_var_pred_trans, SOI_buckets_delta_prices[-1,2])
    
  
    r2 <- summary(lm(forward~lag+cross_long+cross_short))$r.squared
    adjr2 <- summary(lm(forward~lag+cross_long+cross_short))$adj.r.squared
    r2_orig <- summary(lm(forward~lag))$r.squared
    
    print(paste(date, tick, r2, adjr2, r2_orig, anova(lm(forward~lag+cross_long+cross_short))$'Pr(>F)'[1]))
    #model <- lm(SOI_buckets_delta_prices[-1,2]~cross_short)
    #r2p <- summary(model)$adj.r.squared
    #coefs <- summary(model)$coef[,1]
    #xs <- coefs[1]*lag + coefs[2]*cross_short + coefs[3]*cross_long

    #t1 <- paste(tick,"_",dates[l],"\nR^2=",round(r2p,4),sep="")
    #plot(cross_short, SOI_buckets_delta_prices[-1,2]
    #     ,xlab="X(t)",ylab="Forward Log Quotes(mid) Return")
    #title(t1)
    #abline(lm(SOI_buckets_delta_prices[-1,2]~cross_short),col="red")
    
    #t2 <- paste("T+1_",tick,"_",dates[l],"_R^2=",round(r2p,4),sep="")
    #plot(ind_var_pred_trans, SOI_buckets_delta_prices[-1,2])
    #title(t2)
    
    #dev.copy2pdf(file = paste(tick,"_",date,".pdf",sep=""))
    #plot(SOI_buckets_delta_prices[-1,1], SOI_buckets_delta_prices[-1,2])
  
    #r2 <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-1,1]))$r.squared
    #r2p <- summary(lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-l_prices,1]))$r.squared
    #print(paste(key, ": ",r2,",",r2p))
    
    #if (counter %% 4==0){
    #  dev.copy2pdf(file = paste(counter,".pdf",sep=""))  
    #}
  }
  
}

a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/AMZN",sep=""), bit64conversion='double')

windows()
par(mfrow=c(2,1))

delayed_a <- cal_quotes_EMA_bid_ask(delay_quotes_xms(a,5),1.75)
delayed_a <- delay_quotes_xms(a,0)
trades_quotes <- filter_trades_quotes3(delayed_a, thres_h)

SOI_buckets_delta_prices <- calc_SOI(60, trades_quotes,F)
SOI_buckets_delta_prices <- calc_OI_tick_time(10000, trades_quotes,T)
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(60, trades_quotes, 10000, T)

r2 = summary(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3])))$adj.r.squared

plot(SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3]), SOI_buckets_delta_prices[,2]
     ,xlab="SSOI(t)",ylab="Log Quotes(mid) Return")
title(paste("Volume Bucketing with Tick Data\nR^2 = ",round(r2,4),sep=""))
#title(paste("EMA Quotes with 5s delay 2)\nR^2 = ",round(r2,4),sep=""))
abline(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]), col="red")
