source("PriceImprovement.R")

dates <<- c('20130423','20130424','20130425','20130426','20130429','20130430')
thres_h <<- 10000
times <<- c(0.01,0.1,0.5,1,2,5,10,20,30,40,50,60,90,120,150,180,240,300)
#calculate vol for each symbol
tickers <<- c('ABT','AAPL','ACN','AEP','AIG','ALL','AMGN','AMZN','APA','APC','AXP','BA','BAC','BAX','BK','BMY','BRK_dot_B','C','CAT','CL','CMCSA','COF','COP','COST','CSCO','CVS','CVX','DD','DELL','DIS','DOW','DVN','EBAY','EMC','EMR','EXC','F','FCX','FDX','GD','GE','GILD','GM','GOOG','GS','HAL','HD','HON','HPQ','IBM','INTC','JNJ','JPM','KO','LLY','LMT','LOW','MA','MCD','MDLZ','MDT','MET',
             'MMM','MO','MON','MRK','MS','MSFT','NKE','NOV','NSC','NWSA','ORCL','OXY','PEP','PFE','PG','PM','QCOM','RTN','SBUX','SLB',
             'SO','SPG','T','TGT','TWX','TXN','UNH','UNP','UPS','USB','UTX','V','VZ','WAG','WFC','WMB','WMT','XOM')

# This function is used for constructing cross-sectional regression
# for all the tickers within a given date
# If "use_trades" parameter is set to True, SOI and SQI will both used as regressors
# Else, only SQI will be used as a regressor
# If "exclude_d" is turned on, all D venue trades and quotes will be excluded from consideration
# This function outputs a set of files, each containing different time bucketing intervals
# The outputs of this function can be used for regression immediately, the regression are 
# for 1-bucket ahead prediction
aggregate_imbalance <- function(use_trades, exclude_d, date='20130423', lee_ready_classify=F){
  stockvols <- read.csv('SnP100stock_vols.csv', header = FALSE)
  names(stockvols) <- c('Symbol','vol','volvol') #volvol is the volatility of volatility
    
  for (j in 1:length(tickers)){
    tick = tickers[j]
    print(tick)
    b <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/",tick,sep=""), bit64conversion='double')
    if (exclude_d){
      b <- b[which(b$exchange !='D'),]
    }
    a <- filter(b)
    
    if (use_trades){
      d_a <- delay_quotes_xms(b, 0.03)
      trades_quotes <- filter_trades_quotes(d_a, 10000)   
      buy_sells <- classify_buy_sell(trades_quotes, lee_ready_classify)
      all_trades <- filter(trades_quotes, 'T', F)
      all_trades <- all_trades[-length(all_trades),]
    }
    zero_ind <- which(a$ask==0 | a$bid==0)
    if (length(zero_ind)!=0){
      a <- a[-zero_ind,]
    }
    start_ms <- a$exchange_time[1]
    
    for (ti in 1:length(times)){
      t <- times[ti]
      # define groups/buckets based on 60s time interval
      time_period <- t * 1000

      groups <- as.matrix(floor((a$exchange_time-start_ms)/time_period)) + 1
      quotes <- data.frame(cbind(a, groups))
      
      if (use_trades){
        t_groups <- as.matrix(floor((all_trades$exchange_time-start_ms)/time_period)) + 1
        quotes <- group_filter(t_groups, groups, quotes)
        buy_sell_net <- buy_sells * all_trades$size
        trades <- data.frame(cbind(buy_sell_net, all_trades$size, t_groups))
        trades <- group_filter(groups, t_groups, trades)
        
        total_size_grp <- aggregate( trades$V2 , by=list(trades$V3), FUN = sum)
        net_buy_sell <- aggregate(trades$buy_sell_net, by=list(trades$V3), FUN = sum)
        
        soi <- net_buy_sell / total_size_grp
        soi <- soi[-1] #delete first group
      }
      
      end_mid <- aggregate( (quotes$ask + quotes$bid)/2 , by=list(quotes$groups), FUN = last)
      
      mid_return <- diff(end_mid$x) / end_mid$x[c(1:(length(end_mid$x) - 1))]
      
      mid_return <- trim(mid_return)
      
      bid_size_grp <- aggregate(quotes$bid_size, by=list(quotes$groups), FUN = last)
      ask_size_grp <- aggregate(quotes$ask_size, by=list(quotes$groups), FUN = last)
      qoi <- (bid_size_grp$x - ask_size_grp$x) / (bid_size_grp$x + ask_size_grp$x)

      l_bid_size_grp <- aggregate(quotes$bid_size, by=list(quotes$groups), FUN = second_to_last)
      l_ask_size_grp <- aggregate(quotes$ask_size, by=list(quotes$groups), FUN = second_to_last)
      l_ask_size_grp <- as.numeric(l_ask_size_grp$x)
      l_ask_size_grp[is.na(l_ask_size_grp)] <- 1
      l_bid_size_grp <- as.numeric(l_bid_size_grp$x)
      l_bid_size_grp[is.na(l_bid_size_grp)] <- 1
      
      l_qoi <- (l_bid_size_grp - l_ask_size_grp) / (l_bid_size_grp + l_ask_size_grp)
      
      #1 is soi, 2 is return
      l <- length(qoi)
      qoi <- qoi[-l]
      l_qoi <- l_qoi[-1]
      stockvol <- stockvols$vol[stockvols$Symbol == tick]
      scaled_return <- mid_return/stockvol
      
      if (use_trades){ 
        soi <- soi$x[-l]   	    
        msg<- paste(tick,qoi,l_qoi,soi,mid_return,scaled_return ,sep=",")
      } else{
        msg<- paste(tick,qoi,l_qoi,mid_return,scaled_return ,sep=",")
      }
      # One bucket ahead prediction
      cat(paste(msg,'\n'), file=paste('ExhaustiveAnalysis',date,'time',t,'_',use_trades,exclude_d,'.csv',sep=""), append=T)
    }
  }
}

# Testing
aggregate_imbalance(F,F)
aggregate_imbalance(F,T)

aggregate_imbalance(T,T,'20130424',T)
aggregate_imbalance(T,T,'20130426')
aggregate_imbalance(T,T)
