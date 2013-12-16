library(rhdf5)
library(zoo)
library(TTR) 
library(MASS) 

COMMISSION <<- 0.0005
UPPER_THRES <<-0.7
LOWER_THRES <<-0.3

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

filter_trades_quotes <- function(a){ #designed to reduce the # of trades necessary for processing data
  indd <- which(a$type == 'Q');
  indd <- indd[! (hasq(32, a$quals[indd]) | hasq(59, a$quals[indd]))];
  ind = unlist(as.list(rbind(indd - 1, indd)));
  ind = ind[ind > 0];
  trades_quotes <- a[ind,]
  return (trades_quotes)
}

filter <- function(a, sym='Q', exclude_qual=T){ #designed to reduce the # of quotes necessary for processing data
  indd <- which(a$type == sym);
  if (exclude_qual=='T'){
    indd <- indd[! (hasq(32, a$quals[indd]) | hasq(59, a$quals[indd]))];
  }
  trades_quotes <- a[indd,]
  return (trades_quotes)
}

take_mid <- function(quotes){
  mid <- floor((length(quotes) - 1)/2)
  return (quotes[mid])
}

take_last <- function(quotes){
  return (quotes[length(quotes)])
}

calc_TWAP <- function(quotes, time_period, sell=T, commission=COMMISSION){
  start_ms <- quotes$exchange_time[1]
  groups <- as.matrix(floor((quotes$exchange_time-start_ms)/time_period)) + 1  
  quotes <- data.frame(cbind(quotes, groups))
  prices <- vector()
  times <- vector()
  
  if (sell == T){
    prices <- aggregate( quotes$bid , by=list(quotes$groups), FUN = take_last)
    # Section 31 Fee
    prices <- prices - prices/1000000 * 17.40 
  }else{
    prices <- aggregate( quotes$ask , by=list(quotes$groups), FUN = take_last)
  }
  prices <- prices - 0.000119
  prices <- prices - 0.003
  prices <- prices - commission
  #   times <- aggregate( quotes$time , by=list(quotes$groups), FUN = take_last) 
  return (prices$x)
}

helper <- function(bid, q){
  if(bid==T){
    return (q$bid)
  }else{
    return (q$ask)
  }
}

liquidating_schedule <- function(quotes, time_period, num_shares, sell=T){
  # return values
  exec_shares <- vector()
  exec_prices <- vector()
  ti <- 0
  initial_shares <- num_shares
  market_order <- vector()
  
  prices <- helper(sell, quotes)
  prev_grp <- 0
  grp <- 0
  liquidated <- F
  min_grp <- min(quotes$groups)
  # average # of shares to be liquidated according to TWAP schedule
  avg_shares <- ceiling(num_shares/time_period)
  behind_schedule_vol <- 0
  ahead_schedule_vol <- 0
  
  next_price <- prices[1]
  inds <- which(quotes$type=='T')
  placed_limit_order <- F
  limit_price <- 0
  
  for (i in 1:length(quotes[,1])){
    if (num_shares <= 0){ #no more shares
      break;
    }
    if (quotes$type[i] == 'T'){ #update using the latest trade price
      n_price_ind <- min((which(inds==i)+1),length(inds))
      next_price <- quotes$price[inds[n_price_ind]]
      next
    }
    
    price <- prices[i]
    ti <- quotes$time[i]
    
    if (i == length(quotes[,1]) && num_shares > 0){#last quote, must liquidate or buy everything remaining
      exec_shares <- c(exec_shares, num_shares)
      exec_prices <- c(exec_prices, price) #consider use previous price
      market_order <- c(market_order, T)
    }
    grp <- quotes$groups[i]
    imbl <- quotes$imbalance[i]
    
    if (placed_limit_order && ((sell && limit_price <= quotes$bid[i]) || (!sell && limit_price >= quotes$ask[i]))){
      exec_prices <- c(exec_prices, limit_price) #consider use previous price
      num_shares <- num_shares - avg_shares
      exec_shares <- c(exec_shares, avg_shares)
      market_order <- c(market_order, F)
      placed_limit_order <- F
    }
    
    # forced liquidation, at the end of each TWAP interval
    if ((grp != prev_grp) && (grp != min_grp)){
      prev_grp <- grp
      if(liquidated == F){
        #Bad time to liquidate, put shares in behind_schedule_vol
        if ((sell==T && imbl > UPPER_THRES) || (sell==F && imbl < LOWER_THRES)){
          behind_schedule_vol <- behind_schedule_vol + avg_shares
          if (!placed_limit_order){ 
            placed_limit_order <- T
            ahead_schedule_vol <- ahead_schedule_vol + avg_shares
            if (sell){ limit_price <- quotes$bid[i] + 0.05 }
            else if (!sell){ limit_price <- quotes$ask[i] - 0.05 }
          }
        }
        else{
          shares <- min(num_shares,avg_shares)
          exec_shares <- c(exec_shares, shares)
          exec_prices <- c(exec_prices, price) #consider use previous price
          market_order <- c(market_order, T)
          num_shares <- num_shares - shares
        }
      }
      else{
        liquidated <- F
      }
    }
    else if (liquidated == F){ 
      if ((sell==T && imbl < LOWER_THRES) || (sell==F && imbl > UPPER_THRES)){ #time to liquidate or time to acquire
        if (behind_schedule_vol > 0){
          bh_liquid_vol <- min(avg_shares, behind_schedule_vol)
          behind_schedule_vol <- behind_schedule_vol - bh_liquid_vol
          num_shares <- num_shares - bh_liquid_vol
          exec_shares <- c(exec_shares, bh_liquid_vol)
        }
        else{
          liquidated = T
          shares <- min(num_shares,avg_shares)
          num_shares <- num_shares - shares
          exec_shares <- c(exec_shares, shares)
        }
        
        exec_prices <- c(exec_prices, price) #consider use previous price
        market_order <- c(market_order, T)
      }
    }
    else if (ahead_schedule_vol <= 0.5*initial_shares){
      if (!placed_limit_order && ((sell==T && imbl > UPPER_THRES) || (sell==F && imbl < LOWER_THRES))){ 
        placed_limit_order <- T
        ahead_schedule_vol <- ahead_schedule_vol + avg_shares
        if (sell){ limit_price <- quotes$bid[i] + 0.25 }
        else if (!sell){ limit_price <- quotes$ask[i] - 0.25 }
      }
    }      
  }
  print(ti)
  #   return (exec_prices)
  return (cbind(exec_shares, exec_prices, market_order))
}

run_hft_strategy <- function(ticker, sell=T, freq = 600, commission=COMMISSION, shares=100000, dt="20130424"){
  print(paste("Parsing Ticker: ",ticker))
  trades_quotes <- h5read(paste("ticks.",dt,".h5",sep=""), paste("/ticks/",ticker, sep=""), bit64conversion='double')
  quotes <- filter_trades_quotes(trades_quotes)
  initial_price <- quotes$price[which(quotes$type=='T')[1]]
  
  quotes_only <- filter(trades_quotes)
  
  # total quotes volume: 13783600, total traded volume:1829051 => assumption: we can always liquidate trades at best NBBO 
  # quotes_total_size <- sum(quotes$bid_size)
  # trades <- filter(trades_quotes, 'T')
  # last_trade <- length(trades[,1])
  # total_volume <- trades$volume[last_trade]
  
  start_ms <- quotes$exchange_time[1]
  time_period <- freq* 1000 
  groups <- as.matrix(floor((quotes$exchange_time-start_ms)/time_period)) + 1
  
  imbalance <- quotes$bid_size / (quotes$bid_size + quotes$ask_size)
  quotes <- data.frame(cbind(quotes, groups, imbalance))
  
  twap_vec <- calc_TWAP(quotes_only, time_period, sell)
  bucket_num <- length(unique(groups))
  twap_price <- sum(twap_vec)/length(twap_vec)
  
  qi_vec <- liquidating_schedule(quotes, bucket_num, shares, sell)
  qi_prices <- qi_vec[,2]
  qi_prices <- qi_prices - COMMISSION
  limit_index <- which(qi_vec[,3]==0) #limit orders
  
  # TAF Fee, per share basis
  qi_prices <- qi_prices - 0.000119
  if (sell){ #Sec 31 Fee applies on sale
    qi_prices <- qi_prices - qi_prices/1000000*17.40
    qi_prices <- qi_prices - ifelse(index(qi_prices) %in% limit_index, 0.002, -0.003)
  }else{
    qi_prices <- qi_prices + ifelse(index(qi_prices) %in% limit_index, 0.002, -0.003)
  }
  
  qi_shares <- qi_vec[,1]
  
  qi_price <- sum(qi_prices)/length(qi_prices)
  
  print("TWAP Prices")
  print(twap_vec)
  print("QI Prices")
  print(qi_prices)
  
  plot(twap_vec, type="b", col="blue", x=c(1:length(twap_vec)), xlab="Nth Trade", ylab="Trade Price")
  
  lines(qi_prices, type="b", col="red", pch=ifelse(index(qi_prices) %in% limit_index, 19, 1))
  
  if (sell){
    title("Liquidation Trade Price (Net Maker-Taker & Transaction Costs) vs. Nth Trade")
  }
  else{
    title("Purchasing Trade Price (Net Maker-Taker & Transaction Costs) vs. Nth Trade")
  }
  
  legend("bottomright", col=c("red","red", "blue"), legend=c("QI_Market","QI_Limit", "TWAP"), pch=c(1,19,1),lty=c(7,7,7))
  print(summary(twap_vec))
  print(summary(qi_prices))
  return (calc_pnl(shares, initial_price, sell, twap_vec, qi_vec))
}


calc_pnl <-function(shares, price, sell, twap_vec, qi_vec){
  options(digits=2)
  buy_sell <- ifelse(sell==T,1,-1)
  avg_shares <- shares/length(qi_vec[,1])
  print(paste("Initial Shares: ", shares, "; Initial Trade Price: ", round(price,2)))
  print(paste("Ending Cash: $", round(sum(qi_vec[,2]*avg_shares,2))))
  print(paste("Net Saving Over TWAP: $", buy_sell*round(sum(qi_vec[,2]*avg_shares) - sum(twap_vec*avg_shares),2)))
  # Market Order and Limit Order net difference is 0.005
  percent_limit_order <- length(which(qi_vec[,3]==0))/length(qi_vec[,3])
  print(paste("% limit order: ",percent_limit_order))
  print(paste("Saving due to Rebate: $", round(percent_limit_order*(0.003+0.002)*shares,2)))
  print(paste("SEC 31 Transaction Cost: $", round(ifelse(sell, 17.4/1000000*sum(qi_vec[,2])*avg_shares, 0),2)))
  print(paste("TAF Fee: $", 0.000119*shares))
  print(paste("Commission Rate: ", COMMISSION, "Commision Cost: ", COMMISSION*shares))
  avgTwap <- round(sum(twap_vec)/length(twap_vec),2)
  avgQi <- round(sum(qi_vec[,2])/length(qi_vec[,2]),2)
  print(paste("Avg TWAP price: $",avgTwap, "; Avg QI price: $", avgQi))
  print(paste("Avg Shares per market action: ", avg_shares))
  print(paste("TWAP price standard deviation: $", round(sqrt(var(twap_vec)),2), "; QI price standard deviation: $", round(sqrt(var(qi_vec[,2])),2)))
  if (avgQi > avgTwap){
    if (sell){return (1)}
    else {return (-1)}
  }else if (avgQi < avgTwap){
    if (sell){return (-1)}
    else { return (1)}
  }else{ return (0) }
}


cal_outperformance <- function(sell, freq = 600, commission=COMMISSION, shares=100000, dt="20131016"){
  out_perform <- 0
  neutral <- 0
  under_perform <- 0
  tickers <- c('ABT','AAPL','ACN','AEP','AIG','ALL','AMGN','AMZN','APA','APC','AXP','BA','BAC','BAX','BK','BMY','C','CAT','CL','CMCSA','COF','COP','COST','CSCO','CVS','CVX','DD','DELL','DIS','DOW','DVN','EBAY','EMC','EMR','EXC','F','FCX','FDX','GD','GE','GILD','GM','GOOG','GS','HAL','HD','HON','HPQ','IBM','INTC','JNJ','JPM','KO','LLY','LMT','LOW','MA','MCD','MDLZ','MDT','MET','MMM','MO','MON','MRK','MS','MSFT','NKE','NOV','NSC','NWSA','ORCL','OXY','PEP','PFE','PG','PM','QCOM','RTN','SBUX','SLB','SO','SPG','T','TGT','TWX','TXN','UNH','UNP','UPS','USB','UTX','V','VZ','WAG','WFC','WMB','WMT','XOM')
  for (i in 1:length(tickers)){
    tick <- tickers[i]
    res <- run_hft_strategy(tick,sell, freq, commission, shares, dt)
    if (res==1){out_perform <- out_perform + 1}
    else if (res==-1){under_perform <- under_perform + 1}
    else{neutral <- neutral + 1}
  }
  print(paste("Out Perform: ",out_perform, "; Under Perform: ", under_perform, "; Tie: ", neutral))
  return (c(out_perform, under_perform, neutral))
}

windows()

# sell_vec_tf <-cal_outperformance(T, 600, COMMISSION, 100000, "20130424")
# buy_vec_tf <-cal_outperformance(F, 600, COMMISSION, 100000, "20130424")

sell_vec <- cal_outperformance(T)
buy_vec <- cal_outperformance(F)

# cal_outperformance(T)
# cal_outperformance(F)
# par(mfrow=c(2,1))
run_hft_strategy('FFIV',T)
# run_hft_strategy('AIG',T)
