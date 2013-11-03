
calc_SOI <- function(interval, trades, use_trades=T){
  
  m_interval <- interval *1000 #/ 60.0
  classifications <- vector()
  options(digits.secs=9)
  start_t <- 0
  
  prev_price <- 0
  prev_prev_price <- 0.0
  prev_bucket_price <- 0.0
  bucket_volume <- 0.0
  volume_count <- 0.0
  OI <- 0.0
  r_OI <- 0.0
  OI_buckets <- vector()
  r_OI_buckets <- vector()
  
  price_returns <- vector()
  price_volatilities <- vector()
  price_returns_finer_grained <- vector()
  
  r_price_volatilities <- vector()
  r_price_returns_finer_grained <- vector()
  
  prev_symbol <- 'X'
  update_first_quotes <- T
  bid <- 0.0
  ask <- 0.0
  prev_bucket_mid <-0.0
  next_bucket_mid <-0.0
  
  ema_bid <-0.0
  ema_ask <-0.0
  use_quotes <- F
  
  first_record <- T
  qc <- 0
  i <- 1
  total_volume <- 0.0
  r_total_volume <- 0.0
  
  bid_size <- 0
  ask_size <- 0
  
  num_records <- length(trades[,1]) 
  while (i+qc != num_records){
    #print(paste("I&Q: ", i, q))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      bid <- trades$bid[i+qc]
      ask <- trades$ask[i+qc]
      bid_size <- bid_size + trades$bid_size
      ask_size <- ask_size + trades$ask_size
      
      #ema_bid <- trades$ema_bid[i+qc]
      #ema_ask <- trades$ema_ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (first_record){ #first record
        price <- trades$price[i+qc]
        start_t <- trades$exchange_time[i+qc]
        prev_price <- price
        #prev_prev_price <- price
        prev_bucket_price <- price
        prev_bucket_mid <- (bid+ask)/2
        first_record <- F
      }
      
      if (prev_symbol != 'X' && !use_trades){ use_quotes <- T }
    }
    
    price_returns_finer_grained <- c(price_returns_finer_grained, log(price/prev_price))
    time <- trades$exchange_time[i+qc]
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    n <- i+qc
    
    if (use_quotes){
      #b <- assign_buy_bid_ask(ema_bid, ema_ask, price, prev_price, prev_prev_price)
      b <- classify_bid_ask(bid, ask, price, trades$price[1:n])
    }else{
      b <- classify(price, trades$price[1:n])
    }
    
    OI <- OI + b*volume
    classifications <- c(classifications, b)
    total_volume <- total_volume + volume
    
    if (((time - start_t) > m_interval/2) && ((time - start_t) < m_interval) && ((i+qc)!=length(trades[,1]))){
      r_OI <- r_OI + b*volume
      r_total_volume <- r_total_volume + volume
      r_price_returns_finer_grained <- c(r_price_returns_finer_grained, log(price/prev_price))
    }
    
    else if(((time - start_t) > m_interval)|| ((i+qc)==length(trades[,1]))){  
      #print ("Filled one Bucket")
      #price_returns <- c(price_returns, log(price/prev_bucket_price))
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      r_price_volatilities <- c(r_price_volatilities, var(r_price_returns_finer_grained))
      price_returns_finer_grained <- vector()
      r_price_returns_finer_grained <- vector()
      
      price_returns <- c(price_returns, log(((bid+ask)/2)/prev_bucket_mid))
      #price_returns <- c(price_returns, log(next_bucket_mid/prev_bucket_mid))
      
      prev_bucket_price <- price
      prev_bucket_mid <- (bid+ask)/2
      OI_buckets <- c(OI_buckets, OI/total_volume)
      if (r_OI == 0.0){
        r_OI_buckets <- c(r_OI_buckets, OI/total_volume)
      }else{
        r_OI_buckets <- c(r_OI_buckets, r_OI/r_total_volume)
      }
      r_total_volume <- 0.0
      total_volume <- 0.0
      OI <- 0.0
      r_OI <- 0.0
      start_t <- time
      #prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  price_volatilities[is.na(price_volatilities)] <-0
  r_price_volatilities[is.na(r_price_volatilities)] <-0
  print(paste("Not Classifiation Ratio: ", length(length(which(classification==0)))/length(classification)))
  return (cbind(OI_buckets, price_returns, price_volatilities, r_OI_buckets, r_price_volatilities))
}

