# This function can be used to construct SOI; it is used to construct concurrent SOI regression
# See examples below
calc_SOI <- function(interval, trades, use_trades=F){
  
  m_interval <- interval * 1000 
  classifications <- vector()
  options(digits.secs=9)
  start_t <- 0
  
  prev_price <- 0
  prev_prev_price <- 0.0
  prev_bucket_price <- 0.0
  bucket_volume <- 0.0
  volume_count <- 0.0
  OI <- 0.0
  OI_buckets <- vector()
  
  price_returns <- vector()
  
  prev_symbol <- 'X'
  update_first_quotes <- T
  bid <- 0.0
  ask <- 0.0
  prev_bucket_mid <-0.0
  next_bucket_mid <-0.0
  
  use_quotes <- F
  
  first_record <- T
  qc <- 0
  i <- 1
  total_volume <- 0.0
  
  bid_size <- 0
  ask_size <- 0
  
  num_records <- length(trades[,1]) 
  while (i+qc != num_records){
    print(paste("I&Q: ", i+qc))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      bid <- trades$bid[i+qc]
      ask <- trades$ask[i+qc]
      bid_size <- bid_size + trades$bid_size
      ask_size <- ask_size + trades$ask_size
      
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

    time <- trades$exchange_time[i+qc]
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    n <- i+qc
    
    if (use_quotes){
      b <- classify_bid_ask(bid, ask, price)
    }else{
      b <- classify(trades$price[1:n])
    }
    
    OI <- OI + b*volume
    classifications <- c(classifications, b)
    total_volume <- total_volume + volume
    
    if(((time - start_t) > m_interval)|| ((i+qc)==length(trades[,1]))){  
      
      price_returns <- c(price_returns, log(((bid+ask)/2)/prev_bucket_mid))
      
      prev_bucket_price <- price
      prev_bucket_mid <- (bid+ask)/2
      OI_buckets <- c(OI_buckets, OI/total_volume)

      total_volume <- 0.0
      OI <- 0.0
      start_t <- time
      prev_price <- price
    }
    i <- i+1
  }

  #print(paste("Not Classifiation Ratio: ", length(length(which(classification==0)))/length(classification)))
  return (cbind(OI_buckets, price_returns))
}


p <- calc_SOI(30, trades_quotes)
p[,2] <- trim(p[,2])
plot(p[,1],p[,2],xlab='Signed Order Imbalance (30 seconds interval)', ylab='Concurrent Time Bucket Return')
title('Concurrent Time Bucket Return vs. SOI (30 seconds interval) for AMZN Stock on 4/23/2013')
abline(lm(p[,2]~p[,1]))
summary(lm(p[,2]~p[,1]))