<<<<<<< HEAD
library(rhdf5)
library(zoo)
library(TTR) 

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

delay_quotes_xms <- function(data_a, delay_time){
  options(digits.secs=6)
  data_a$time <- strptime(data_a$time,"%H:%M:%OS")
  data_a[data_a$type == 'Q',]$time <- data_a[data_a$type == 'Q',]$time + delay_time
  return(data_a[with(data_a, order(time)), ])
}

assign_buy_bid_ask <- function(bid, ask, price){
  bid_diff <- price - bid
  ask_diff <- price - ask
  if (abs(bid_diff) < abs(ask_diff)){ # trade price closer to bid => taking the bid => assign sell
    return (-1)
  }
  else if (abs(bid_diff) > abs(ask_diff)){ # trade price closer to ask => assign buy
    return (1)
  }
  else{
    return (0)
  }
}

assign_buy <- function(prev_prev_price, prev_price, price, 
                       use_sub_penny_rule, use_momentum_rule, 
                       use_quote, bid, ask){
  if (use_quote){
    return (assign_buy_bid_ask(bid, ask, price))
  }
  else{
    p <- 0.0
    prev_p <- 0.0
    if (use_sub_penny_rule){
      p <- price
      prev_p <- prev_price
    }else{
      p<-round(price,2)
      prev_p <- round(prev_price,2)
    }
    if (use_momentum_rule && p==prev_p){
      if (prev_prev_price < p){
        return (-1)
      }else if (prev_prev_price > p){
        return (1)
      }else{
        return (0)
      }
    }else{
      if (p > prev_p){
        return (-1)
      }else if (p < prev_p){
        return (1)
      }else{
        return (0)
      }
    }
  }
}

modify_series <- function(series, burn_size){
  cut <- 0.0
  for (i in 1:length(series)){
    cut <- cut + series[i]
    if (cut >= burn_size){
      s <- vector()
      if (cut == burn_size){
        s <- series[i+1:length(series)]
      }else{
        s <- c(cut-burn_size, series[i+1:length(series)])
      }
      s <- s[!is.na(s)]
      return (s)
    }
  }
}

calc_SOI <- function(interval, trades){
  
  m_interval <- interval / 60.0
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
  first_record <- T
  use_quotes <- F
  q <- 0
  i <- 1
  total_volume <- 0.0
  
  while (i+q != length(trades[,1])){
    #print(paste("I&Q: ", i, q))
    type <- trades[i+q,"type"]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades[i+q,"bid"]
      prev_ask <- trades[i+q,"ask"]
      q <- q+1
      next
    }
    else{ # This is a trade
      if (first_record){ #first record
        price <- as.numeric(trades[i+q,"price"])
        start_t <- strptime(trades[i+q,"time"],"%H:%M:%OS")
        prev_price <- price
        prev_prev_price <- price
        prev_bucket_price <- price
        first_record <- F
      }
      if (prev_symbol != 'X'){ use_quotes <- T }
    }
    
    time <- strptime(trades[i+q,"time"],"%H:%M:%OS")
    volume <- as.numeric(trades[i+q,"size"])
    price <- as.numeric(trades[i+q,"price"])
    
    b <- assign_buy(prev_prev_price, prev_price, price, T, T, use_quotes, prev_bid, prev_ask)
    
    OI <- OI + b*volume
    total_volume <- total_volume + volume
    
    if(((time - start_t) > m_interval)|| ((i+q)==length(trades[,1]))){  
      print ("Filled one Bucket")
      price_returns <- c(price_returns, log(price/prev_bucket_price))
      prev_bucket_price <- price
      OI_buckets <- c(OI_buckets, OI/total_volume)
      total_volume <- 0.0
      OI <- 0.0
      start_t <- time
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  return (cbind(OI_buckets, price_returns))
}


calc_volat_by_volumes <- function(trades, bucket_volume_size, realized_vol_period
                                  , compute_pure_volume=F) {
  volume_volatilities <- vector()
  vol_vector <- vector()
  cum_volume <- 0.0
  i <- 1
  burn <- 0
  first_burn <- T
  while (i != length(trades[,1])){
    volume <- as.numeric(trades[i,"size"])
    #print(paste("volume: ",volume))
    cum_volume <- cum_volume + volume
    #print(paste("cum volume: ",cum_volume))
    vol_vector <- c(vol_vector, volume)
    if (((cum_volume >= bucket_volume_size * realized_vol_period) && first_burn)
        || ((cum_volume >= bucket_volume_size) && !first_burn)){
      if (!first_burn){
        burn <- burn + 1
      }
      if (((cum_volume == bucket_volume_size * realized_vol_period) && first_burn)
          || ((cum_volume == bucket_volume_size) && !first_burn)){
        i <- i + 1
      }else{
        if (first_burn){
          #print(paste("Set trade size to : ",cum_volume - bucket_volume_size*realized_vol_period))
          trades[i,"size"] <- cum_volume - bucket_volume_size*realized_vol_period
        }else{
          #print(paste("Set trade size to : ",cum_volume - bucket_volume_size))
          trades[i,"size"] <- cum_volume - bucket_volume_size
        }

      }
      first_burn <- F
      #print(paste("VEC: ",modify_series(vol_vector, burn * bucket_volume_size)))
      #print(paste("VOL: ", sd(modify_series(vol_vector, burn * bucket_volume_size))))
      if (!compute_pure_volume){
        volume_volatilities <- c(volume_volatilities, var(modify_series(vol_vector, burn * bucket_volume_size)))
      }else{
        volume_volatilities <- c(volume_volatilities, sum(modify_series(vol_vector, burn * bucket_volume_size)))
      }
      cum_volume <- 0.0
      
    }
    else{
      i <- i + 1
    } 
  }
  return (volume_volatilities)
}

#interval in seconds
calc_OI_by_time_buckets <- function(interval
                                    , trades
                                    , bucket_volume_size
                                    , use_gaussian
                                    , L=50
                                    , signed=F
                                    , use_momentum_rule = T
                                    , use_sub_penny_rule = T
) {
  m_interval <- interval / 60.0
  options(digits.secs=9)
  start_t <- 0
  prev_price <- 0
  prev_bucket_price <- 0.0
  bucket_volume <- 0.0
  volume_count <- 0.0
  OI <- 0.0
  OI_buckets <- vector()
  price_returns <- vector()
  price_volatilities <- vector()
  price_returns_finer_grained <- vector()
  gaussian_sigma_vector <- vector()
  first_record <- T
  i <- 1
  j <- 0
  sigma <-0.0
  prev_symbol <- 'X'
  prev_bid <- 0.0
  prev_ask <- 0.0
  use_quotes <- F
  qc <- 0; trlen <- length(trades[,1])
  while (i+qc <= trlen){
    #print(paste("I&Q: ", i, qc))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades$bid[i+qc]
      prev_ask <- trades$ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (prev_symbol != 'X'){ use_quotes <- T }
      #print("Parsing Trades")
    }
    j <- j+1
    
    tm <- trades$time[i+qc]
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    
    #print(paste("SIZE: ", volume))
    #print(paste("Cum Vol: ", volume_count))
    if (first_record){ #first record
      start_t <- tm
      prev_price <- price
      prev_prev_price <- price
      prev_bucket_price <- price
      first_record <- F
    }
    
    price_returns_finer_grained <- c(price_returns_finer_grained, log(price/prev_price))
    gaussian_sigma_vector <- c(gaussian_sigma_vector, price-prev_price)
    entries <- length(gaussian_sigma_vector)
    if (entries > L){
      gaussian_sigma_vector <- gaussian_sigma_vector[2:entries]
    }
    
    if (volume_count + volume >= bucket_volume_size){ #filled one bucket
      residual_volume <- bucket_volume_size - volume_count#bucket_volume
      #print("filled one bucket")
      b <- 1.0
      if (use_gaussian){ sigma <- var(gaussian_sigma_vector) } 
      if(!is.na(sigma) && !(sigma==0)){
        #print(gaussian_sigma_vector)
        #print("gaussian b")
        b <- 2*pnorm((price-prev_price)/sigma)  - 1 
      }
      else{
        #print(paste("DEBUG: ", prev_bid, prev_ask, price))
        
        b <- assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                        use_quotes, prev_bid, prev_ask)
      }
      OI <- OI + b*residual_volume
      #print(paste("Plus total: ", OI, ", residual: ", residual_volume))
      #print(paste("Updating OI: ", OI, b, residual_volume))
      if (volume_count + volume > bucket_volume_size){ #split order
        trades$size[i+qc] <- volume_count + volume - bucket_volume_size
        #print(paste("Split volume to ", trades[i,"size"], "I: ", i))
      }else{
        i <- i+1
      }
      
      price_returns <- c(price_returns, (price-prev_bucket_price)/prev_bucket_price)
      
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      #print(paste("Vector Size: ", length(price_returns_finer_grained)))
      #print(paste("VAR: ", var(price_returns_finer_grained)))
      if ((j %% L) == 0){ # using L number of buckets
        price_returns_finer_grained <- vector()
      }
      #if (signed){
        OI_buckets <- c(OI_buckets, OI / bucket_volume_size) #update OI_bucket  
        #print (paste("SOI is: ",OI))
      #}else{
      # OI_buckets <- c(OI_buckets, abs(OI / bucket_volume_size)) #update OI_bucket 
        #print (paste("OI is: ",OI))
      #}
      
      OI <- 0.0
      #print("Reset OI.")
      bucket_volume <-0.0
      volume_count <-0.0
      start_t <-tm
      prev_prev_price <- prev_price
      prev_price <- price
      prev_bucket_price <- price
      next
    }
    else{
      bucket_volume <- bucket_volume + volume
      volume_count <- volume_count + volume
    }
    
    #TR-VPIN time interval is reached, update OI vector
    if((tm - start_t) > m_interval){ 
      #print("filled one bin--->")
      b <- assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                      use_quotes, prev_bid, prev_ask)
      OI <- OI + b*bucket_volume
      
      #print(paste("Plus total-bin: ", OI, ", vol: ", bucket_volume))
      bucket_volume <- 0.0 #reset
      #volume_count <- volume_count + volume
      start_t <- tm
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  
  if (signed){
	OI_buckets = abs(OI_buckets);
  }
  OI_vs_delta_prices <- cbind(OI_buckets, price_returns, price_volatilities)
  return (OI_vs_delta_prices)
}


function(xs, ys,xlabb="x",ylabb="y",mmain =""){
  one_factor_model <- lm(ys~xs)
  print(summary(one_factor_model))
  plot(xs, ys,xlab=xlabb,ylab=ylabb,main=mmain)
  abline(lm(ys~xs))
  return(one_factor_model)
=======
library(rhdf5)
library(zoo)
library(TTR) 

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

assign_buy_bid_ask <- function(bid, ask, price){
  bid_diff <- price - bid
  ask_diff <- price - ask
  if (abs(bid_diff) < abs(ask_diff)){ # trade price closer to bid => taking the bid => assign sell
    return (-1)
  }
  else if (abs(bid_diff) > abs(ask_diff)){ # trade price closer to ask => assign buy
    return (1)
  }
  else{
    return (0)
  }
}

assign_buy <- function(prev_prev_price, prev_price, price, 
                       use_sub_penny_rule, use_momentum_rule, 
                       use_quote, bid, ask){
  if (use_quote){
    return (assign_buy_bid_ask(bid, ask, price))
  }
  else{
    p <- 0.0
    prev_p <- 0.0
    if (use_sub_penny_rule){
      p <- price
      prev_p <- prev_price
    }else{
      p<-round(price,2)
      prev_p <- round(prev_price,2)
    }
    if (use_momentum_rule && p==prev_p){
      if (prev_prev_price < p){
        return (-1)
      }else if (prev_prev_price > p){
        return (1)
      }else{
        return (0)
      }
    }else{
      if (p > prev_p){
        return (-1)
      }else if (p < prev_p){
        return (1)
      }else{
        return (0)
      }
    }
  }
}

modify_series <- function(series, burn_size){
  cut <- 0.0
  for (i in 1:length(series)){
    cut <- cut + series[i]
    if (cut >= burn_size){
      s <- vector()
      if (cut == burn_size){
        s <- series[i+1:length(series)]
      }else{
        s <- c(cut-burn_size, series[i+1:length(series)])
      }
      s <- s[!is.na(s)]
      return (s)
    }
  }
}

calc_SOI <- function(interval, trades){
  
  m_interval <- interval / 60.0
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
  first_record <- T
  use_quotes <- F
  qc <- 0
  i <- 1
  total_volume <- 0.0
  
  while (i+qc != length(trades[,1])){
    #print(paste("I&Q: ", i, q))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades$bid[i+qc]
      prev_ask <- trades$ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (first_record){ #first record
        price <- trades$price[i+qc]
        start_t <- trades$time[i+qc]
        prev_price <- price
        prev_prev_price <- price
        prev_bucket_price <- price
        first_record <- F
      }
      if (prev_symbol != 'X'){ use_quotes <- T }
    }
    
    time <- trades$time[i+qc]
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    
    b <- assign_buy(prev_prev_price, prev_price, price, T, T, use_quotes, prev_bid, prev_ask)
    
    OI <- OI + b*volume
    total_volume <- total_volume + volume
    
    if(((time - start_t) > m_interval)|| ((i+q)==length(trades[,1]))){  
      print ("Filled one Bucket")
      price_returns <- c(price_returns, log(price/prev_bucket_price))
      prev_bucket_price <- price
      OI_buckets <- c(OI_buckets, OI/total_volume)
      total_volume <- 0.0
      OI <- 0.0
      start_t <- time
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  return (cbind(OI_buckets, price_returns))
}


calc_volat_by_volumes <- function(trades, bucket_volume_size, realized_vol_period
                                  , compute_pure_volume=F) {
  volume_volatilities <- vector()
  vol_vector <- vector()
  cum_volume <- 0.0
  i <- 1
  burn <- 0
  first_burn <- T
  while (i != length(trades[,1])){
    volume <- as.numeric(trades[i,"size"])
    #print(paste("volume: ",volume))
    cum_volume <- cum_volume + volume
    #print(paste("cum volume: ",cum_volume))
    vol_vector <- c(vol_vector, volume)
    if (((cum_volume >= bucket_volume_size * realized_vol_period) && first_burn)
        || ((cum_volume >= bucket_volume_size) && !first_burn)){
      if (!first_burn){
        burn <- burn + 1
      }
      if (((cum_volume == bucket_volume_size * realized_vol_period) && first_burn)
          || ((cum_volume == bucket_volume_size) && !first_burn)){
        i <- i + 1
      }else{
        if (first_burn){
          #print(paste("Set trade size to : ",cum_volume - bucket_volume_size*realized_vol_period))
          trades[i,"size"] <- cum_volume - bucket_volume_size*realized_vol_period
        }else{
          #print(paste("Set trade size to : ",cum_volume - bucket_volume_size))
          trades[i,"size"] <- cum_volume - bucket_volume_size
        }

      }
      first_burn <- F
      #print(paste("VEC: ",modify_series(vol_vector, burn * bucket_volume_size)))
      #print(paste("VOL: ", sd(modify_series(vol_vector, burn * bucket_volume_size))))
      if (!compute_pure_volume){
        volume_volatilities <- c(volume_volatilities, var(modify_series(vol_vector, burn * bucket_volume_size)))
      }else{
        volume_volatilities <- c(volume_volatilities, sum(modify_series(vol_vector, burn * bucket_volume_size)))
      }
      cum_volume <- 0.0
      
    }
    else{
      i <- i + 1
    } 
  }
  return (volume_volatilities)
}

#interval in seconds
calc_OI_by_time_buckets <- function(interval
                                    , trades
                                    , bucket_volume_size
                                    , use_gaussian
                                    , L=50
                                    , signed=F
                                    , use_momentum_rule = T
                                    , use_sub_penny_rule = T
) {
  m_interval <- interval / 60.0
  options(digits.secs=9)
  start_t <- 0
  prev_price <- 0
  prev_bucket_price <- 0.0
  bucket_volume <- 0.0
  volume_count <- 0.0
  OI <- 0.0
  OI_buckets <- vector()
  price_returns <- vector()
  price_volatilities <- vector()
  price_returns_finer_grained <- vector()
  gaussian_sigma_vector <- vector()
  first_record <- T
  i <- 1
  j <- 0
  sigma <-0.0
  prev_symbol <- 'X'
  prev_bid <- 0.0
  prev_ask <- 0.0
  use_quotes <- F
  qc <- 0; trlen <- length(trades[,1])
  while (i+qc <= trlen){
    #print(paste("I&Q: ", i, qc))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades$bid[i+qc]
      prev_ask <- trades$ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (prev_symbol != 'X'){ use_quotes <- T }
      #print("Parsing Trades")
    }
    j <- j+1
    
    tm <- trades$time[i+qc]
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    
    #print(paste("SIZE: ", volume))
    #print(paste("Cum Vol: ", volume_count))
    if (first_record){ #first record
      start_t <- tm
      prev_price <- price
      prev_prev_price <- price
      prev_bucket_price <- price
      first_record <- F
    }
    
    price_returns_finer_grained <- c(price_returns_finer_grained, log(price/prev_price))
    gaussian_sigma_vector <- c(gaussian_sigma_vector, price-prev_price)
    entries <- length(gaussian_sigma_vector)
    if (entries > L){
      gaussian_sigma_vector <- gaussian_sigma_vector[2:entries]
    }
    
    if (volume_count + volume >= bucket_volume_size){ #filled one bucket
      residual_volume <- bucket_volume_size - volume_count#bucket_volume
      #print("filled one bucket")
      b <- 1.0
      if (use_gaussian){ sigma <- var(gaussian_sigma_vector) } 
      if(!is.na(sigma) && !(sigma==0)){
        #print(gaussian_sigma_vector)
        #print("gaussian b")
        b <- 2*pnorm((price-prev_price)/sigma)  - 1 
      }
      else{
        #print(paste("DEBUG: ", prev_bid, prev_ask, price))
        
        b <- assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                        use_quotes, prev_bid, prev_ask)
      }
      OI <- OI + b*residual_volume
      #print(paste("Plus total: ", OI, ", residual: ", residual_volume))
      #print(paste("Updating OI: ", OI, b, residual_volume))
      if (volume_count + volume > bucket_volume_size){ #split order
        trades$size[i+qc] <- volume_count + volume - bucket_volume_size
        #print(paste("Split volume to ", trades[i,"size"], "I: ", i))
      }else{
        i <- i+1
      }
      
      price_returns <- c(price_returns, (price-prev_bucket_price)/prev_bucket_price)
      
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      #print(paste("Vector Size: ", length(price_returns_finer_grained)))
      #print(paste("VAR: ", var(price_returns_finer_grained)))
      if ((j %% L) == 0){ # using L number of buckets
        price_returns_finer_grained <- vector()
      }
      #if (signed){
        OI_buckets <- c(OI_buckets, OI / bucket_volume_size) #update OI_bucket  
        #print (paste("SOI is: ",OI))
      #}else{
      # OI_buckets <- c(OI_buckets, abs(OI / bucket_volume_size)) #update OI_bucket 
        #print (paste("OI is: ",OI))
      #}
      
      OI <- 0.0
      #print("Reset OI.")
      bucket_volume <-0.0
      volume_count <-0.0
      start_t <-tm
      prev_prev_price <- prev_price
      prev_price <- price
      prev_bucket_price <- price
      next
    }
    else{
      bucket_volume <- bucket_volume + volume
      volume_count <- volume_count + volume
    }
    
    #TR-VPIN time interval is reached, update OI vector
    if((tm - start_t) > m_interval){ 
      #print("filled one bin--->")
      b <- assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                      use_quotes, prev_bid, prev_ask)
      OI <- OI + b*bucket_volume
      
      #print(paste("Plus total-bin: ", OI, ", vol: ", bucket_volume))
      bucket_volume <- 0.0 #reset
      #volume_count <- volume_count + volume
      start_t <- tm
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  
  if (signed){
	OI_buckets = abs(OI_buckets);
  }
  OI_vs_delta_prices <- cbind(OI_buckets, price_returns, price_volatilities)
  return (OI_vs_delta_prices)
}


function(xs, ys,xlabb="x",ylabb="y",mmain =""){
  one_factor_model <- lm(ys~xs)
  print(summary(one_factor_model))
  plot(xs, ys,xlab=xlabb,ylab=ylabb,main=mmain)
  abline(lm(ys~xs))
  return(one_factor_model)
>>>>>>> 1806095b32b92f5ea8bac3b8197a5e24574cd83a
}