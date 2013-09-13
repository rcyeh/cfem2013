library(rhdf5)
library(zoo)
library(TTR) 

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

assign_buy_bid_ask <- function(bid, ask, price){
  bid_diff <- price - bid
  ask_diff <- price - ask
  if (abs(bid_diff) < abs(ask_diff)){ # trade price closer to bid => taking the bid => assign sell
    return (F)
  }
  else{ # trade price closer to ask => assign buy
    return (T)
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
        return (F)
      }else{
        return (T)
      }
    }else{
      return (p>=prev_p)
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

calc_OI_by_volume_buckets <- function(fixed_volume, trades
                                      , bucket_volume_size
                                      , use_gaussian
                                      , L=50
                                      , signed=F
                                      , use_momentum_rule = T
                                      , use_sub_penny_rule = T
                                      , use_quotes_for_price_change = F){
  Q <- bucket_volume_size / fixed_volume
  print(Q)
  prev_price <- 0
  prev_bucket_price <- 0.0

  residual_volume <- 0.0
  OI <- 0.0
  OI_buckets <- vector()
  price_returns <- vector()
  price_volatilities <- vector()
  price_returns_finer_grained <- vector()
  gaussian_sigma_vector <- vector() # used to compute sigma for Bulk-Volume VPIN
  quotes_vector <- vector()
  first_record <- T
  i <- 1
  j <- 0
  k <- 0
  q <- 0
  prev_symbol <- 'X'
  prev_bid <- 0.0
  prev_ask <- 0.0
  use_quotes <- F
  
  while (i+q != length(trades[,1])){ 
    #print(paste("I&Q: ", i, q))
    type <- trades[i+q,"type"]
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades[i+q,"bid"]
      prev_ask <- trades[i+q,"ask"]
      q <- q+1
      quotes_vector <- c(quotes_vector, (prev_bid + prev_ask)/2)
      if (use_quotes_for_price_change){
        entries <- length(quotes_vector)
        if (entries > L){
          quotes_vector <- quotes_vector[2:entries]
        }
      }
      next
    }
    else{ # This is a trade
      if (prev_symbol != 'X'){ use_quotes <- T }
      print("Parsing Trades")
    }
    
    k <- k+1
    volume <- as.numeric(trades[i+q,"size"])
    price <- as.numeric(trades[i+q,"price"])
    
    #print(paste("SIZE: ", volume))
    #print(paste("Cum Vol: ", residual_volume))
    if (first_record){ #first record
      prev_price <- price
      prev_prev_price <- price
      prev_bucket_price <- price
      first_record <- F
    }
    
    price_returns_finer_grained <- c(price_returns_finer_grained, log(price/prev_price))
    
    # Please see Anderson's paper pg 22, here I use the most L number of transactions as a sample
    # The paper does not specify on how to acquire the sample of price changes across bins, in this case. The sample is dynamic
    gaussian_sigma_vector <- c(gaussian_sigma_vector, price-prev_price)
    entries <- length(gaussian_sigma_vector)
    if (entries > L){
      gaussian_sigma_vector <- gaussian_sigma_vector[2:entries]
    }
    residual_volume <- residual_volume + volume
    
    if (residual_volume >= fixed_volume){ #filled one bin
      print("filled one bin")
      j <- j+1
      b <- 1.0
      sigma <- 0.0
      
      if (use_gaussian){
        sigma <- var(gaussian_sigma_vector)
      }
      
      if (!is.na(sigma) && !(sigma==0)){ 
        b <- 2*pnorm((price-prev_price)/sigma) - 1 
        #print(paste("Gaussian: ", price, prev_price, sigma, b))
      }
      else{
        if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule
                       ,use_quotes, prev_bid, prev_ask)){
          print(paste("DEBUG: BUY"))
          b <- 1.0
        }else{
          print(paste("DEBUG: SELL"))
          b <- -1.0
        }
      }
      #print(paste("Set b to: ", b))
      OI <- OI + b*fixed_volume 
      residual_volume <- residual_volume - fixed_volume
      if ( residual_volume > fixed_volume){ 
        trades[i+q,"size"] <- 0.0
        #print(paste("Split volume to ", trades[i,"size"], "I: ", i))
      }
      else{
        i <- i+1
      }
      
      prev_prev_price <- prev_price
      prev_price <- price      
    }else{
      i <- i+1
    }
    
    if ((k %% L)==0){ # only use L vectors to calculate volatility
      price_returns_finer_grained <- vector()
    }
    
    #FB-VPIN bucket is filled, update OI vector
    if (j == 5){ 
      j <-0
      #print("FILLED ONE BUCKET")
      price_returns <- c(price_returns, log(price/prev_bucket_price))
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      
      if (signed){
        OI_buckets <- c(OI_buckets, OI / bucket_volume_size) #update OI_bucket  
      }else{
        OI_buckets <- c(OI_buckets, abs(OI / bucket_volume_size)) #update OI_bucket 
      }
      prev_bucket_price <- price
      OI <- 0.0
    }
  }
  
  OI_vs_delta_prices <- cbind(OI_buckets, price_returns, price_volatilities)
  return (OI_vs_delta_prices)
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
                                    , use_quotes_for_price_change = F
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
  quotes_vector <- vector()
  first_record <- T
  i <- 1
  j <- 0
  sigma <-0.0
  prev_symbol <- 'X'
  prev_bid <- 0.0
  prev_ask <- 0.0
  use_quotes <- F
  q <- 0
  while (i+q != length(trades[,1])){
    #print(paste("I&Q: ", i, q))
    type <- trades[i+q,"type"]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_bid <- trades[i+q,"bid"]
      prev_ask <- trades[i+q,"ask"]
      q <- q+1
      quotes_vector <- c(quotes_vector, (prev_bid + prev_ask)/2)
      if (use_quotes_for_price_change){
        entries <- length(quotes_vector)
        if (entries > L){
          quotes_vector <- quotes_vector[2:entries]
        }
      }
      next
    }
    else{ # This is a trade
      if (prev_symbol != 'X'){ use_quotes <- T }
      #print("Parsing Trades")
    }
    j <- j+1
    
		time <- strptime(trades[i+q,"time"],"%H:%M:%OS")
		volume <- as.numeric(trades[i+q,"size"])
		price <- as.numeric(trades[i+q,"price"])
    
    #print(paste("SIZE: ", volume))
    #print(paste("Cum Vol: ", volume_count))
		if (first_record){ #first record
			start_t <- time
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
      print("filled one bucket")
			b <- 1.0
			if (use_gaussian){ sigma <- var(gaussian_sigma_vector) } 
      if(!is.na(sigma) && !(sigma==0)){
        #print(gaussian_sigma_vector)
        print("gaussian b")
        b <- 2*pnorm((price-prev_price)/sigma)  - 1 
			}
      else{
        #print(paste("DEBUG: ", prev_bid, prev_ask, price))
        
        if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                       use_quotes, prev_bid, prev_ask)){	
          print(paste("DEBUG: BUY"))      
          b <- 1.0
        }else{
          print(paste("DEBUG: SELL"))
          b <- -1.0
        }
      }
			OI <- OI + b*residual_volume
      #print(paste("Plus total: ", OI, ", residual: ", residual_volume))
      #print(paste("Updating OI: ", OI, b, residual_volume))
			if (volume_count + volume > bucket_volume_size){ #split order
				trades[i+q,"size"] <- volume_count + volume - bucket_volume_size
				#print(paste("Split volume to ", trades[i,"size"], "I: ", i))
			}else{
			  i <- i+1
			}
      
      if (use_quotes_for_price_change){
        price_returns <- c(price_returns, log((prev_bid+prev_ask)/2) - log(EMA(quotes_vector, L)[L])) 
      }else{
        price_returns <- c(price_returns, log(price/prev_bucket_price))
      }
      
			price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      #print(paste("Vector Size: ", length(price_returns_finer_grained)))
      #print(paste("VAR: ", var(price_returns_finer_grained)))
      if ((j %% L) == 0){ # using L number of buckets
			  price_returns_finer_grained <- vector()
      }
      if (signed){
        OI_buckets <- c(OI_buckets, OI / bucket_volume_size) #update OI_bucket  
        #print (paste("SOI is: ",OI))
      }else{
        OI_buckets <- c(OI_buckets, abs(OI / bucket_volume_size)) #update OI_bucket 
        #print (paste("OI is: ",OI))
      }
			
			OI <- 0.0
      #print("Reset OI.")
			bucket_volume <-0.0
			volume_count <-0.0
			start_t <-time
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
		if(((time - start_t) > m_interval) || (i==length(trades[,1]))){ 
      print("filled one bin--->")
			if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule,
                     use_quotes, prev_bid, prev_ask)){
			  print(paste("DEBUG: ", prev_bid, prev_ask))
				OI <- OI + bucket_volume
			}else{
        print(paste("DEBUG: SELL"))
				OI <- OI - bucket_volume
			}
      #print(paste("Plus total-bin: ", OI, ", vol: ", bucket_volume))
			bucket_volume <- 0.0 #reset
			#volume_count <- volume_count + volume
			start_t <- time
			prev_prev_price <- prev_price
			prev_price <- price
		}
    i <- i+1
	}
  
  OI_vs_delta_prices <- cbind(OI_buckets, price_returns, price_volatilities)
	return (OI_vs_delta_prices)
}

plot_model_stat<-function(xs, ys){
  one_factor_model <- lm(ys~xs)
  print(summary(one_factor_model))
  plot(xs, ys)
  abline(lm(ys~xs))
  return(one_factor_model)
}
