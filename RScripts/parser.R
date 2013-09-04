library(rhdf5)
library(zoo)
library(TTR)

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

assign_buy <- function(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule){
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

calc_volat_by_volumes <- function(trades, bucket_volume_size, realized_vol_period) {
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
      volume_volatilities <- c(volume_volatilities, var(modify_series(vol_vector, burn * bucket_volume_size)))
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
                                      , use_sub_penny_rule = T){
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
  first_record <- T
  i <- 1
  j <- 0
  k <- 0
  
  while (i != length(trades[,1])){ 
    k <- k+1
    volume <- as.numeric(trades[i,"size"])
    price <- as.numeric(trades[i,"price"])
    
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
      #print("filled one bin")
      j <- j+1
      b <- 1.0
      if (use_gaussian){
        sigma <- var(gaussian_sigma_vector)
        if (sigma == 0){ b<- 0.0 }
        else{ b <- 2*pnorm((price-prev_price)/sigma) - 1 }
      }else{
        if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule)){
          b <- 1.0
        }else{
          b <- -1.0
        }
        OI <- OI + b*fixed_volume 
      }
      residual_volume <- residual_volume - fixed_volume
      if ( residual_volume > fixed_volume){ 
        trades[i,"size"] <- 0.0
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
  first_record <- T
  i <- 1
  j <- 0
  
  while (i != length(trades[,1])){   
    j <- j+1
    #print(paste("I: ", i))
		time <- strptime(trades[i,"time"],"%H:%M:%OS")
		volume <- as.numeric(trades[i,"size"])
		price <- as.numeric(trades[i,"price"])
    
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
    
		if (volume_count + volume >= bucket_volume_size){ #filled one bucket
			residual_volume <- bucket_volume_size - bucket_volume
      #print("filled one bucket")
			b <- 1.0
			if (use_gaussian){
			  sigma <- var(gaussian_sigma_vector)
			  if (sigma == 0){ b<- 0.0 }
			  else{ b <- 2*pnorm((price-prev_price)/sigma)  - 1 }
			}
      else{
        if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule)){				
                b <- 1.0
        }else{
                b <- -1.0
        }
      }
			OI <- OI + b*residual_volume
			if (volume_count + volume > bucket_volume_size){ #split order
				trades[i,"size"] <- volume_count + volume - bucket_volume_size
				#print(paste("Split volume to ", trades[i,"size"], "I: ", i))
			}else{
			  i <- i+1
			}
      
      price_returns <- c(price_returns, log(price/prev_bucket_price))
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
			if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule)){
				OI <- OI + bucket_volume
			}else{
				OI <- OI - bucket_volume
			}
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
