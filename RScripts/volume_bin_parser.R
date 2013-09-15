
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