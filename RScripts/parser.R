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


#interval in seconds
calc_OI_by_time_buckets <- function(interval, trades, bucket_volume_size, signed=F
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
  price_changes <- vector()
  first_record <- T
  i <- 1
  while (i != length(trades[,1])){    
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
		if (volume_count + volume >= bucket_volume_size){ #filled one bucket
			residual_volume <- bucket_volume_size - bucket_volume
      #print("filled one bucket")
      if (assign_buy(prev_prev_price, prev_price, price, use_sub_penny_rule, use_momentum_rule)){				
              OI <- OI + residual_volume
      }else{
              OI <- OI - residual_volume
      }
			if (volume_count + volume > bucket_volume_size){ #split order
				trades[i,"size"] <- volume_count + volume - bucket_volume_size
				#print(paste("Split volume to ", trades[i,"size"], "I: ", i))
			}else{
			  i <- i+1
			}
      
      price_changes <- c(price_changes, log(price/prev_bucket_price))
      
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
		if(((time - start_t) > m_interval) || (i==length(trades[,1]))){ #Let's update OI
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
  OI_vs_delta_prices <- cbind(OI_buckets, price_changes)
	#return (OI_buckets)
	return (OI_vs_delta_prices)
}

plot_model_stat<-function(xs, ys){
  one_factor_model <- lm(ys~xs)
  print(summary(one_factor_model))
  plot(xs, ys)
  abline(lm(ys~xs))
  return(one_factor_model)
}

a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')


quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

head(quotes)

trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

#potential trades to exclude
#trades <- a[a$type == 'T' & (hasq(32,a$quals) | hasq(59,a$quals)),unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

L <- 50
bucket_size <- 1000
time_bin <- 60

OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size)
total_entry <- length(OI_buckets_delta_prices[,1])

OI_buckets_delta_prices_sma <- SMA(OI_buckets_delta_prices[,2], L)[L:total_entry]
OI_buckets_delta_prices_ema <- EMA(OI_buckets_delta_prices[,2], L)[L:total_entry]
TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]

OI_buckets_delta_prices_ns <- calc_OI_by_time_buckets(time_bin,trades,bucket_size,F,T)

# Regress against Instantaneous Price Change -> No significance
plot_model_stat(TR_VPIN, OI_buckets_delta_prices[L:total_entry,2])
# Do not Apply sub-penny rule, do not take subpenny price change into account
plot_model_stat(TR_VPIN, OI_buckets_delta_prices_ns[L:total_entry,2])

# Both SMA & EMA have significant p-value with tiny R^2
plot_model_stat(TR_VPIN, OI_buckets_delta_prices_sma)
plot_model_stat(TR_VPIN, OI_buckets_delta_prices_ema)


# Regress against rolling realized standard deviation -> Mechanistic Significance
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
plot_model_stat(TR_VPIN, MA_volume_delta)

bucket_size <- 3000
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size)
TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]
plot_model_stat(TR_VPIN, MA_volume_delta)
