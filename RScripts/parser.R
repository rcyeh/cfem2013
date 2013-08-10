library(rhdf5)
library(zoo)

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }


#interval in seconds
calc_OI_by_time_buckets <- function(interval, trades, bucket_volume_size, signed=F
                                    #, use_momentum_rule=F
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
  
  i <- 1
  while (i != length(trades[,1])){    
    #print(paste("I: ", i))
		time <- strptime(trades[i,"time"],"%H:%M:%OS")
		volume <- as.numeric(trades[i,"size"])
		price <- as.numeric(trades[i,"price"])
		if (start_t==0){ #first record
			start_t <- time
			prev_price <- price
      prev_bucket_price <- price
			bucket_volume <- volume
			volume_count <- volume
      i <- i+1
			next
		}else{
			if (volume_count + volume >= bucket_volume_size){ #filled one bucket
				residual_volume <- bucket_volume_size - bucket_volume
				if (price >= prev_price){
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
				prev_price <- price
        prev_bucket_price <- price
				next
			}
			else{
				bucket_volume <- bucket_volume + volume
	      volume_count <- bucket_volume
			}
			if(((time - start_t) > m_interval) || (i==length(trades[,1]))){ #Let's update OI
				if (price > prev_price){
					OI <- OI + bucket_volume
				}else{
					OI <- OI - bucket_volume
				}
				bucket_volume <- 0.0 #reset
				start_t <- time
				prev_price <- price
			}
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
}

a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')


quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

head(quotes)

trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
#head(trades)
#trades <- trades[1:3000,]
#a[a$type == 'T' & (hasq(32,a$quals) | hasq(59,a$quals)),unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

OI_buckets_delta_prices <- calc_OI_by_time_buckets(10,trades,1000)
L <- 50
total_entry <- length(OI_buckets_delta_prices[,1])
TR_VPIN <- rollmean(OI_buckets_delta_prices[,1], L)
MA_P_Changes <- rollmean(OI_buckets_delta_prices[,2], L)

# No significance
plot_model_stat(TR_VPIN, OI_buckets_delta_prices[L:total_entry,2])

# Significance, but with tiny R^2
plot_model_stat(TR_VPIN, MA_P_Changes)

