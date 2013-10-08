library(rhdf5)
library(zoo)
library(TTR) 
library(MASS) 

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

assign_buy <- function(prev_prev_p, prev_p, p){
  if (p==prev_p){
    if (prev_prev_p < p){
      return (1)
    }else if (prev_prev_p > p){
      return (-1)
    }else{
      return (0)
    }
  }else{
    if (p > prev_p){
      return (1)
    }else if (p < prev_p){
      return (-1)
    }else{
      return (0)
    }
  }
}

calc_SOI <- function(interval, trades, use_trades=T){
  
  m_interval <- interval *1000 #/ 60.0
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
  price_volatilities <- vector()
  price_returns_finer_grained <- vector()
  
  prev_symbol <- 'X'
  update_first_quotes <- T
  prev_prev_bid <- 0.0
  prev_prev_ask <- 0.0
  prev_bid <- 0.0
  prev_ask <- 0.0
  bid <- 0.0
  ask <- 0.0
  prev_bucket_mid <-0.0
  ema_bid <-0.0
  ema_ask <-0.0
  use_quotes <- F
  
  first_record <- T
  qc <- 0
  i <- 1
  total_volume <- 0.0
  
  while (i+qc != length(trades[,1])){
    #print(paste("I&Q: ", i, q))
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_prev_bid <- prev_bid
      prev_prev_ask <- prev_ask
      prev_bid <- bid
      prev_ask <- ask
      bid <- trades$bid[i+qc]
      ask <- trades$ask[i+qc]
      ema_bid <- trades$ema_bid[i+qc]
      ema_ask <- trades$ema_ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (first_record){ #first record
        price <- trades$price[i+qc]
        start_t <- trades$exchange_time[i+qc]
        prev_price <- price
        prev_prev_price <- price
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
    
    if (use_quotes){
      b <- assign_buy_bid_ask(ema_bid, ema_ask, price)
    }else{
      b <- assign_buy(prev_prev_price, prev_price, price)
    }
    
    OI <- OI + b*volume
    total_volume <- total_volume + volume

    
    if(((time - start_t) > m_interval)|| ((i+qc)==length(trades[,1]))){  
      #print ("Filled one Bucket")
      #price_returns <- c(price_returns, log(price/prev_bucket_price))
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      price_returns_finer_grained <- vector()
      
      price_returns <- c(price_returns, log(((bid+ask)/2)/prev_bucket_mid))
      prev_bucket_price <- price
      prev_bucket_mid <- (bid+ask)/2
      OI_buckets <- c(OI_buckets, OI/total_volume)
      total_volume <- 0.0
      OI <- 0.0
      start_t <- time
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  price_volatilities[is.na(price_volatilities)] <-0
  return (cbind(OI_buckets, price_returns, price_volatilities))
}

#interval in seconds
calc_OI_by_time_buckets <- function(interval
                                    , trades
                                    , bucket_volume_size
                                    , use_trades = F
                                    , use_gaussian = F
                                    , signed = T
) {
  m_interval <- interval * 1000 #use miliseconds
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
  update_first_quotes <- T
  prev_prev_bid <- 0.0
  prev_prev_ask <- 0.0
  prev_bid <- 0.0
  prev_ask <- 0.0
  bid <- 0.0
  ask <- 0.0
  ema_bid <- 0.0
  ema_ask <- 0.0
  prev_bucket_mid <- 0.0
  L <- 25
  
  use_quotes <- F
  qc <- 0; trlen <- length(trades[,1])
  
  while (i+qc <= trlen){
    type <- trades$type[i+qc]
    
    if (type == 'Q'){ # This is a Quote
      prev_symbol <- 'Q'
      prev_prev_bid <- prev_bid
      prev_prev_ask <- prev_ask
      prev_bid <- bid
      prev_ask <- ask
      bid <- trades$bid[i+qc]
      ask <- trades$ask[i+qc]

      ema_bid <- trades$ema_bid[i+qc]
      ema_ask <- trades$ema_ask[i+qc]
      qc <- qc+1
      next
    }
    else{ # This is a trade
      if (!use_trades){if (prev_symbol != 'X'){ use_quotes <- T }}
    }
    j <- j+1
    
    tm <- trades$exchange_time[i+qc] #just remember to format time before hand
    volume <- trades$size[i+qc]
    price <- trades$price[i+qc]
    
    if (first_record){ #first record
      start_t <- tm
      prev_price <- price
      prev_prev_price <- price
      prev_bucket_price <- price
      prev_bucket_mid <- (bid+ask)/2
      first_record <- F
    }
    
    price_returns_finer_grained <- c(price_returns_finer_grained, log(price/prev_price))
    #gaussian_sigma_vector <- c(gaussian_sigma_vector, price-prev_price)
    #entries <- length(gaussian_sigma_vector)
    #if (entries > L){
    #  gaussian_sigma_vector <- gaussian_sigma_vector[2:entries]
    #}
    
    if (volume_count + volume >= bucket_volume_size){ #filled one bucket
      residual_volume <- bucket_volume_size - volume_count
      #print("filled one bucket")
      b <- 1.0
      if (use_gaussian){ sigma <- var(gaussian_sigma_vector) } 
      if(!is.na(sigma) && !(sigma==0)){
        b <- 2*pnorm((price-prev_price)/sigma)  - 1 
      }
      else{
        if (use_quotes){
          b <- assign_buy_bid_ask(ema_bid, ema_ask, price)
        }else{
          b <- assign_buy(prev_prev_price, prev_price, price)
        }
      }
      OI <- OI + b*residual_volume
      if (volume_count + volume > bucket_volume_size){ #split order
        trades$size[i+qc] <- volume_count + volume - bucket_volume_size
      }else{
        i <- i+1
      }

      #price_returns <- c(price_returns, log(((bid+ask)/2)/((ema_bid+ema_ask)/2)))
      price_returns <- c(price_returns, log(((bid+ask)/2)/prev_bucket_mid))
      
      price_volatilities <- c(price_volatilities, var(price_returns_finer_grained))
      
      #if ((j %% L) == 0){ # using L number of buckets
        price_returns_finer_grained <- vector()
      #}
      
      OI_buckets <- c(OI_buckets, OI / bucket_volume_size) #update OI_bucket  
      
      OI <- 0.0
      #print("Reset OI.")
      bucket_volume <-0.0
      volume_count <-0.0
      start_t <-tm
      prev_prev_price <- prev_price
      prev_price <- price
      prev_bucket_price <- price
      prev_bucket_mid <- (bid+ask)/2
      next
    }
    else{
      bucket_volume <- bucket_volume + volume
      volume_count <- volume_count + volume
    }
    
    #TR-VPIN time interval is reached, update OI vector
    if((tm - start_t) > m_interval){ 
      #print("filled one bin--->")
      if (use_quotes){
        b <- assign_buy_bid_ask(ema_bid, ema_ask, price)
      }else{
        b <- assign_buy(prev_prev_price, prev_price, price)
      }
      OI <- OI + b*bucket_volume
      
      bucket_volume <- 0.0 #reset
      start_t <- tm
      prev_prev_price <- prev_price
      prev_price <- price
    }
    i <- i+1
  }
  
  if (!signed){
	  OI_buckets = abs(OI_buckets);
  }
  
  price_volatilities[is.na(price_volatilities)] <-0
  OI_vs_delta_prices <- cbind(OI_buckets, price_returns, price_volatilities)
  #OI_vs_delta_prices <- cbind(OI_buckets, price_returns)
  return (OI_vs_delta_prices)
}


plot_one_factor_model <-function(xs, ys,xlabb="x",ylabb="y",mmain =""){
  one_factor_model <- lm(ys~xs)
  print(summary(one_factor_model))
  plot(xs, ys,xlab=xlabb,ylab=ylabb,main=mmain)
  abline(lm(ys~xs))
  return(one_factor_model)
}

delay_quotes_xms <- function(data_a, delay_time){ #delay time in miliseconds
  #options(digits.secs=6)
  #data_a$time <- strptime(data_a$time,"%H:%M:%OS")
  data_a$exchange_time[data_a$type == 'Q'] <- data_a$exchange_time[data_a$type == 'Q'] + delay_time*1000
  return(data_a[order(data_a$exchange_time), ])
}

cal_quotes_EMA <- function(a,alpha=1){
  delta_t <- c(0,diff(a$exchange_time[a$type == "Q"]/100000))
  weights <- cumprod(exp(alpha*delta_t))
  mid_quotes <- 0.5 * (a$bid[a$type == "Q"]+a$ask[a$type == "Q"]);
  quotes_ema <- cumsum(weights*mid_quotes)/cumsum(weights)
  #plot(index(mid_quotes),mid_quotes,type="l")
  #lines(index(EMA_q),EMA_q,col="red")
  a$ema_mid[a$type == "Q"] <- quotes_ema
  return(a)
}

#Somehow produces different results than EMA1
cal_quotes_EMA3 <- function(a,alpha=1){
  dt <- c(1e5, diff(a$exchange_time[a$type == 'Q']))
  iw <- exp(-dt*alpha)
  mid <- 0.5 * (a$bid[a$type == 'Q'] + a$ask[a$type == 'Q'])
  rs <<- 0
  rw <<- 0
  emamid <- apply(data.frame(iw, mid), 1, function (x) { rs <<- rs * x[1] + x[2]; rw <<- rw * x[1] + 1; return (rs/rw); })
  a$ema_mid[a$type == "Q"] <- emamid
  return (a)
}

cal_quotes_EMA_bid_ask <- function(a,alpha=1){
  a <- a[order(a$exchange_time),]
  delta_t <- c(0,diff(a$exchange_time[a$type == "Q"]/100000))
  weights <- cumprod(exp(alpha*delta_t))
  bid_ema <- cumsum(weights*a$bid[a$type == "Q"])/cumsum(weights)
  ask_ema <- cumsum(weights*a$ask[a$type == "Q"])/cumsum(weights)
  a$ema_bid[a$type == "Q"] <- bid_ema
  a$ema_ask[a$type == "Q"] <- ask_ema
  return(a)
}

filter_trades_quotes2 <- function(a, volume_limit=10000){ #designed to reduce the # of quotes necessary for processing data
  indd <- which(a$type == 'T' & a$size <= volume_limit);
  indd <- indd[! (hasq(32, a$quals[indd]) | hasq(59, a$quals[indd]))];
  ind = unlist(as.list(rbind(indd - 1, indd)));
  ind = ind[ind > 0];
  trades_quotes <- a[ind,]
  return (trades_quotes)
}

filter_trades_quotes3 <- function(a, volume_limit=10000){
  q1 <- a$quals %% 256
  q2 <- floor(a$quals / 256) %% 256
  q3 <- floor(a$quals / 256 / 256) %% 256
  q4 <- floor(a$quals / 256 / 256 / 256)
  #keep <- a$type == 'T' & a$size <= volume_limit & q1 != 32 & q2 != 32 & q3 != 32 & q4 != 32 & q1 != 59 & q2 != 59 & q3 != 59 & q4 != 59
  keep <- a$type == 'T' & q1 != 32 & q2 != 32 & q3 != 32 & q4 != 32 & q1 != 59 & q2 != 59 & q3 != 59 & q4 != 59
  trades_quotes <- a[keep | c(keep[2:length(keep)], FALSE),]
  block_trades <- which(trades_quotes$size > volume_limit && trades_quotes$type == 'T')
  trades_quotes$size[block_trades] = volume_limit
  return (trades_quotes)
}

buildmodel <- function(ticker,traindate,timebucket_size,threshold,scatterplot = F,fittedvsactualplot = T){
  a <- h5read(paste("ticks.",traindate,".h5",sep=""), paste("/ticks/",ticker,sep=""), bit64conversion='double')
  a <- a[order(a$exchange_time),]
  trades_quotes <- filter_trades_quotes3(a,threshold)
  SOI_buckets_delta_prices <- calc_SOI(timebucket_size,trades_quotes) 
  crossterm <- SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3])
  traindata <- data.frame(preturn = SOI_buckets_delta_prices[,2],crossterm = crossterm)
  lm10 <- lm(preturn ~ crossterm, data = traindata)
  r2 <- summary(lm10)$r.squared
  ### train scatter plot ###
  if(scatterplot & fittedvsactualplot){
    par(mfrow=c(2,1))
  }
  
  if(scatterplot){
    if(!fittedvsactualplot){
      par(mfrow=c(1,1))
    }
    plot(crossterm,SOI_buckets_delta_prices[,2],
         main= paste("Concurrent In Sample\n",ticker," ",timebucket_size,"s time bucket on ",traindate,sep=""),
         xlab= paste("SSOI\nPRetrun(k) = alpha + beta*SSOI(k)+epsilon(k), R^2=",round(r2*100,1),"%",sep=""),
         ylab=expression("PRetrun"))
    abline(lm10, col="red")
  }
  
  if(fittedvsactualplot){
    if(!scatterplot){
      par(mfrow=c(1,1))
    }
    plot(index(SOI_buckets_delta_prices[,2])*timebucket_size/60,SOI_buckets_delta_prices[,2],type="l",
         main= paste("Concurrent In Sample Plot \n",ticker," ",timebucket_size,"s time bucket on ",traindate,sep=""),
         xlab= paste("Time(minutes)\nPRetrun(k) = alpha + beta*SSOI(k)+epsilon(k), R^2=",round(r2*100,1),"%",sep=""),
         ylab=expression("PRetrun"))
    lines(index(lm10$fitted.values)*timebucket_size/60,lm10$fitted.values,col="red",lty=1)
  }
  return(lm10)
}

outtest <- function(model,ticker,testdate,timebucket_size,threshold,lineplot = T){
  a <- h5read(paste("ticks.",testdate,".h5",sep=""), paste("/ticks/",ticker,sep=""), bit64conversion='double')
  a <- a[order(a$exchange_time),]
  trades_quotes <- filter_trades_quotes3(a,threshold)
  SOI_buckets_delta_prices <- calc_SOI(timebucket_size,trades_quotes) 
  crossterm <- SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3])
  testdata <- data.frame(preturn = SOI_buckets_delta_prices[,2],crossterm = crossterm)
  testpredict <- predict.lm(model,newdata = testdata,interval="none")
  cdpr <- sum(((sign(testpredict) == sign(testdata$preturn))+0))/length(testpredict)
  if(lineplot){
    par(mfrow=c(1,1))
    plot(index(SOI_buckets_delta_prices[,2])*timebucket_size/60,SOI_buckets_delta_prices[,2],type="l",
         main= paste("Concurrent Out of Sample Test \n",ticker," ",timebucket_size,"s time bucket on ",testdate,sep=""),
         xlab= paste("Time(minutes)\nPRetrun(k) = alpha + beta*SSOI(k)+epsilon(k), CDPR =",round(cdpr*100,1),"%",sep=""),
         ylab=expression("PRetrun"))
    #CDPR: correct direction prediction Ratio
    lines(index(testpredict)*timebucket_size/60,testpredict,col="red",lty=1)
  }
}

