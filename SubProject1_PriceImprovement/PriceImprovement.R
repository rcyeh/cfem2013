library(rhdf5)
library(zoo)
library(TTR) 
library(MASS) 

qsplit <- function(d) { return (c( floor(d / 256 / 256 / 256), floor(d / 256 / 256) %% 256, floor(d / 256) %% 256, d %% 256)) }

hasq <- function(qual, v) { unlist(lapply(v, function(x) { qual %in% qsplit(x) })) }

# Delay time in miliseconds
delay_quotes_xms <- function(data_a, delay_time){ 
  data_a$exchange_time[data_a$type == 'Q'] <- data_a$exchange_time[data_a$type == 'Q'] + delay_time*1000
  return(data_a[order(data_a$exchange_time), ])
}

# Designed to reduce the # of quotes necessary for processing data
# This function filters out all the intermediary quotes that are not trailing or proceeding 
# Trade Entries
filter_trades_quotes <- function(a, volume_limit=10000){ 
  indd <- which(a$type == 'T' & a$size <= volume_limit);
  indd <- indd[! (hasq(32, a$quals[indd]) | hasq(59, a$quals[indd]))];
  ind = unlist(as.list(rbind(indd - 1, indd)));
  ind = ind[ind > 0];
  trades_quotes <- a[ind,]
  return (trades_quotes)
}

# Classify quotes based on where does the trade price fall under 
classify_bid_ask <- function(bid, ask, price){
  bid_diff <- price - bid
  ask_diff <- price - ask
  if (abs(bid_diff) < abs(ask_diff)){ # trade price closer to bid => taking the bid => assign sell
    return (-1)
  }
  else if (abs(bid_diff) > abs(ask_diff)){ # trade price closer to ask => assign buy
    return (1)
  }
  else{
    return (0) # assign neutral (i.e. not classifying)
  }
}

# This funtion uses recursion for continuous tick test, used together with 
# Function "classify", use with caution, may cause memory problem
classify_trades <- function(trade_vec, rep_times){
  
  class <- vector()
  if (rep_times > 20){
    repetition <<- repetition + rep_times
    return (class)
  }
  
  num_records <- length(trade_vec)
  i <- num_records
  if (num_records < 2){
    indicator <<- indicator + 1
    return (c(0, rep_times+1))
  }
  while(i > 1){
    record <- trade_vec[i]
    if (record > trade_vec[i-1]){
      class <- c(class, rep(1, rep_times+1+repetition))
      indicator <<- indicator + rep_times+1+repetition
      repetition <<-0
      break
    }else if (record < trade_vec[i-1]){
      class <- c(class, rep(-1, rep_times+1+repetition))
      indicator <<- indicator + rep_times+1+repetition
      repetition <<-0
      break
    }else{
      return( c(class, (classify_trades(trade_vec[-c(num_records:i)], rep_times+1))))
    }
    i <- i-1
  }
  
  return (class)
}

indicator <<- 0
repetition <<- 0
classify <- function(trade_vec){
  repetition <<- 0
  indicator <<- 0
  classifications <- vector()
  l <- length(trade_vec)
  while (indicator < l-1){
    up <- max(0,l-indicator-repetition)
    classifications <- c(classifications, classify_trades(trade_vec[1:up], 0))
  }
  return (rev(classifications))
}

# This function classifies every trade within the trades_quotes vector
# Please be careful setting "break_even" parameter, it is used for
# additional tick test in case trade price falls in the middle of NBBO
# Turning on the option is time and memory consuming
classify_buy_sell <- function(trades_quotes, break_even=F){
  classification <- vector()
  l <- length(trades_quotes$price)
  bid <- 0
  ask <- 0
  for (i in 1:l){
    if (trades_quotes$type[i]=='Q'){
      bid <- trades_quotes$bid[i]
      ask <- trades_quotes$ask[i]
      next
    }
    else{
      if (!break_even){
        b_or_s <- classify_bid_ask(bid, ask, trades_quotes$price[i]) 
      }
      else{
        #b_or_s <- classify(price, trades_quotes$price[which(trades_quotes[1:i,]$type=='T')])
        b_or_s <- classify_bid_ask(bid, ask, trades_quotes$price[i]) 
        if (b_or_s == 0){
          b_or_s <- classify(trades_quotes$price[which(trades_quotes[1:i,]$type=='T')])
        }
      }
      classification <- c(classification, b_or_s)
    }
  }
  return (classification[-length(classification)])
}

last <- function (a){
  return (a[length(a)])
}

second_to_last <- function (a){
  return (a[length(a)-1])
}

filter <- function(a, sym='Q', exclude_qual=T){ 
  indd <- which(a$type == sym);
  if (exclude_qual==T){
    indd <- indd[! (hasq(32, a$quals[indd]) | hasq(59, a$quals[indd]))];
  }
  trades_quotes <- a[indd,]
  return (trades_quotes)
}

trim <- function(x) {
  medx <- median(x);
  madx <- mad(x);
  print(madx)
  if(madx != 0){
    x = pmin(x, medx + 2 * madx);
    x = pmax(x, medx - 2 * madx);
  }
  return (x);
}

group_filter <-function(group2, group1, filter_data){
  t_f <- group1 %in% group2
  ind <- which(t_f==TRUE)
  return (filter_data[ind,])
}


