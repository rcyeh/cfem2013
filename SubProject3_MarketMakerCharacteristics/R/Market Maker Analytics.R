library(rhdf5)
setwd("C:/Path/To/Data/Directory")

#Helper Functions
get_fraction <- function (num, denom){
  if (length(num)==0){
    return (0)
  }else{
    return (num/denom)
  }
}

#Get Individual Volume Ratio
get_vol_ratio <- function(tick, grp_by_symbols, grp_by_avg ){  
  #Total volume for the symbols
  total_exe_vol <- sum(grp_by_symbols[which(grp_by_symbols$Group.1==paste(tick)),10-2]) 
  tick_agg <- grp_by_symbols[which(grp_by_symbols$Group.1==paste(tick)),]
   
  #Get Market Order, Limit Order, Other Limit Order Volume
  market_vol <- tick_agg[which(tick_agg$Group.2==11),10-2]
  limit_vol <- tick_agg[which(tick_agg$Group.2==12),10-2]
  other_limit_vol <- tick_agg[which(tick_agg$Group.2==13),10-2]
  other_limit_vol <- other_limit_vol + tick_agg[which(tick_agg$Group.2==14),10-2]
  other_limit_vol <- other_limit_vol + tick_agg[which(tick_agg$Group.2==15),10-2]
  
  #Get Price Improvement Volume, at the quote volume, outside of quote volume
  vol_with_price_imp <- sum(grp_by_symbols[which(grp_by_symbols$Group.1==paste(tick)), 19-2])
  exe_at_quotes <- sum(grp_by_symbols[which(grp_by_symbols$Group.1==paste(tick)), 22-2])
  outside_quotes <- sum(grp_by_symbols[which(grp_by_symbols$Group.1==paste(tick)), 24-2])

  #Get Effective Spread
  eff_spread <- sum(grp_by_avg[which(grp_by_symbols$Group.1==paste(tick)), 18-2])
  
  #Error checking, if there is no volume, assign zero
  if(length(total_exe_vol)==0) {total_exe_vol = 0}
  if(length(tick_agg)==0){tick_agg = 0}
  if(length(market_vol)==0){market_vol = 0}
  if(length(limit_vol)==0){limit_vol = 0}
  if(length(other_limit_vol)==0){other_limit_vol= 0}
  if(length(vol_with_price_imp)==0){vol_with_price_imp=0}
  if(length(exe_at_quotes)==0){exe_at_quotes = 0}
  if(length(outside_quotes)==0){outside_quotes = 0}
  if(length(eff_spread)==0){eff_spread = 0}  
  #Aggregate and save data into vector
  out = c(total_exe_vol,market_vol,limit_vol,other_limit_vol,vol_with_price_imp,exe_at_quotes,outside_quotes, eff_spread)
  return(out)
}

#Get the volume ratios given market maker and date
tickers<-read.delim("ticker_list.csv",header=F,sep=",")$V1
dates = c('201304','201304')
mama = c('UBSS','TRIM','SOHO','SGMA','NITE','ETMM','CDRG','ATDF')
  symbols = c()
  grp_by_symbols = c()
  for (i in 1:length(mama))
  {
    #Perform all the read first and clean up the data
    print(paste("reading from",mama[i],'...'))
    filename = paste(mama[i], dates, ".dat", sep='')
    tdata <- read.delim(file=filename, sep='|', na.string='.')
    tdata[is.na(tdata)] <- 0
    tgrp_by_symbols <- aggregate(tdata[,5:26], by=list(tdata[,4],tdata[,5]), FUN = sum)
    sgrp_by_symbols <- aggregate(tdata[,5:26], by=list(tdata[,4],tdata[,5]), FUN = mean)
    symbols <- union(symbols, unique(tgrp_by_symbols$Group.1))    
    eval(parse(text=(paste(mama[i],"_sum <- tgrp_by_symbols",sep=''))))
    eval(parse(text=(paste(mama[i],"_avg <- sgrp_by_symbols",sep=''))))
  }
  
  final_vec = c()
  for (m in 1:length(tickers)){
    # Analyze stock by stock
    tick <- tickers[m]
	# Display progress as this will take some time
    print(paste(tick,' ',round(100*m/length(tickers),2),'%',sep=''))
    sym_vec = rep(0, 8)
	# Get ratio for each ticker by calling helper function
    for (i in 1:length(mama)){
       eval(parse(text=(    paste("result <- get_vol_ratio(tick,",mama[i],"_sum, ",mama[i],"_avg)" ,sep='')  )))
       sym_vec <- result+sym_vec
    }
	# Add individual ticker result to the others
    total_exe_vol <- sym_vec[1] 
    market_vol <- sym_vec[2]
    limit_vol <- sym_vec[3]
    other_limit_vol <- sym_vec[4]
    vol_with_price_imp <- sym_vec[5]
    exe_at_quotes <- sym_vec[6]
    outside_quotes <- sym_vec[7]
    eff_spread <- sym_vec[8]/length(mama)
    mkt_order_frac <- get_fraction(market_vol, total_exe_vol)
    lim_order_frac <- get_fraction(limit_vol, total_exe_vol)   
    other_lim_order_frac <- get_fraction(other_limit_vol, total_exe_vol)
    price_imp_frac <- get_fraction(vol_with_price_imp, total_exe_vol)
    at_quote_frac <- get_fraction(exe_at_quotes, total_exe_vol)
    os_quote_frac <- get_fraction(outside_quotes, total_exe_vol)
    final_vec <- rbind(final_vec, c(tick, mkt_order_frac, lim_order_frac, other_lim_order_frac, price_imp_frac, vol_with_price_imp, at_quote_frac, os_quote_frac, eff_spread, total_exe_vol))
  }
  
  #Store and write the result to a csv file
  colnames(final_vec) = c("tick","mkt_order_frac", "lim_order_frac", "other_lim_order_frac", "price_imp_frac", "vol_with_price_imp", "at_quote_frac", "os_quote_frac", "eff_spread", "total_exe_vol")
  write.table(cbind(tickers,final_vec),"final_vector.csv")