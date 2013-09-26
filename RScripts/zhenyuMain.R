#Please change your source file path
source("C:/cfem2013_main/RScripts/parser.R")

a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')

quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

head(quotes)

trades_quotes <- filter_trades_quotes(a)
trades_quotes_ema <- filter_trades_quotes_EMA(a, 0.05)
  
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
#potential trades to exclude
#trades_exc <- a[a$type == 'T' & (hasq(32,a$quals) | hasq(59,a$quals)),unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
trades_exc_ind <- which(a$type == 'T' & (hasq(32,a$quals) | hasq(59,a$quals)))

l_trades = dim(trades)[1]
indd = which(a$type == "T")
ind = rep(0,2*l_trades)
for ( i in 1:l_trades){
  ind[(1+(i-1)*2):(2*i)] <- seq(indd[i]-1,indd[i],1)
}
trades_quotes <- a[ind,]
trades_quotes <- trades_quotes[-trades_exc_ind,]
                

L <- 50
fixed_bin <- 200 
bucket_size <- 1000 # must be a multiple of fixed_bin for FB_VPIN to work properly
time_bin <- 60

# Please uncomment to test the desired OI used by VPINs (TR_VPIN, FB_VPIN, bulk-volume TR, bulk-volume FB)

# TR_VPIN
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, F)
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades_quotes,bucket_size, F, L, T)

# TR_VPIN with bulk volume
#OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, T)

# FB_VPIN without bulk volume
#OI_buckets_delta_prices <- calc_OI_by_volume_buckets(fixed_bin,trades,bucket_size, F)

# FB_VPIN with bulk volume
#OI_buckets_delta_prices <- calc_OI_by_volume_buckets(fixed_bin,trades,bucket_size, T)


total_entry <- length(SOI_buckets_delta_prices[,1])

SOI_buckets_delta_prices_sma <- SMA(SOI_buckets_delta_prices[,2], L)[(L+1):total_entry]
SOI_buckets_delta_prices_ema <- EMA(SOI_buckets_delta_prices[,2], L)[(L+1):total_entry]
price_volatilities <- OI_buckets_delta_prices[,3]
price_volatilities[is.na(price_volatilities)] <- 0
price_volatilities <- SMA(price_volatilities, L)[L:total_entry]

TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]
S_TR_VPIN <- SMA(SOI_buckets_delta_prices[,1], L)[L:(total_entry-1)]
#OI_buckets_delta_prices_ns <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, F, L, F, F, T)

# Regress against Instantaneous Price Change -> No significance
plot_model_stat(S_TR_VPIN, SOI_buckets_delta_prices[(L+1):total_entry,2])
# Do not Apply sub-penny rule, do not take subpenny price change into account
#plot_model_stat(TR_VPIN, OI_buckets_delta_prices_ns[L:total_entry,2])

# Both SMA & EMA have significant p-value with tiny R^2
plot_model_stat(S_TR_VPIN, SOI_buckets_delta_prices_sma)
plot_model_stat(S_TR_VPIN, SOI_buckets_delta_prices_ema)


##############################[Volatility Test]#################################

# Regress against rolling realized standard deviation -> Mechanistic Significance
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
plot_model_stat(TR_VPIN, MA_volume_delta)
plot_model_stat(TR_VPIN, price_volatilities)

################################################################################

bucket_size <- 3000
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size)
total_entry <- length(OI_buckets_delta_prices[,1])
TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]
plot_model_stat(TR_VPIN, MA_volume_delta)

