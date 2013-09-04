#Please change your source file path
source("C:/CFEM2013/RScripts/parser.R")

a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')


quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

head(quotes)

trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

#potential trades to exclude
#trades <- a[a$type == 'T' & (hasq(32,a$quals) | hasq(59,a$quals)),unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]

L <- 50
fixed_bin <- 200 
bucket_size <- 1000 # must be a multiple of fixed_bin for FB_VPIN to work properly
time_bin <- 60

# Please uncomment to test the desired OI used by VPINs (TR_VPIN, FB_VPIN, bulk-volume TR, bulk-volume FB)

# TR_VPIN
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, F)

# TR_VPIN with bulk volume
#OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, T)

# FB_VPIN without bulk volume
#OI_buckets_delta_prices <- calc_OI_by_volume_buckets(fixed_bin,trades,bucket_size, F)

# FB_VPIN with bulk volume
OI_buckets_delta_prices <- calc_OI_by_volume_buckets(fixed_bin,trades,bucket_size, T)


total_entry <- length(OI_buckets_delta_prices[,1])

OI_buckets_delta_prices_sma <- SMA(OI_buckets_delta_prices[,2], L)[L:total_entry]
OI_buckets_delta_prices_ema <- EMA(OI_buckets_delta_prices[,2], L)[L:total_entry]
price_volatilities <- OI_buckets_delta_prices[,3]
price_volatilities[is.na(price_volatilities)] <- 0
price_volatilities <- SMA(price_volatilities, L)[L:total_entry]

TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]
#OI_buckets_delta_prices_ns <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, F, L, F, F, T)

# Regress against Instantaneous Price Change -> No significance
plot_model_stat(TR_VPIN, OI_buckets_delta_prices[L:total_entry,2])
# Do not Apply sub-penny rule, do not take subpenny price change into account
#plot_model_stat(TR_VPIN, OI_buckets_delta_prices_ns[L:total_entry,2])

# Both SMA & EMA have significant p-value with tiny R^2
plot_model_stat(TR_VPIN, OI_buckets_delta_prices_sma)
plot_model_stat(TR_VPIN, OI_buckets_delta_prices_ema)


# Regress against rolling realized standard deviation -> Mechanistic Significance
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
plot_model_stat(TR_VPIN, MA_volume_delta)
plot_model_stat(TR_VPIN, price_volatilities)

####################################################################################

bucket_size <- 3000
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size)
total_entry <- length(OI_buckets_delta_prices[,1])
TR_VPIN <- SMA(OI_buckets_delta_prices[,1], L)[L:total_entry]
plot_model_stat(TR_VPIN, MA_volume_delta)
