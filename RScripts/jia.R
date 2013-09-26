#install.packages("fBasics")
#install.packages("car")
#install.packages("aod")
library(aod)
library(fBasics)
library(car)

setwd("/Users/JiaXu/Documents/FE project 2013/RScripts")
source("parser.R")
setwd("/Users/JiaXu/Documents/FE project")
a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')
options(digits.secs=6)
a$time <- strptime(a$time,"%H:%M:%OS")
a$exchange_time <- strptime(a$exchange_time,"%H:%M:%OS")
a_test <- h5read("ticks.20130424.h5", "/ticks/AMZN", bit64conversion='double')
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
s_trades <- trades[with(trades, order(time)), ]



SPY <- read.csv("hello.csv",header=TRUE,sep=" ")
head(SPY)
SPY$type = "T"
#SOI_buckets_delta_prices <- calc_OI_by_time_buckets(5,(SPY[-which(SPY$size > 10000),])[1:70000,],10000, F, L, T)
#SOI_buckets_delta_prices_big <- calc_OI_by_time_buckets(30,(SPY[-which(SPY$size > 20000),])[1:50000,],20000, F, L, T)
SOI <- calc_SOI(30, SPY[1:100000,])
SOI2 <- calc_SOI(120, SPY[1:100000,])
SOI2 <- calc_SOI(360, SPY[-which(SPY$size>10000),][1:50000,])
L <- 50
fixed_bin <- 200 

##delay by latency
a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')
options(digits.secs=6)
a$time<-strptime(a$time,"%H:%M:%OS")
a$time <- a$time - a$latency*0.001
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
s_trades <- trades[with(trades, order(time)), ]
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(135, s_trades[-which(s_trades$size > 1000),], 10000, F, L, T)
#

time <- seq(30,180,30)
bucket <- seq(1000,10000,1000)
thres <- 20000
decay <- seq()
delay <- seq(0,0.1,0.005)
l_time <- length(time)
l_bucket <- length(bucket)
l_thres <- length(thres)
l_decay <- length(decay)
l_delay <- length(delay)
R2_finer2 <- c()

s_a <- a[with(a, order(time)), ]
for(j in 1:l_time){
  #for( k in 1:l_delay){
    for(i in 1:l_bucket){
      bucket_size <- bucket[i]
      time_bin <- time[j]
      #delay_1 <- delay[k]

      #SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,filter_trades_quotes_EMA(a,time_decay),],bucket_size, F, L, T)
     # trades_quotes <- filter_trades_quotes(delay_quotes_xms(a, delay_1))
      
      trades_quotes <- filter_trades_quotes(s_a)
      SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin, trades_quotes, bucket_size, F, L, T)
      SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin, trades_quotes, bucket_size, F, L, T)
     # l_prices = length(SOI_buckets_delta_prices[,2])
      SOI_buckets_delta_prices[,2][1:2] <-0
      r2 <- summary(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]))$r.squared
    #  print(paste(bucket[i],"_",time[j],"_",delay[k], ": ",r2))
      print(paste(bucket[i],"_",time[j],"_", ": ",r2))
      R2_finer2[paste(bucket[i],"_",time[j],"_",delay[k])] = r2
    }
  #}
}

time <- seq(30,180,30)
thres <- seq(10000,20000,10000)
l_time <- length(time)
l_thres <- length(thres)
R2_finer <- c()
for( k in 1:l_thres){
  for(j in 1:l_time){
      time_bin <- time[j]
      thres1 <- thres[k]
      SOI <- calc_SOI(time_bin, SPY[-which(SPY$size>thres1),][1:20000,])
      l_prices = length(SOI[,2])
      R2_finer[paste(time[j],"_",thres[k])] <- summary(lm(SOI[-1,2] ~ EMA(SOI[-l_prices,1],1)+EMA(SOI[-l_prices,1],2)))$adj.r.squared
      #R2_finer[paste(time[j],"_",thres[k])] <- summary(lm(SOI[,2] ~ SOI[,1]))$r.squared
  }
}

par(mfrow=c(1,3))
plot(SOI[,1],SOI[,2],xlab="SOI",ylab="return",main="SPY time bin concurrent (120-10000)")
plot(SOI1[,1],SOI1[,2],xlab="SOI",ylab="return",main="SPY time bin concurrent (30-10000)")
plot(SOI2[,1],SOI2[,2],xlab="SOI",ylab="return",main="SPY time bin concurrent (60-10000)")


sort(R2,decreasing = TRUE)[1:10]
write.csv(sort(R2,decreasing =TRUE)[1:10],file="Lee_Ready_Forecast_AMZN_R2.csv")

write.csv(sort(R2_finer,decreasing =TRUE)[1:10],file="AMZN predict using Richard method.csv")

lm_lag = lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-l_prices,1]*SOI_buckets_delta_prices[-l_prices,3])
data_predict = data.frame(SimpleR = SOI_buckets_delta_prices[-1,2],SOI = SOI_buckets_delta_prices[-l_prices,1],
                  BucketVol = SOI_buckets_delta_prices[-l_prices,3], 
                  SOIXBucketVol = SOI_buckets_delta_prices[-l_prices,1]*SOI_buckets_delta_prices[-l_prices,3])
scatterplotMatrix(~ SimpleR + SOI + BucketVol + SOIXBucketVol, data=data_predict, spread=FALSE,
                  lty.smooth=2, main="Scatter Plot Matrix with Bucket 2000, Exclude trades>4000, time bin 30s")
summary(lm_lag)
#Zoom-in
plot(SOI_buckets_delta_prices[,1]*
       SOI_buckets_delta_prices[,3],SOI_buckets_delta_prices[,2],xlim=c(-4e-07,4e-07),ylim=c(-0.0018,0.0020))
plot(SOI_buckets_delta_prices[,3],abs(SOI_buckets_delta_prices[,2]),xlim=c(0,4e-07),ylim=c(0,0.0025))

ind = which(data_predict$BucketVol > 1e-06)
data_predict_new <- data_predict[-ind,]
scatterplotMatrix(~ SimpleR + SOI + BucketVol + SOIXBucketVol, data=data_predict_new, spread=FALSE,
                  lty.smooth=2, main="Scatter Plot Matrix with Bucket 2000, Exclude trades>4000, time bin 30s")

# Please uncomment to test the desired OI used by VPINs (TR_VPIN, FB_VPIN, bulk-volume TR, bulk-volume FB)

# TR_VPIN
OI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades,bucket_size, F) 
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,trades[-which(trades$size>2000),],bucket_size, F, L, T)
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,a[-which(a$size>2000),],bucket_size, F, L, T, T, T, T)


l_trades = dim(trades)[1]
indd = which(a$type == "T")
ind = rep(0,3*l_trades)
for ( i in 1:l_trades){
  ind[(1+(i-1)*3):(3*i)] <- seq(indd[i]-1,indd[i]+1,1)
}
whatyouwant = a[ind,]
#Test SIO

lm1 <-lm(SOI_buckets_delta_prices[-c(1,2),2]~SOI_buckets_delta_prices[-c(l_prices,l_prices-1),1]+
           SOI_buckets_delta_prices[-c(1,l_prices),1])
plot_model_stat(SOI_buckets_delta_prices[,1],SOI_buckets_delta_prices[,2],"SOI","Bucket simple return","LeeReady Concurrent period with bucket size=10,000, time bin=135s, exclude trades with size > 1000")
summary(lm1)
pairs(data.frame(SOI_buckets_delta_prices[-c(1,2),2],SOI_buckets_delta_prices[-c(l_prices,l_prices-1),1],
                   SOI_buckets_delta_prices[-c(1,l_prices),1]))
names(summary(lm1))
#foreast
nnn = length(SOI_buckets_delta_prices[,2])
lm10 <-lm(SOI_buckets_delta_prices[-1,2]~SOI_buckets_delta_prices[-nnn,1])
summary(lm10)

lm5 <-lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]*SOI_buckets_delta_prices[,3])
plot(SOI_buckets_delta_prices[,1]*
      SOI_buckets_delta_prices[,3],SOI_buckets_delta_prices[,2])
abline(lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,3]),col="red")
summary(lm5)
residualPlots(lm5)

ind = which(SOI_buckets_delta_prices[,2]==max(SOI_buckets_delta_prices[,2]))
lm4 <-lm(SOI_buckets_delta_prices[-ind,2]~SOI_buckets_delta_prices[-ind,1]+SOI_buckets_delta_prices[-ind,1]*
           SOI_buckets_delta_prices[-ind,3])
summary(lm4)
plot(SOI_buckets_delta_prices[-ind,1]*SOI_buckets_delta_prices[-ind,3],SOI_buckets_delta_prices[-ind,2])
abline(lm(SOI_buckets_delta_prices[-ind,2]~SOI_buckets_delta_prices[-ind,3]),col="red")
residualPlots(lm4)
#0.09 R^2

plot(SOI_buckets_delta_prices[,1]*
       SOI_buckets_delta_prices[,3],SOI_buckets_delta_prices[,2],xlim=c(-5e-07,4e-07),ylim=c(-0.001,0.0013))

ind1 = which(SOI_buckets_delta_prices[,3]==0,arr.ind = T)
length(which(SOI_buckets_delta_prices[,3]==0,arr.ind = T))
thres = 0.3
ind2 = which(abs(SOI_buckets_delta_prices[,1])<thres,arr.ind = T)
ind3 = c(ind1,ind2)
ind3
xx = SOI_buckets_delta_prices[-ind3,1]*
SOI_buckets_delta_prices[-ind3,3]
yy = SOI_buckets_delta_prices[-ind3,2]
plot.new()
plot(xx,yy,xlim=c(-5e-06,5e-06),ylim=c(-0.003,0.003))

lm3 <-lm(SOI_buckets_delta_prices[1:2000,2]~SOI_buckets_delta_prices[1:2000,1])
plot_model_stat(SOI_buckets_delta_prices[1:2000,2],SOI_buckets_delta_prices[1:2000,1])
summary(lm3)
residualPlots(lm3)
#.1255 R^2
lm6 <-lm(SOI_buckets_delta_prices[1:2000,2]~SOI_buckets_delta_prices[1:2000,1]+SOI_buckets_delta_prices[1:2000,1]*SOI_buckets_delta_prices[1:2000,3])
summary(lm6)

########SIO prediction########
lm7 <-lm(SOI_buckets_delta_prices[2:2001,2]~SOI_buckets_delta_prices[1:2000,1]+SOI_buckets_delta_prices[1:2000,1]*SOI_buckets_delta_prices[1:2000,3])
summary(lm7)

lm8 <-lm(SOI_buckets_delta_prices[2:2001,2]~SOI_buckets_delta_prices[1:2000,1]+
           SOI_buckets_delta_prices[1:2000,1]*SOI_buckets_delta_prices[1:2000,3]+
           SOI_buckets_delta_prices[1:2000,2])
summary(lm8)

#
lm2 <- lm(SOI_buckets_delta_prices[which(SOI_buckets_delta_prices[,2]!=0,arr.ind = T),2]~
            SOI_buckets_delta_prices[which(SOI_buckets_delta_prices[,2]!=0,arr.ind = T),1])
summary(lm2)
residualPlots(lm2)



######### all the auotes and trades parsed
SOI_buckets_delta_prices <- calc_OI_by_time_buckets(time_bin,a,bucket_size, F, L, T)

SOI_buckets_delta_prices3 <- calc_OI_by_time_buckets(time_bin,a,bucket_size, F, L, T, T, T, T)

##############################[Volatility Test]#################################

# Regress against rolling realized standard deviation -> Mechanistic Significance
par(mfrow=c(1,3))
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
MA_volume <- calc_volat_by_volumes(trades, bucket_size, L, T)
plot_model_stat(TR_VPIN,MA_volume_delta,"volume_delta")
plot_model_stat(TR_VPIN, MA_volume,"volume")
price_volatilities <- OI_buckets_delta_prices[,3]
price_volatilities[is.na(price_volatilities)] <- 0
price_volatilities <- SMA(price_volatilities, L)[L:total_entry]
plot_model_stat(TR_VPIN, price_volatilities,"price vol")

#OFF-SET BY 1 peroid
par(mfrow=c(1,3))
MA_volume_delta <- calc_volat_by_volumes(trades, bucket_size, L)
MA_volume <- calc_volat_by_volumes(trades, bucket_size, L, T)
n0 = length(MA_volume)
plot_model_stat(TR_VPIN[-n0],MA_volume_delta[-1],"volume_delta","OFF-SET BY 1")
plot_model_stat(TR_VPIN, MA_volume,"volume","OFF-SET BY 1")
price_volatilities <- OI_buckets_delta_prices[,3]
price_volatilities[is.na(price_volatilities)] <- 0
price_volatilities <- SMA(price_volatilities, L)[L:total_entry]
plot_model_stat(TR_VPIN[-n0], price_volatilities[-1],"price vol","OFF-SET BY 1")
