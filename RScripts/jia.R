#install.packages("fBasics")
#install.packages("car")
#install.packages("aod")
library(aod)
library(fBasics)
library(car)

setwd("/Users/JiaXu/Documents/FE Project")
source("parser.R")

#Test SIO
lm1 <-lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1])
plot_model_stat(SOI_buckets_delta_prices[,2],SOI_buckets_delta_prices[,1])
summary(lm1)

lm5 <-lm(SOI_buckets_delta_prices[,2]~SOI_buckets_delta_prices[,1]+SOI_buckets_delta_prices[,1]*
           SOI_buckets_delta_prices[,3])
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
