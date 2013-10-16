library(aod)
library(fBasics)
library(car)

setwd("/Users/JiaXu/Documents/FE project 2013/RScripts")
source("parser.R")
setwd("/Users/JiaXu/Documents/FE project")
date = "20130430"
tick = "AGN"
#  "AGN"
#TIF ,"BA", 
a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/",tick,sep=""), bit64conversion='double')

delays <- c(30,100, 300, 1000, 3000)
  ##seq(1,30,1)  c(seq(0,.5,.005), seq(1,10,1)) 
l_delays <- length(delays)
thres_h = 1000
r2s <- c()
for(i in 1:l_delays){
  delay = delays[i]
  delayed_a <- delay_quotes_xms(a,delay)
  trades_quotes <- filter_trades_quotes3(delayed_a, thres_h)
  SOI_buckets_delta_prices <- calc_SOI(60, trades_quotes,F)
#exclude out of 2 standard deviation points from SOI, Preturn and Volatility
  means <- c()
  stdev2 <- c()
  for(j in 1:3){
    means[j] = mean(SOI_buckets_delta_prices[,j])
    stdev2[j] = 2*sd(SOI_buckets_delta_prices[,j])
  }
  exind = which((abs(SOI_buckets_delta_prices[,1]-means[1])> stdev2[1]) | (abs(SOI_buckets_delta_prices[,2]-means[2]) > stdev2[2])) #| (abs(SOI_buckets_delta_prices[,3]-means[3])>stdev2[3]))
  #cross = SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3])
  rdata = data.frame(PReturn = SOI_buckets_delta_prices[-exind,2], SOI = SOI_buckets_delta_prices[-exind,1])
                     #cross = cross[-exind])
  r2s[i] = summary(lm(PReturn ~ SOI,data = rdata))$adj.r.squared
}

plot(delays,r2s,type="l",main= paste(date," ", tick))
