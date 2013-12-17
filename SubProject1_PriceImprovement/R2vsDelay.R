library(aod)
library(fBasics)
library(car)
library("ggplot2")

source("calc_SOI.R")

date = "20130423"
tick = "AMZN"

a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/",tick,sep=""), bit64conversion='double')

delays <- c(seq(0,.5,.005), seq(1,10,1),c(1,5,10,30,100, 300, 1000, 3000))
l_delays <- length(delays)
thres_h = 1000
r2s <- c()
for(i in 1:l_delays){
  delay = delays[i]
  delayed_a <- delay_quotes_xms(a,delay)
  trades_quotes <- filter_trades_quotes(delayed_a, thres_h)
  SOI_buckets_delta_prices1 <- calc_SOI(60, trades_quotes,F)
  SOI_buckets_delta_prices <- SOI_buckets_delta_prices1[-c(1:5),]
  #exclude out of 2 standard deviation points from SOI, Preturn and Volatility
  means <- c()
  stdev2 <- c()
  for(j in 1:3){
    means[j] = mean(SOI_buckets_delta_prices[,j])
    stdev2[j] = 2*sd(SOI_buckets_delta_prices[,j])
  }
  ind = which((abs(SOI_buckets_delta_prices[,1]-means[1])<= stdev2[1]) & (abs(SOI_buckets_delta_prices[,2]-means[2]) <= stdev2[2])) #| (abs(SOI_buckets_delta_prices[,3]-means[3])>stdev2[3]))
  
  rdata = data.frame(PReturn = SOI_buckets_delta_prices[ind,2], SOI = SOI_buckets_delta_prices[ind,1])
                     #cross = cross[ind])
  r2s[i] = summary(lm(PReturn ~ SOI,data = rdata))$adj.r.squared
}

ll = length(seq(0,.5,.005)) 
par(mfrow=c(1,2))
qplot(delays,r2s,geom=c("line"),color="cyl",main= paste("Current regression R^2 against quotes delay time\n",tick," on ",date,sep=""),ylab="R^2",xlab="delay (second)")
idd = which(delays[1:ll] == max(delays[1:ll]))
qplot(delays[1:ll]*1000,r2s[1:ll],geom=c("line"),color="cyl",
      main= paste("Zoom in: 1-500 millisecond part\n",tick," on ",date,sep=""),
      ylab="R^2",xlab="delay (millisecond)")
