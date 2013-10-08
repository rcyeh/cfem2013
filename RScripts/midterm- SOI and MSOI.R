library(aod)
library(fBasics)
library(car)

setwd("/Users/JiaXu/Documents/FE project 2013/RScripts")
source("parser.R")
setwd("/Users/JiaXu/Documents/FE project")

#### analysis AMZN with SOI and MSOI ####
a <- h5read("ticks.20130423.h5", "/ticks/AMZN", bit64conversion='double')
a <- a[order(a$exchange_time),]
#quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
#trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line|type", "\\|"))]
trades_quotes <- filter_trades_quotes3(a,1000)
SOI_buckets_delta_prices <- calc_SOI(60,trades_quotes) 
crossterm <- SOI_buckets_delta_prices[,1]*sqrt(SOI_buckets_delta_prices[,3])
lm10 <- lm(SOI_buckets_delta_prices[,2] ~ SOI_buckets_delta_prices[,1])
summary(lm10)
plot(SOI_buckets_delta_prices[,1],SOI_buckets_delta_prices[,2],
     main="AMZN 60s time bucket concurrent",
     xlab= expression(paste("                SOI\nPRetrun(k) =",alpha,"+",beta, "*SOI(k)+",epsilon,"(k), R^2 = 27%",sep="")),
     ylab=expression("PRetrun"))
abline(lm10, col="red")

lm11 <- lm(SOI_buckets_delta_prices[,2] ~ crossterm)
summary(lm11)
plot(crossterm,SOI_buckets_delta_prices[,2],
     main="AMZN 60s time bucket concurrent",
     xlab= expression(paste("                MSOI\nPRetrun(k) =",alpha,"+",beta, "*MSOI(k)+",epsilon,"(k), R^2 = 62%",sep="")),
     ylab=expression("PRetrun"))
abline(lm11, col="red")

##### Out of sample test #####
model1 <- buildmodel("AMZN","20130423",60,1000,T)
outtest(model1,"AMZN","20130429",60,1000)
#CDPR: correct direction prediction Ratio



