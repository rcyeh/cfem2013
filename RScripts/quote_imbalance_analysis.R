source("parser.R")

first <- function (a){
  return (a[1])
}

last <- function (a){
  return (a[length(a)])
}

date <- '20130423'

a <- h5read(paste("ticks.",date,".h5",sep=""), paste("/ticks/C",sep=""), bit64conversion='double')
a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]

zero_ind <- which(a$ask==0 | a$bid==0)
a <- a[-zero_ind,]

start_ms <- a$exchange_time[1]

time_period <- 60 * 1000

groups <- as.matrix(floor((a$exchange_time-start_ms)/time_period)) + 1
quotes <- data.frame(cbind(a, groups))

bid_size_grp <- aggregate(quotes$bid_size, by=list(quotes$groups), FUN = sum)
ask_size_grp <- aggregate(quotes$ask_size, by=list(quotes$groups), FUN = sum)
qoi <- (bid_size_grp - ask_size_grp) / (bid_size_grp + ask_size_grp)$x

bgn_mid <- aggregate( (quotes$ask + quotes$bid)/2 , by=list(quotes$groups), FUN = first)
end_mid <- aggregate( (quotes$ask + quotes$bid)/2 , by=list(quotes$groups), FUN = last)
#m <- length(end_mid$x)
mid_return <- (end_mid - bgn_mid)/bgn_mid

#mid_return <- (end_mid$x[-1] - end_mid$x[-m])/end_mid$x[-m]
qoi_return <- cbind(qoi$x, mid_return$x)
#qoi_return <- cbind(qoi$x[-1], mid_return)

# One bucket ahead prediction
l <- length(qoi_return[,1])
summary(lm(qoi_return[-1,2]~qoi_return[-l,1]))
plot(qoi_return[-l,1],qoi_return[-1,2])

# Concurrent regression
plot(qoi_return[,1],qoi_return[,2])
summary(lm(qoi_return[,2]~qoi_return[,1]))

#ind <- which((is.nan(qoi_return[,1])) | (is.nan(qoi_return[,2])) | (is.infinite(qoi_return[,1])) | (is.infinite(qoi_return[,2])))
#qoi_return <- qoi_return[-ind,]
