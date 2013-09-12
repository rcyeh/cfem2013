symbol = 'AAPL'
a <- h5read("ticks.20130424.h5", paste("/ticks/",symbol,sep='') , bit64conversion='double')
quotes <- a[a$type == 'Q',unlist(strsplit("time|latency|symbol|refresh|bid_exchange|ask_exchange|exchange_time|bid_size|bid|ask|ask_size|quals|seq_no|instrument_status|prev_close", "\\|"))]
trades <- a[a$type == 'T',unlist(strsplit("time|latency|symbol|exchange|exchange_time|seq_no|price|size|volume|quals|market_status|instrument_status|thru_exempt|sub_market|line", "\\|"))]
trades$time <- as.POSIXct(paste('23/04/2013',substr(as.character(trades$time),1,11)), format = "%d/%m/%Y %H:%M:%S") 

#break into three group 9:40 and 15:50, NYC time
# or convert into UTC 13:40 and UTC 19:50 
mid_day_start <- as.POSIXct('23/04/2013 13:40:00', format = "%d/%m/%Y %H:%M:%S")
mid_day_end <- as.POSIXct('23/04/2013 19:50:00', format = "%d/%m/%Y %H:%M:%S")
early <- as.POSIXct('23/04/2013 12:10:00', format = "%d/%m/%Y %H:%M:%S")
late <- as.POSIXct('24/04/2013 21:05:00', format = "%d/%m/%Y %H:%M:%S")
trades$timegrp[trades$time >= late ] = 'trade_startend'
trades$timegrp[mid_day_end <= trades$time & trades$time < late] = 'afternoon'
trades$timegrp[mid_day_start <= trades$time & trades$time < mid_day_end] = 'midday'
trades$timegrp[early <= trades$time & trades$time < mid_day_start] = 'morning'
trades$timegrp[trades$time < early ] = 'trade_startend'

#Make a plot of exchange vs timegrp vs (size of trades)
dev.new()
ggplot() +
  geom_point(aes(x = timegrp,y = exchange,size = size),data=trades) +
  scale_area(guide = guide_legend())


names(trades)
head(trades)

# Number of trades only
loc=trades$exchange
time=trades$timegrp
table=table(loc,time)
chisq=chisq.test(table)$statistic[[1]]
class(chisq)
nobs=length(loc)
ncols=ncol(table)
nrows=nrow(table)
v=sqrt(chisq/(nobs*min(ncols,nrows)-1)) # Cramer's V
v

# 
sizes=trades$size
table1=table(sizes,time,loc)
chisq2=chisq.test(table2)$statistic[[1]]
class(chisq2)
nobs=length(loc)
ncols=ncol(table)
nrows=nrow(table)
v=sqrt(chisq2/(nobs*min(ncols,nrows)-1)) # Cramer's V
v
