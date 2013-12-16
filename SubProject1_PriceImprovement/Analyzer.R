plot_forward_vs_regress_bin <- function(bin = 0.2, 
                                       times = c(180,120,90,60,30,10),
                                       date='20130423',
                                       xaxis='SOI',
                                       xnumCol=2,
                                       ynumCol=6)
{
  cols <- rev(c('yellow','tan','chartreuse1','chartreuse4','cornflowerblue','blue','red','purple'))
  #   cols <- rev(cols)
  #   times <- c(60,50,40,30,20,10)
  
  for (j in 1:length(times)){
    time <- times[j]
    file_name = paste('ExhaustiveAnalysis',date,'time',time,'_TRUETRUE.csv',sep='')
    sample <- read.csv(file=file_name,header=FALSE)
    names(sample) <-c('Symbol','QOI','SOI','UForward','Forward')
    N = round(2/bin)
    bin = 2/N
    #bingroups <- aggregate(sample$UForward, by=list(floor(as.numeric(sample$SOI)/bin)), FUN = mean)
    bingroups <- aggregate(sample[,ynumCol], by=list(floor(as.numeric(sample[,xnumCol])/bin)), FUN = mean)
    print(j)
    if(j==1){
      plot(bingroups$Group.1*bin,bingroups$x,type='l',lwd = 2,col='yellow3',xlab =xaxis,ylab='Return',main=paste('Return versus ',xaxis,' on',date,dep=''))
    }else{
      lines(bingroups$Group.1*bin,bingroups$x,type='l',lwd = 2,col=cols[j-1])
    }
  }
  legend('bottomleft',legend = paste(times,'s',sep='') ,col = c('yellow3',cols),lwd = 2,text.font=4, ncol=4)
}

#plot_forward_vs_regress_bin(xaxis='Signed Quote Imbalance', times = c(120,90,60,30,20,10,2,0.01), ynumCol=5)

#plot_forward_vs_regress_bin(xaxis='Signed Order Imbalance (varying time interval)', times = c(20,10,2,0.5,0.1, 0.01), xnumCol=3, ynumCol=5, bin=0.1)
plot_forward_vs_regress_bin(xaxis='Signed Order Imbalance (varying time interval)', times = c(240,180,150,120,60,30), xnumCol=3, ynumCol=5, bin=0.05)