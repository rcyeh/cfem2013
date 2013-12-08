plot_forward_vs_regress_bin <- function(bin = 0.2, 
                                       times <- c(180,120,90,60,30,10),
                                       date='20130423')
{
  cols <- c('tan','chartreuse1','chartreuse4','cornflowerblue','blue')
  #   cols <- rev(cols)
  #   times <- c(60,50,40,30,20,10)
  
  for (j in 1:length(times)){
    time <- times[j]
    file_name = paste('ExhaustiveAnalysis',date,'time',time,'_TRUETRUE.csv',sep='')
    sample <- read.csv(file=file_name,header=FALSE)
    names(sample) <-c('Symbol','QOI','SOI','Forward','UForward')
    N = round(2/bin)
    bin = 2/N
    bingroups <- aggregate(sample$Forward, by=list(floor(as.numeric(sample$SOI)/bin)), FUN = mean)
    print(j)
    if(j==1){
      plot(bingroups$Group.1*bin,bingroups$x,type='l',lwd = 2,col='yellow3',xlab ='SOI',ylab='return',main=paste('Return versus SOI on ',date,dep=''))
    }else{
      lines(bingroups$Group.1*bin,bingroups$x,type='l',lwd = 2,col=cols[j-1])
    }
  }
  legend('topleft',legend = paste(times,'s',sep='') ,col = c('yellow3',cols),lwd = 2,text.font=4, ncol=4)
}
