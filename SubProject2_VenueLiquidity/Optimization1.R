setwd("~/Desktop/SP100/")
library("data.table")

read_data = function(){
  for (i in 1:n){
    e1 <<- assign(paste("early_spread_nonD",i,sep=""),read.csv(paste("early_spread_nonD",i,".csv",sep="")))
    e2 <<- assign(paste("early_spread_D",i,sep=""),read.csv(paste("early_spread_D",i,".csv",sep="")))
    m1 <<- assign(paste("midday_spread_nonD",i,sep=""),read.csv(paste("midday_spread_nonD",i,".csv",sep="")))
    m2 <<- assign(paste("midday_spread_D",i,sep=""),read.csv(paste("midday_spread_D",i,".csv",sep="")))
    l1 <<- assign(paste("late_spread_nonD",i,sep=""),read.csv(paste("late_spread_nonD",i,".csv",sep="")))
    l2 <<- assign(paste("late_spread_D",i,sep=""),read.csv(paste("late_spread_D",i,".csv",sep="")))
    assign(paste("early",i,sep=""),rbind(e1,e2), envir = .GlobalEnv)
    # assign data to a variable name
    assign(paste("midday",i,sep=""),rbind(m1,m2), envir = .GlobalEnv)
    assign(paste("late",i,sep=""),rbind(l1,l2), envir = .GlobalEnv)
  }
}

# Data Processing
Data_Process = function(time){
	
	data_final = matrix(ncol=12)	
	for (i in 1:n){

    print(i)
		assign(paste("data"),get(paste(time,i,sep=""))[,c(2,3,5,6,8,9)])

		# 5 trading venue categories
		len = dim(data)[1]
		ex = rep("O",len)
		ex[which(data$exchange == "B")] = "BJY"
		ex[which(data$exchange == "J")] = "BJY"
		ex[which(data$exchange == "Y")] = "BJY"
		ex[which(data$exchange == "D1")] = "D1"
		ex[which(data$exchange == "D2")] = "D2"
		ex[which(as.character(data$exchange) == as.character(data$primary.exchange))] = "PE"
		ex = as.factor(ex)
		data$exchange = ex

		# total trading volume
		vol = aggregate(data$size,by = list(data$symbol,data$exchange), sum)
		vol = vol[order(vol[,1],vol[,2]),]
		names(vol)[1:3] = c("stock","exchange","size")
		data_new = vol
	
		# normalize trading volume
		a = aggregate(x = data_new[,3], by = list(data_new[,1]),sum)
		b = aggregate(x = data_new[,3], by = list(data_new[,1]),length)
		len1 = dim(a)[1]
		d = vector()
		for (j in 1:len1){
			c = rep(a$x[j],b$x[j])
			d = c(d,c)
		}
		data_new$size = data_new$size/d
		table1 = xtabs(data_new$size ~ data_new$stock + data_new$exchange)
		
		# effective spread
		DT = data.table(data)
		DT1 = as.data.frame(DT[,list(wret = weighted.mean(effectiveSpread/price,size)),by=list(symbol,exchange)])
		DT1 = DT1[order(DT1[,1],DT1[,2]),]
		table2 = xtabs(DT1$wret ~ DT1$symbol + DT1$exchange)

		len2 = dim(table1)[1]
		table = cbind(table1,table2,rep(i,len2),a[,2])
		
		colnames(table)[11] = "day"
		colnames(table)[12] = "volume"
		data_final = rbind(data_final,table)
	}
	data_final = as.data.frame(data_final[-1,])
	tick = rownames(data_final)
	data_final = data.frame(tick,data_final)
	data_final = data_final[order(data_final[,1]),]
	data_final[,7] = as.factor(data_final[,7])
	
	return(data_final)
}

n = 21
read_data()

early = Data_Process("early")
midday = Data_Process("midday")
late = Data_Process("late")

# SP 100
SP100 = read.csv("SP100.csv",head = F)

# Computing Missclassification Rate
Opt = function(data, guess){
	a = aggregate(data[,1],by = list(data[,1]),length)
	result = vector()
	M_C = function(guess){
		mc = sum(rowSums(t(abs(t(data[,2:6]) - guess)))*data[,13])/(2*sum(data[,13]))
		return(mc)
	}
	opt_res = optim(guess,M_C)
    par = opt_res$par
	value = opt_res$value
  print(par)
  print(value)
	result = c(result, par)
	result = c(result, value)
		
	return(result)
}

MC_cal1 = function(time){
	guess = get(paste("optimal_",time,"_ratio",sep = ""))
	data = get(paste(time,"i",sep = ""))
	mc = sum(rowSums(t(abs(t(data[,2:6]) - guess)))*data[,13])/(2*sum(data[,13]))
	return(mc)
}

MC_cal2 = function(time,n){
	guess = get(paste("optimal_",time,"_ratio",sep = ""))
	data = get(paste(time,"i",sep = ""))
	data[,7] = as.numeric(as.character(data[,7])) 
	spread_adj = (rowMeans(data[,c(7,9,10,11)]) - data[,c(7,9,10,11)])*n
	len = dim(spread_adj)[1]
	guess_adj = cbind(spread_adj[,1],rep(0,len),spread_adj[,2:4]) + t(replicate(len, guess))
	mc = sum(rowSums(abs(guess_adj - data[,2:6]))*data[,13])/(2*sum(data[,13]))
	return(mc)
}

MC_comp = function(time){
	n = 2000
	num = 1:n - n/2
	mc1 = MC_cal1(time)
	mc2 = vector()
	for (i in 1:n){
		mc2[i] = MC_cal2(time,num[i])
	}
	mc_diff = mc2 - rep(mc1,n)
	# plot(num,mc_diff,xlab = "n",ylab = "spread-adjusted rate - rate", main = time)
	min_n = which(mc_diff == min(mc_diff))
	return(list(num[min_n],mc2[min_n]))
}

n_early = vector()
n_midday = vector()
n_late = vector()
mc_early_1 = vector()
mc_midday_1 = vector()
mc_late_1 = vector()
mc_early_2 = vector()
mc_midday_2 = vector()
mc_late_2 = vector()


for (i in 1:21){
	earlysp100 = early[which(early[,1] %in% SP100[,1] & as.numeric(early[,12]) %in% c(1:21)[-i]),]
	middaysp100 = midday[which(midday[,1] %in% SP100[,1] & as.numeric(midday[,12]) %in% c(1:21)[-i]),]
	latesp100 = late[which(late[,1] %in% SP100[,1] & as.numeric(late[,12]) %in% c(1:21)[-i]),]
	opt_early = Opt(earlysp100,runif(5))
	opt_early_r = round(opt_early,3)
	optimal_early_ratio = opt_early_r[1:5]
	mc_rate_early = opt_early_r[6]
	opt_midday = Opt(middaysp100,runif(5))
	opt_midday_r = round(opt_midday,3)
	optimal_midday_ratio = opt_midday_r[1:5]
	mc_rate_midday = opt_midday_r[6]
	opt_late = Opt(latesp100,runif(5))
	opt_late_r = round(opt_late,3)
	optimal_late_ratio = opt_late_r[1:5]
	mc_rate_late = opt_late_r[6]
	earlyi = early[which(early[,1] %in% SP100[,1] & as.numeric(early[,12]) == i),]
	middayi = midday[which(midday[,1] %in% SP100[,1] & as.numeric(midday[,12]) == i),]
	latei = late[which(late[,1] %in% SP100[,1] & as.numeric(late[,12]) == i),]
	mc_early = MC_comp("early")
	mc_midday = MC_comp("midday")
	mc_late = MC_comp("late")
	n_early[i] = mc_early[[1]]
	n_midday[i] = mc_midday[[1]]	
	n_late[i] = mc_late[[1]]
	mc_early_1[i] = MC_cal1("early")
	mc_midday_1[i] = MC_cal1("midday")	
	mc_late_1[i] = MC_cal1("late")
	mc_early_2[i] = mc_early[[2]]
	mc_midday_2[i] = mc_midday[[2]]	
	mc_late_2[i] = mc_late[[2]]	
}


round(mc_early_1,4)
round(mc_early_2,4)
round(mc_midday_1,4)
round(mc_midday_2,4)
round(mc_late_1,4)
round(mc_late_2,4)