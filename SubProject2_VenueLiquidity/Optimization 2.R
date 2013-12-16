setwd("~/Desktop/SP500/")
library("data.table")

read_data = function(){
  for (i in 1:n){
    e1 <<- assign(paste("early_spread_nonD",i,sep=""),read.csv(paste("early_spread_nonD",i,".csv",sep="")))
    e2 <<- assign(paste("early_spread_D",i,sep=""),read.csv(paste("early_spread_D",i,".csv",sep="")))
    m1 <<- assign(paste("midday_spread_nonD",i,sep=""),read.csv(paste("midday_spread_nonD",i,".csv",sep="")))
    m2 <<- assign(paste("midday_spread_D",i,sep=""),read.csv(paste("midday_spread_D",i,".csv",sep="")))
    l1 <<- assign(paste("late_spread_nonD",i,sep=""),read.csv(paste("late_spread_nonD",i,".csv",sep="")))
    l2 <<- assign(paste("late_spread_D",i,sep=""),read.csv(paste("late_spread_D",i,".csv",sep="")))
    e = rbind(e1,e2)
    m = rbind(m1,m2)
    l = rbind(l1,l2)
    assign(paste("earlyA",i,sep=""), e[which(e[,9] == "A"),], envir = .GlobalEnv)
    assign(paste("middayA",i,sep=""), m[which(m[,9] == "A"),], envir = .GlobalEnv)
    assign(paste("lateA",i,sep=""), l[which(l[,9] == "A"),], envir = .GlobalEnv)
    assign(paste("earlyN",i,sep=""), e[which(e[,9] == "N"),], envir = .GlobalEnv)
    assign(paste("middayN",i,sep=""), m[which(m[,9] == "N"),], envir = .GlobalEnv)
    assign(paste("lateN",i,sep=""), l[which(l[,9] == "N"),], envir = .GlobalEnv)
    assign(paste("earlyP",i,sep=""), e[which(e[,9] == "P"),], envir = .GlobalEnv)
    assign(paste("middayP",i,sep=""), m[which(m[,9] == "P"),], envir = .GlobalEnv)
    assign(paste("lateP",i,sep=""), l[which(l[,9] == "P"),], envir = .GlobalEnv)
    assign(paste("earlyQ",i,sep=""), e[which(e[,9] == "Q"),], envir = .GlobalEnv)
    assign(paste("middayQ",i,sep=""), m[which(m[,9] == "Q"),], envir = .GlobalEnv)
    assign(paste("lateQ",i,sep=""), l[which(l[,9] == "Q"),], envir = .GlobalEnv)
    assign(paste("earlyZ",i,sep=""), e[which(e[,9] == "Z"),], envir = .GlobalEnv)
    assign(paste("middayZ",i,sep=""), m[which(m[,9] == "Z"),], envir = .GlobalEnv)
    assign(paste("lateZ",i,sep=""), l[which(l[,9] == "Z"),], envir = .GlobalEnv)
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
		data[,1] = as.factor(as.character(data[,1]))
		
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

# SP 500
SP500 = read.csv("SP500.csv",head = F)

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

n = 21
read_data()

earlyA = Data_Process("earlyA")
middayA = Data_Process("middayA")
lateA = Data_Process("lateA")
earlyN = Data_Process("earlyN")
middayN = Data_Process("middayN")
lateN = Data_Process("lateN")
earlyP = Data_Process("earlyP")
middayP = Data_Process("middayP")
lateP = Data_Process("lateP")
earlyQ = Data_Process("earlyQ")
middayQ = Data_Process("middayQ")
lateQ = Data_Process("lateQ")

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
	early = earlyP
	midday = middayP
	late = lateP
	earlysp500 = early[which(as.numeric(early[,12]) %in% c(1:21)[-i]),]
	middaysp500 = midday[which(as.numeric(midday[,12]) %in% c(1:21)[-i]),]
	latesp500 = late[which(as.numeric(late[,12]) %in% c(1:21)[-i]),]
	opt_early = Opt(earlysp500,runif(5))
	opt_early_r = round(opt_early,3)
	optimal_early_ratio = opt_early_r[1:5]
	mc_rate_early = opt_early_r[6]
	opt_midday = Opt(middaysp500,runif(5))
	opt_midday_r = round(opt_midday,3)
	optimal_midday_ratio = opt_midday_r[1:5]
	mc_rate_midday = opt_midday_r[6]
	opt_late = Opt(latesp500,runif(5))
	opt_late_r = round(opt_late,3)
	optimal_late_ratio = opt_late_r[1:5]
	mc_rate_late = opt_late_r[6]
	earlyi = early[which(as.numeric(early[,12]) == i),]
	middayi = midday[which(as.numeric(midday[,12]) == i),]
	latei = late[which(as.numeric(late[,12]) == i),]
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

## Using historical fraction
His_Frac1 = function(data){
	stock = data[which(data[,12] == 21),1]
	data = data[which(data[,1] %in% stock),]
	a = aggregate(data[,2], by = list(data[,1]), length)
	stock_int = a[which(a[,2] != 1),1]
	data = data[which(data[,1] %in% stock_int),]
	train = data[which(data[,1] %in% stock & data[,12] %in% c(1:20)),c(2:6)]
	guess = colMeans(train)
	test = data[which(data[,1] %in% stock & data[,12] == 21),c(2:6)]
	vol = data[which(data[,1] %in% stock & data[,12] == 21),13]
	mc = sum(rowSums(t(abs(t(test) - guess)))*vol)/(2*sum(vol))
	return(mc)
}

His_Frac1(middayP)

## Optimal by stock
Opt2 = function(data){
	stock = data[which(data[,12] == 21),1]
	data = data[which(data[,1] %in% stock),]
	a = aggregate(data[,2], by = list(data[,1]), length)
	stock_int = a[which(a[,2] != 1),1]
	data = data[which(data[,1] %in% stock_int),]
	train = data[which(data[,1] %in% stock & data[,12] %in% c(1:20)),c(1:6)]
	test = data[which(data[,1] %in% stock & data[,12] == 21),c(2:6)]
	guess = c(0.1,0.1,0.4,0.2,0.2)
	b = aggregate(data[,1],by = list(data[,1]),length)
	len = dim(b)[1]
	vol = data[which(data[,1] %in% stock & data[,12] == 21),13]
	result = matrix(nrow = len, ncol = 5)
	colnames(result) = c("BJY","D1","D2","O","PE")
	rownames(result) = a[1:len,1]
	for (i in 1:len){
		ticker = b[i,1]
		M_C = function(guess){
			data_an = train[which(as.character(train[,1]) == ticker),]
			mc = sum(rowSums(t(abs(t(data_an[,2:6]) - guess)))*vol)/(2*sum(vol))
			return(mc)
		}
		opt = optim(guess,M_C)
		result[i,c(1:5)] = opt$par
	}
	mc = sum(rowSums(abs(result-test))*vol)/(2*sum(vol))
	return(mc)
}

Opt2(earlyA)
Opt2(middayA)
Opt2(lateA)

## Using historical fraction
His_Frac2 = function(data){
	stock = data[which(data[,12] == 21),1]
	data = data[which(data[,1] %in% stock),]
	a = aggregate(data[,2], by = list(data[,1]), length)
	stock_int = a[which(a[,2] != 1),1]
	data = data[which(data[,1] %in% stock_int),]
	train = data[which(data[,1] %in% stock & data[,12] %in% c(1:20)),]
	test = data[which(data[,1] %in% stock & data[,12] == 21),c(2:6)]
	vol = data[which(data[,1] %in% stock & data[,12] == 21),13]
	t = matrix(ncol = 5, nrow = dim(test)[1])
	for (i in 1:5){
		t[,i] = aggregate(train[,i+1], by = list(train[,1]), mean)[,2]
	}
	mc = sum(rowSums(abs(t-test))*vol)/(2*sum(vol))
	return(mc)
}

His_Frac2(earlyA)
