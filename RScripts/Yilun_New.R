setwd("~/Desktop")
early_nonD = read.csv("early_spread_nonD.csv",head = T)
midday_nonD = read.csv("midday_spread_nonD.csv",head = T)
late_nonD = read.csv("late_spread_nonD.csv",head = T)
early_D = read.csv("early_spread_D.csv",head = T)
midday_D = read.csv("midday_spread_D.csv",head = T)
late_D = read.csv("late_spread_D.csv",head = T)
early = rbind(early_nonD,early_D)
midday = rbind(midday_nonD,midday_D)
late = rbind(late_nonD,late_D)
early = early[order(early[,2],early[,3],decreasing = FALSE),]
midday = midday[order(midday[,2],midday[,3],decreasing = FALSE),]
late = late[order(late[,2],late[,3],decreasing = FALSE),]


##### Normalization #####
Data_Norm = function(data){
	tick = data[,2]
	trading_exchange = data[,3]
	spread_perc = data[,5]/data[,6]
	a = aggregate(x = data[,8], by = list(data[,2]),sum)
	b = aggregate(x = data[,8], by = list(data[,2]),length)
	len = dim(a)[1]
	d = vector()
	for (i in 1:len){
		c = rep(a$x[i],b$x[i])
		d = c(d,c)
	}
	vol_perc = data[,8]/d
	price = data[,6]
	vol = data[,8]
	data_new1 = as.data.frame(cbind(spread_perc,vol_perc,price,vol))
	data_new2 = data.frame(tick,trading_exchange,data_new1)
	data_new = data_new2[-which(data_new2[,2] == "D1"),]
	return(data_new)
}

early_norm = Data_Norm(early)
midday_norm = Data_Norm(midday)
late_norm = Data_Norm(late)

early_es = early_norm[,3]
midday_es = midday_norm[-which(midday_norm[,3] >= 1),3]
late_es = late_norm[-which(late_norm[,3] >= 1),3]


mean(early_es)
mean(midday_es)
mean(late_es)
median(early_es)
median(midday_es)
median(late_es)


##### Spread Comparison #####
Spread_Comp_Data = function(data){
	
	# Select stocks traded at D2
	stock = data[which(data[,2] == "D2"),1]
	data1 = data[which(data[,1] %in% stock),]
	b = aggregate(x = data1[,4], by = list(data1[,1]), length)
	
	# Delete stocks only trade in 1 venue
	c = b[which(b[,2] == 1),1]
	data2 = data1[-which(data1[,1] %in% c),]
	
	# Compute mean spread for non_D1 venues
	data3 = data2[-which(data2[,2] == "D2"),]
	
	# Stocks of interest and their trading price and volume
	stocks = aggregate(data3[,3],by = list(data3[,1]),mean)[,1]
	meanprice = aggregate(x = data[,5], by = list(data[,1]), mean)
	totalvol = aggregate(x = data[,6], by = list(data[,1]), sum)
	meanprice_gb = meanprice[which(meanprice[,1] %in% stocks),2]
	totalvol_gb = totalvol[which(totalvol[,1] %in% stocks),2]
	
	# D2 and mean non-D2 spread
	spread_nonD2 = aggregate(data3[,3],by = list(data3[,1]),mean)[,2]
	spread_D2 = data2[which(data2[,2] == "D2"),3]
	spread_diff = (spread_nonD2 - spread_D2)/mean(spread_D2)
	
	# Good and bad stock classification
	good_bad = rep(0,length(spread_diff))		
	good = which(spread_diff > 0.01)
	good_bad[good] = 1
	
	# D2 fraction
	D2 = data[which(data[,2] == "D2"),]
	D2_frac = D2[which(D2[,1] %in% stocks),4]
	
	data_new1 = as.data.frame(cbind(spread_nonD2,spread_D2,spread_diff,good_bad,meanprice_gb,totalvol_gb,D2_frac))
	data_new2 = data.frame(stocks,data_new1)
	
	# Delete outliers
	data_new = data_new2[-which(abs(data_new2$spread_diff) >= 10),]
	return(data_new)
}

sp_early = Spread_Comp_Data(early_norm)
sp_midday = Spread_Comp_Data(midday_norm)
sp_late = Spread_Comp_Data(late_norm)

##### D2 fraction - Richard's suggestion
aggregate(sp_early[,8], by = list(sp_early[,5]), mean)
aggregate(sp_midday[,8], by = list(sp_midday[,5]), mean)
aggregate(sp_late[,8], by = list(sp_late[,5]), mean)
library("data.table")
DT1 = data.table(sp_early)
DT2 = data.table(sp_midday)
DT3 = data.table(sp_late)
DT1[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]
DT2[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]
DT3[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]


Spread_Comp_Plot = function(data,n){
	quant = quantile(data[,6],probs = seq(0,1,1/n))
	a = vector()
	for (i in 1:n){
		b = which(data[,6] >= quant[i] & data[,6] < quant[i+1])
		data1 = data[b,]
		a[i] = sum(data1[,5])/length(data1[,5])
	}
	plot(ts(a),xlab = "Group", ylab = "Percentage of stocks with negative spread difference")
	return(a)
}

Spread_Comp_Plot(sp_late,6)

##### Overall Correlation #####
Overall_Cor = function(data){
	cor_D1 = cor(data[,3],data[,4])
	data_new = data[-which(data[,2] == "D1"),]
	cor_nonD1 = cor(data_new[,3],data_new[,4])
	return(list(cor_D1,cor_nonD1))
}

Overall_Cor(early_norm)
Overall_Cor(midday_norm)
Overall_Cor(late_norm)


##### Individual Correlation #####
Individual_Cor = function(data){
	
	# Select stocks traded at D2
	stock = data[which(data[,2] == "D2"),1]
	data1 = data[which(data[,1] %in% stock),]
	b = aggregate(x = data1[,4], by = list(data1[,1]), length)
	
	# Delete stocks only trade in 1 or 2 venue
	c = b[which(b[,2] %in% c(1,2)),1]
	data2 = data1[-which(data1[,1] %in% c),]
	
	# Delete stocks whose volume variance equals to 0
	d1 = aggregate(x = data2[,3], by = list(data2[,1]), sd)
	d2 = aggregate(x = data2[,4], by = list(data2[,1]), sd)
	e1 = d1[,2]*d2[,2]
	e = d1[which(e1 == 0),1]
	data3 = data2[-which(data2[,1] %in% e),]
	
	# Compute correlation
	f = aggregate(x = data3[,3], by = list(data3[,1]), length)
	no_exchange = f[,2]
	stocks = f[,1]
	len = dim(f)[1]

	cor = vector()
	c1 = 1
	c2 = 0
	
	for (i in 1:len){
		c2 = c2+f[i,2]
		cor[i] = cor(data3[c(c1:c2),3],data3[c(c1:c2),4])
		c1 = c1+f[i,2]
	}
	
	# Stocks of interest and their trading price and volume
	meanprice = aggregate(x = data[,5], by = list(data[,1]), mean)
	totalvol = aggregate(x = data[,6], by = list(data[,1]), sum)
	meanprice_pn = meanprice[which(meanprice[,1] %in% stocks),2]
	totalvol_pn = totalvol[which(totalvol[,1] %in% stocks),2]
	
	# Good and bad stock classification
	pn = rep(0,length(stocks))		
	pos = which(cor > 0.4)
	neg = which(cor < -0.4)
	pn[pos] = 1
	pn[neg] = -1

	data_new1 = as.data.frame(cbind(no_exchange,cor,pn,meanprice_pn,totalvol_pn))
	data_new = data.frame(stocks,data_new1)	
	
	return(data_new)
}

cor_early = Individual_Cor(early_norm)
cor_midday = Individual_Cor(midday_norm)
cor_late = Individual_Cor(late_norm)

Cor_Plot = function(data,n){
	data = cor_late
	n = 10
	quant = quantile(data[,5],probs = seq(0,1,1/n))
	pos = vector()
	neg = vector()
	for (i in 1:n){
		b = which(data[,5] >= quant[i] & data[,5] < quant[i+1])
		data1 = data[b,]
		pos[i] = dim(data[which(data1[,4] == 1),])[1]
		neg[i] = dim(data[which(data1[,4] == -1),])[1]
	}
	plot(ts(neg))
	return(a)
}

data = sp_late
mean(data[which(data[,5] == 1),6])
median(data[which(data[,5] == 1),6])
mean(data[which(data[,5] == 1),7])
median(data[which(data[,5] == 1),7])
mean(data[which(data[,5] == 0),6])
median(data[which(data[,5] == 0),6])
mean(data[which(data[,5] == 0),7])
median(data[which(data[,5] == 0),7])

##### Scatter Plot #####
Scatter_Plot = function(data){
	a = aggregate(data[,3], by = list(data[,1]), length)
	b = which(a[,2] %in% c(1:4))
	c = a[-b,1]
	data_new = data[which(data[,1] %in% c),]
	plot(data_new[,3], data_new[,4], xlab = "normalized effective spread", ylab = "normalized trading volume", xlim = c(0,0.1))
}

