setwd("~/Desktop/Data")
early_nonD2 = read.csv("early_spread_nonD2.csv",head = T)
midday_nonD2 = read.csv("midday_spread_nonD2.csv",head = T)
late_nonD2 = read.csv("late_spread_nonD2.csv",head = T)
early_D2 = read.csv("early_spread_D2.csv",head = T)
midday_D2 = read.csv("midday_spread_D2.csv",head = T)
late_D2 = read.csv("late_spread_D2.csv",head = T)

early_nonD3 = read.csv("early_spread_nonD3.csv",head = T)
midday_nonD3 = read.csv("midday_spread_nonD3.csv",head = T)
late_nonD3 = read.csv("late_spread_nonD3.csv",head = T)
early_D3 = read.csv("early_spread_D3.csv",head = T)
midday_D3 = read.csv("midday_spread_D3.csv",head = T)
late_D3 = read.csv("late_spread_D3.csv",head = T)

early_nonD4 = read.csv("early_spread_nonD4.csv",head = T)
midday_nonD4 = read.csv("midday_spread_nonD4.csv",head = T)
late_nonD4 = read.csv("late_spread_nonD4.csv",head = T)
early_D4 = read.csv("early_spread_D4.csv",head = T)
midday_D4 = read.csv("midday_spread_D4.csv",head = T)
late_D4 = read.csv("late_spread_D4.csv",head = T)

early_nonD5 = read.csv("early_spread_nonD5.csv",head = T)
midday_nonD5 = read.csv("midday_spread_nonD5.csv",head = T)
late_nonD5 = read.csv("late_spread_nonD5.csv",head = T)
early_D5 = read.csv("early_spread_D5.csv",head = T)
midday_D5 = read.csv("midday_spread_D5.csv",head = T)
late_D5 = read.csv("late_spread_D5.csv",head = T)

early_nonD6 = read.csv("early_spread_nonD6.csv",head = T)
midday_nonD6 = read.csv("midday_spread_nonD6.csv",head = T)
late_nonD6 = read.csv("late_spread_nonD6.csv",head = T)
early_D6 = read.csv("early_spread_D6.csv",head = T)
midday_D6 = read.csv("midday_spread_D6.csv",head = T)
late_D6 = read.csv("late_spread_D6.csv",head = T)

early1 = rbind(early_nonD2,early_D2)
midday1 = rbind(midday_nonD2,midday_D2)
late1 = rbind(late_nonD2,late_D2)
early1 = early1[order(early1[,2],early1[,3],decreasing = FALSE),]
midday1 = midday1[order(midday1[,2],midday1[,3],decreasing = FALSE),]
late1 = late1[order(late1[,2],late1[,3],decreasing = FALSE),]

early2 = rbind(early_nonD3,early_D3)
midday2 = rbind(midday_nonD3,midday_D3)
late2 = rbind(late_nonD3,late_D3)
early2 = early2[order(early2[,2],early2[,3],decreasing = FALSE),]
midday2 = midday2[order(midday2[,2],midday2[,3],decreasing = FALSE),]
late2 = late2[order(late2[,2],late2[,3],decreasing = FALSE),]

early3 = rbind(early_nonD4,early_D4)
midday3 = rbind(midday_nonD4,midday_D4)
late3 = rbind(late_nonD4,late_D4)
early3 = early3[order(early3[,2],early3[,3],decreasing = FALSE),]
midday3 = midday3[order(midday3[,2],midday3[,3],decreasing = FALSE),]
late3 = late3[order(late3[,2],late3[,3],decreasing = FALSE),]

early4 = rbind(early_nonD5,early_D5)
midday4 = rbind(midday_nonD5,midday_D5)
late4 = rbind(late_nonD5,late_D5)
early4 = early4[order(early4[,2],early4[,3],decreasing = FALSE),]
midday4 = midday4[order(midday4[,2],midday4[,3],decreasing = FALSE),]
late4 = late4[order(late4[,2],late4[,3],decreasing = FALSE),]

early5 = rbind(early_nonD6,early_D6)
midday5 = rbind(midday_nonD6,midday_D6)
late5 = rbind(late_nonD6,late_D6)
early5 = early5[order(early5[,2],early5[,3],decreasing = FALSE),]
midday5 = midday5[order(midday5[,2],midday5[,3],decreasing = FALSE),]
late5 = late5[order(late5[,2],late5[,3],decreasing = FALSE),]

##### Normalization #####
Data_Norm = function(data){
	tick = data[,2]
	trading_exchange = data[,3]
	pe = data[,9]
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
	data_new2 = data.frame(tick,trading_exchange,pe,data_new1)
	data_new = data_new2[-which(data_new2[,2] == "D1"),]
	return(data_new)
}

early_norm_1 = Data_Norm(early1)
midday_norm_1 = Data_Norm(midday1)
late_norm_1 = Data_Norm(late1)
early_norm_2 = Data_Norm(early2)
midday_norm_2 = Data_Norm(midday2)
late_norm_2 = Data_Norm(late2)
early_norm_3 = Data_Norm(early3)
midday_norm_3 = Data_Norm(midday3)
late_norm_3 = Data_Norm(late3)
early_norm_4 = Data_Norm(early4)
midday_norm_4 = Data_Norm(midday4)
late_norm_4 = Data_Norm(late4)
early_norm_5 = Data_Norm(early5)
midday_norm_5 = Data_Norm(midday5)
late_norm_5 = Data_Norm(late5)

early_es = early_norm_1[,3]
midday_es = midday_norm_1[-which(midday_norm_1[,3] >= 1),3]
late_es = late_norm_1[-which(late_norm_1[,3] >= 1),3]

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
	stocks = aggregate(data3[,4],by = list(data3[,1]),mean)[,1]
	meanprice = aggregate(x = data[,6], by = list(data[,1]), mean)
	totalvol = aggregate(x = data[,7], by = list(data[,1]), sum)
	meanprice_gb = meanprice[which(meanprice[,1] %in% stocks),2]
	totalvol_gb = totalvol[which(totalvol[,1] %in% stocks),2]
	
	# D2 and mean non-D2 spread
	spread_nonD2 = aggregate(data3[,4],by = list(data3[,1]),mean)[,2]
	spread_D2 = data2[which(data2[,2] == "D2"),4]
	spread_diff = (spread_nonD2 - spread_D2)/mean(spread_D2)
	
	# Good and bad stock classification
	good_bad = rep(0,length(spread_diff))		
	good = which(spread_diff > 0.01)
	good_bad[good] = 1
	
	# D2 fraction
	D2 = data[which(data[,2] == "D2"),]
	D2_frac = D2[which(D2[,1] %in% stocks),5]
	
	data_new1 = as.data.frame(cbind(spread_nonD2,spread_D2,spread_diff,good_bad,meanprice_gb,totalvol_gb,D2_frac))
	data_new2 = data.frame(stocks,data_new1)
	
	# Delete outliers
	data_new = data_new2[-which(abs(data_new2$spread_diff) >= 10),]
	return(data_new)
}

sp_early_1 = Spread_Comp_Data(early_norm_1)
sp_midday_1 = Spread_Comp_Data(midday_norm_1)
sp_late_1 = Spread_Comp_Data(late_norm_1)
sp_early_2 = Spread_Comp_Data(early_norm_2)
sp_midday_2 = Spread_Comp_Data(midday_norm_2)
sp_late_2 = Spread_Comp_Data(late_norm_2)
sp_early_3 = Spread_Comp_Data(early_norm_3)
sp_midday_3 = Spread_Comp_Data(midday_norm_3)
sp_late_3 = Spread_Comp_Data(late_norm_3)
sp_early_4 = Spread_Comp_Data(early_norm_4)
sp_midday_4 = Spread_Comp_Data(midday_norm_4)
sp_late_4 = Spread_Comp_Data(late_norm_4)
sp_early_5 = Spread_Comp_Data(early_norm_5)
sp_midday_5 = Spread_Comp_Data(midday_norm_5)
sp_late_5 = Spread_Comp_Data(late_norm_5)


##### D2 fraction - Richard's suggestion
mean_pos = vector()
mean_neg = vector()
wmean_pos = vector()
wmean_neg = vector()

mean_pos[1] = aggregate(sp_early_1[,8], by = list(sp_early_1[,5]), mean)[2,2]
mean_neg[1] = aggregate(sp_early_1[,8], by = list(sp_early_1[,5]), mean)[1,2]
mean_pos[2] = aggregate(sp_midday_1[,8], by = list(sp_midday_1[,5]), mean)[2,2]
mean_neg[2] = aggregate(sp_midday_1[,8], by = list(sp_midday_1[,5]), mean)[1,2]
mean_pos[3] = aggregate(sp_late_1[,8], by = list(sp_late_1[,5]), mean)[2,2]
mean_neg[3] = aggregate(sp_late_1[,8], by = list(sp_late_1[,5]), mean)[1,2]

mean_pos[4] = aggregate(sp_early_2[,8], by = list(sp_early_2[,5]), mean)[2,2]
mean_neg[4] = aggregate(sp_early_2[,8], by = list(sp_early_2[,5]), mean)[1,2]
mean_pos[5] = aggregate(sp_midday_2[,8], by = list(sp_midday_2[,5]), mean)[2,2]
mean_neg[5] = aggregate(sp_midday_2[,8], by = list(sp_midday_2[,5]), mean)[1,2]
mean_pos[6] = aggregate(sp_late_2[,8], by = list(sp_late_2[,5]), mean)[2,2]
mean_neg[6] = aggregate(sp_late_2[,8], by = list(sp_late_2[,5]), mean)[1,2]

mean_pos[7] = aggregate(sp_early_3[,8], by = list(sp_early_3[,5]), mean)[2,2]
mean_neg[7] = aggregate(sp_early_3[,8], by = list(sp_early_3[,5]), mean)[1,2]
mean_pos[8] = aggregate(sp_midday_3[,8], by = list(sp_midday_3[,5]), mean)[2,2]
mean_neg[8] = aggregate(sp_midday_3[,8], by = list(sp_midday_3[,5]), mean)[1,2]
mean_pos[9] = aggregate(sp_late_3[,8], by = list(sp_late_3[,5]), mean)[2,2]
mean_neg[9] = aggregate(sp_late_3[,8], by = list(sp_late_3[,5]), mean)[1,2]

mean_pos[10] = aggregate(sp_early_4[,8], by = list(sp_early_4[,5]), mean)[2,2]
mean_neg[10] = aggregate(sp_early_4[,8], by = list(sp_early_4[,5]), mean)[1,2]
mean_pos[11] = aggregate(sp_midday_4[,8], by = list(sp_midday_4[,5]), mean)[2,2]
mean_neg[11] = aggregate(sp_midday_4[,8], by = list(sp_midday_4[,5]), mean)[1,2]
mean_pos[12] = aggregate(sp_late_4[,8], by = list(sp_late_4[,5]), mean)[2,2]
mean_neg[12] = aggregate(sp_late_4[,8], by = list(sp_late_4[,5]), mean)[1,2]

mean_pos[13] = aggregate(sp_early_5[,8], by = list(sp_early_5[,5]), mean)[2,2]
mean_neg[13] = aggregate(sp_early_5[,8], by = list(sp_early_5[,5]), mean)[1,2]
mean_pos[14] = aggregate(sp_midday_5[,8], by = list(sp_midday_5[,5]), mean)[2,2]
mean_neg[14] = aggregate(sp_midday_5[,8], by = list(sp_midday_5[,5]), mean)[1,2]
mean_pos[15] = aggregate(sp_late_5[,8], by = list(sp_late_5[,5]), mean)[2,2]
mean_neg[15] = aggregate(sp_late_5[,8], by = list(sp_late_5[,5]), mean)[1,2]

plot(ts(mean_pos),ylim = c(0.2,0.8), ylab = "Mean trading volume fraction in D2", col = 1, main = "D2's mean trading volume fraction changes in 5 trading days")
lines(ts(mean_neg), col=2)
legend("topright",legend = c("positive spread difference","negative spread difference"), lty = 1, col = c(1,2))

library("data.table")
DT1 = data.table(sp_early_1)
DT2 = data.table(sp_midday_1)
DT3 = data.table(sp_late_1)
DT4 = data.table(sp_early_2)
DT5 = data.table(sp_midday_2)
DT6 = data.table(sp_late_2)
DT7 = data.table(sp_early_3)
DT8 = data.table(sp_midday_3)
DT9 = data.table(sp_late_3)
DT10 = data.table(sp_early_4)
DT11 = data.table(sp_midday_4)
DT12 = data.table(sp_late_4)
DT13 = data.table(sp_early_5)
DT14 = data.table(sp_midday_5)
DT15 = data.table(sp_late_5)
wmean_pos[1] = DT1[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[1] = DT1[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[2] = DT2[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[2] = DT2[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[3] = DT3[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[3] = DT3[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[4] = DT4[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[4] = DT4[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[5] = DT5[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[5] = DT5[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[6] = DT6[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[6] = DT6[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[7] = DT7[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[7] = DT7[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[8] = DT8[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[8] = DT8[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[9] = DT9[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[9] = DT9[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[10] = DT10[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[10] = DT10[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[11] = DT11[,list(wret =weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[11] = DT11[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[12] = DT12[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[12] = DT12[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[13] = DT13[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[13] = DT13[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[14] = DT14[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[14] = DT14[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]
wmean_pos[15] = DT15[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[2]
wmean_neg[15] = DT15[,list(wret = weighted.mean(D2_frac,totalvol_gb)),by=good_bad]$wret[1]

plot(ts(wmean_pos),ylim = c(0.2,0.8), ylab = "Weighted-mean trading volume fraction in D2", col = 1, main = "D2's weighted-mean trading volume fraction changes in 5 days")
lines(ts(wmean_neg), col=2)
legend("topright",legend = c("positive spread difference","negative spread difference"), lty = 1, col = c(1,2))


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

Spread_Comp_Plot(sp_midday,6)

##### Overall Correlation #####
Overall_Cor = function(data){
	cor_D1 = cor(data[,4],data[,5])
	return(cor_D1)
}

Overall_Cor(early_norm)
Overall_Cor(midday_norm)
Overall_Cor(late_norm)


##### Individual Correlation #####
Individual_Cor = function(data){
	
	# Select stocks traded at D2
	stock = data[which(data[,2] == "D2"),1]
	data1 = data[which(data[,1] %in% stock),]
	b = aggregate(x = data1[,5], by = list(data1[,1]), length)
	
	# Delete stocks only trade in 1 or 2 venue
	c = b[which(b[,2] %in% c(1,2)),1]
	data2 = data1[-which(data1[,1] %in% c),]
	
	# Delete stocks whose volume variance equals to 0
	d1 = aggregate(x = data2[,4], by = list(data2[,1]), sd)
	d2 = aggregate(x = data2[,5], by = list(data2[,1]), sd)
	e1 = d1[,2]*d2[,2]
	e = d1[which(e1 == 0),1]
	data3 = data2[-which(data2[,1] %in% e),]
	
	# Compute correlation
	f = aggregate(x = data3[,4], by = list(data3[,1]), length)
	no_exchange = f[,2]
	stocks = f[,1]
	len = dim(f)[1]

	cor = vector()
	c1 = 1
	c2 = 0
	
	for (i in 1:len){
		c2 = c2+f[i,2]
		cor[i] = cor(data3[c(c1:c2),4],data3[c(c1:c2),5])
		c1 = c1+f[i,2]
	}
	
	# Stocks of interest and their trading price and volume
	meanprice = aggregate(x = data[,6], by = list(data[,1]), mean)
	totalvol = aggregate(x = data[,7], by = list(data[,1]), sum)
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
	a = aggregate(data[,4], by = list(data[,1]), length)
	b = which(a[,2] %in% c(1:4))
	c = a[-b,1]
	data_new = data[which(data[,1] %in% c),]
	plot(data_new[,4], data_new[,5], xlab = "normalized effective spread", ylab = "normalized trading volume", xlim = c(0,0.03))
}

##### Regression #####

Regress_Data = function(data){
	
	tick = data[,2]
	trading_exchange = data[,3]
	pe = data[,9]
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
	data_new1 = as.data.frame(cbind(vol_perc,spread_perc,price,vol))
	data_new2 = data.frame(tick,trading_exchange,pe,data_new1)
	
	# Delete outliers
	data_new2 = data_new2[which(data_new2[,5] < 0.03),]
	
	# Traded in at least 4 venues
	f = aggregate(data_new2[,2],by = list(data_new2[,1]), length)
	stocks = f[which(f[,2] > 4),1]
	data_new2 = data_new2[which(data_new2[,1] %in% stocks),]
	
	len = dim(data_new2)[1]
	exchange = rep("Other",len)
	exchange[which(data_new2[,2] == "D1")] = "D1"
	exchange[which(data_new2[,2] == "D2")] = "D2"
	exchange[which(data_new2[,2] == "B")] = "BJY"
	exchange[which(data_new2[,2] == "J")] = "BJY"
	exchange[which(data_new2[,2] == "Y")] = "BJY"
	exchange[as.character(data_new2[,2]) == as.character(data_new2[,3])] = "PE"
	exchange = as.factor(exchange)	
	data_new3 = data.frame(data_new2[,-c(2,3)],exchange)
	DT = data.table(data_new3)
	DT1 = as.data.frame(DT[,list(wret = weighted.mean(spread_perc,vol)),by=list(tick,exchange)])
	DT2 = as.data.frame(DT[,list(wret = weighted.mean(price,vol)),by=list(tick,exchange)])
	DT3 = aggregate(data_new3[,2], by=list(data_new3[,6],data_new3[,1]),sum)
	DT4 = DT1[order(DT1[,1],DT1[,2]),]
	DT5 = DT2[order(DT1[,1],DT1[,2]),]
	data_new4 = data.frame(DT4,DT5[,3],DT3[,3])
	names(data_new4)[3] = paste("spread")
	names(data_new4)[4] = paste("price")
	names(data_new4)[5] = paste("vol")	
	return(data_new4)
}

early_reg1 = Regress_Data(early1)
midday_reg1 = Regress_Data(midday1)
late_reg1 = Regress_Data(late1)

Data2 = function(data1,data2,time){
	a = data2[which(data2[,5] == 0),1]
	b = data2[which(data2[,5] == 1),1]
	c = vector()
	c[which(data1[,1] %in% a)] = 0
	c[which(data1[,1] %in% b)] = 1
	data3 = data.frame(data1,c)
	names(data3)[6] = paste("spread_diff")
	data4 = data3[which(is.na(data3[,6]) == FALSE),]
	time = rep(time,dim(data4)[1])
	data5 = data.frame(data4,time)
	return(data5)
}

early_reg = Data2(early_reg1,sp_early_1,"early")
midday_reg = Data2(midday_reg1,sp_midday_1,"midday")
late_reg = Data2(late_reg1,sp_late_1,"late")
reg = rbind(early_reg,midday_reg,late_reg)

# Simple Linear Regression
data = reg
spread = data[,3]
price = data[,4]
spread_diff = as.factor(data[,6])
tv = as.factor(data[,2])
time = as.factor(data[,7])
vol = data[,5]
fit1 = lm(vol~tv*spread)
summary(fit1)

par(mfrow=c(2,3))
plot(reg[which(reg[,2] == "D1"),3],reg[which(reg[,2] == "D1"),5], xlab = "effective spread", ylab = "trading volume fraction", main = "D1")
plot(reg[which(reg[,2] == "D2"),3],reg[which(reg[,2] == "D2"),5], xlab = "effective spread", ylab = "trading volume fraction", main = "D2")
plot(reg[which(reg[,2] == "PE"),3],reg[which(reg[,2] == "PE"),5], xlab = "effective spread", ylab = "trading volume fraction", main = "Primary Exchange")
plot(reg[which(reg[,2] == "BJY"),3],reg[which(reg[,2] == "BJY"),5], xlab = "effective spread", ylab = "trading volume fraction", main = "BJY")
plot(reg[which(reg[,2] == "Other"),3],reg[which(reg[,2] == "Other"),5], xlab = "effective spread", ylab = "trading volume fraction", main = "Other")

