library(tidyverse)
stock = read_tsv("Stock202.txt")
stockNBBO = read_tsv("Stock202BBO.txt")

#coef_003 = matrix(nrow = 22, ncol = 5)
#desc_hor_003 = matrix(nrow = 22, ncol = 5)
#desc_vol_003 = matrix(nrow = 22, ncol = 4)
#coef_201 = matrix(nrow = 22, ncol = 5)
#desc_hor_201 = matrix(nrow = 22, ncol = 5)
#desc_vol_201 = matrix(nrow = 22, ncol = 4)
#coef_202 = matrix(nrow = 22, ncol = 5)
#desc_hor_202 = matrix(nrow = 22, ncol = 5)
#desc_vol_202 = matrix(nrow = 22, ncol = 4)

stock_tot = inner_join(stockNBBO, stock[,c(1,2,5)], by=c("day","time"))


for(i in 2:nrow(stock_tot)){
  stock_tot[i,6] = (stock_tot$bid[i] + stock_tot$ask[i]) / 2
  
  if(stock_tot$buysell[i] == 1){
    if(stock_tot$buysell[i-1] == 1){
      stock_tot[i,7] = (stock_tot$ask[i] - stock_tot[i,6]) - (stock_tot$ask[i-1] - stock_tot[i,6])
    }else{stock_tot[i,7] = (stock_tot$ask[i] - stock_tot[i,6]) + (stock_tot[i,6] - stock_tot$bid[i-1])}
  }else{
    if(stock_tot$buysell[i-1] == 1){
      stock_tot[i,7] = -(stock_tot[i,6] - stock_tot$bid[i]) - (stock_tot$ask[i-1] - stock_tot[i,6])
    }else{stock_tot[i,7] = -(stock_tot[i,6] - stock_tot$bid[i]) + (stock_tot[i,6] - stock_tot$bid[i-1])}
  }
  
  print(i)
}

colnames(stock_tot) = c("day", "time", "bid", "ask", "buysell", "q", "dif_precio")

for(i in unique(stock_tot$day)){
  stock_tot[which(stock_tot$day == i),][1,c(6,7)] = NA
  dif_P = stock_tot[which(stock_tot$day == i),]$dif_precio[-1]
  Xt = stock_tot[which(stock_tot$day == i),]$buysell[-1]
  `Xt-1` = stock_tot[which(stock_tot$day == i),]$buysell[-nrow(stock_tot[which(stock_tot$day == i),])]
  assign(paste("lm", i, sep = ""), lm(dif_P ~ Xt + `Xt-1`))
}

for(i in 1:length(unique(stock_tot$day))){
  coef_202[i,] = c(unique(stock_tot$day)[i], get(paste("lm",unique(stock_tot$day)[i],sep=""))[[1]], 
                   sigma(get(paste("lm",unique(stock_tot$day)[i],sep="")))^2)
}

colnames(coef_202) = c("day", "intercept", "Xt", "Xt-1", "RSE^2")

for(i in 1:length(unique(stock_tot$day))){
  desc_hor_202[i,] = c(-coef_202[i,4], coef_202[i,3] + coef_202[i,4], coef_202[i,5], 
                       (coef_202[i,3] + coef_202[i,4])/coef_202[i,3], -coef_202[i,4]/coef_202[i,3])
}

colnames(desc_hor_202) = c("gamma", "alpha", "RSE^2", "SA", "CO")

for(i in 1:length(unique(stock_tot$day))){
  desc_vol_202[i,c(1,2)] = c((desc_hor_202[i,2])^2 / ((desc_hor_202[i,2])^2 + desc_hor_202[i,3]), 
                       (2*desc_hor_202[i,1]*(desc_hor_202[i,2] + desc_hor_202[i,1]) + desc_hor_202[i,2]^2 + desc_hor_202[i,3]))
  desc_vol_202[i,c(3,4)] = c((2*desc_hor_202[i,1]*(desc_hor_202[i,2] + desc_hor_202[i,1]) / desc_vol_202[i,2]), 
                             desc_hor_202[i,3] / desc_vol_202[i,2])
}

colnames(desc_vol_202) = c("SA_PE", "var_dif_PO", "NI_PO", "IP_PO")
stock_tot_202 = stock_tot
stock_tot$q5 = NA
stock_tot$q30 = NA
stock_tot$q60 = NA
for(i in 1:nrow(stock_tot)){
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 5)),]$day) != 0){
    stock_tot$q5[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 5)),]$bid[1] +
                        stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 5)),]$ask[1]) / 2) 
  }else{next}
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time == (stock_tot$time[i] + 30)),]$day) != 0){
    stock_tot$q30[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 30)),]$bid[1] +
                         stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 30)),]$ask[1]) / 2) 
  }else{next}
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time == (stock_tot$time[i] + 60)),]$day) != 0){
    stock_tot$q60[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 60)),]$bid[1] +
                         stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time <= (stock_tot$time[i] + 60)),]$ask[1-]) / 2) 
  }else{next}
}
sum(!is.na(stock_tot[,c(8,9,10)]))
