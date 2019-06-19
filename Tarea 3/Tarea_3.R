#Se cargan las librerías necesarias.
library(tidyverse)

#Se leen los datos y almacenan en dataframes.
stock = read_tsv("Stock102.txt")
stockNBBO = read_tsv("Stock102NBBO.txt")

#Se crean las variables que serán necesarias en el algoritmo
#coef_101 = matrix(nrow = 22, ncol = 5)
#desc_hor_101 = matrix(nrow = 22, ncol = 5)
#desc_vol_101 = matrix(nrow = 22, ncol = 4)
coef_102 = matrix(nrow = 22, ncol = 5)
desc_hor_102 = matrix(nrow = 22, ncol = 5)
desc_vol_102 = matrix(nrow = 22, ncol = 4)
#coef_201 = matrix(nrow = 22, ncol = 5)
#desc_hor_201 = matrix(nrow = 22, ncol = 5)
#desc_vol_201 = matrix(nrow = 22, ncol = 4)
#coef_202 = matrix(nrow = 22, ncol = 5)
#desc_hor_202 = matrix(nrow = 22, ncol = 5)
#desc_vol_202 = matrix(nrow = 22, ncol = 4)
#coef_203 = matrix(nrow = 22, ncol = 5)
#desc_hor_203 = matrix(nrow = 22, ncol = 5)
#desc_vol_203 = matrix(nrow = 22, ncol = 4)


coef = matrix(nrow = 22, ncol = 5)
desc_hor = matrix(nrow = 22, ncol = 5)
desc_vol = matrix(nrow = 22, ncol = 4)
stock_tot = inner_join(stockNBBO, stock[,c(1,2,5)], by=c("day","time"))


for(i in 2:nrow(stock_tot)){
  #Se calcula el punto medio
  stock_tot[i,6] = (stock_tot$bid[i] + stock_tot$ask[i]) / 2
  
  #Se calcula el cambio de precios teniendo en cuenta todas las posibilidades.
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
#Se asignan nombres a las nuevas columnas.
colnames(stock_tot) = c("day", "time", "bid", "ask", "buysell", "q", "dif_precio")

for(i in unique(stock_tot$day)){
  #La primera observación del día no tiene sentido como se ha calculado, ya que se utiliza el último registro del día anterior,
  #por lo tanto se rellena con NA
  stock_tot[which(stock_tot$day == i),][1,c(6,7)] = NA
  #se realiza la regresión para calcular las betas.
  dif_P = stock_tot[which(stock_tot$day == i),]$dif_precio[-1]
  Xt = stock_tot[which(stock_tot$day == i),]$buysell[-1]
  `Xt-1` = stock_tot[which(stock_tot$day == i),]$buysell[-nrow(stock_tot[which(stock_tot$day == i),])]
  assign(paste("lm", i, sep = ""), lm(dif_P ~ Xt + `Xt-1`))
}

for(i in 1:length(unique(stock_tot$day))){
  #Se almacenan los coeficientes en una nueva variable.
  coef[i,] = c(unique(stock_tot$day)[i], get(paste("lm",unique(stock_tot$day)[i],sep=""))[[1]], 
                   sigma(get(paste("lm",unique(stock_tot$day)[i],sep="")))^2)
}



for(i in 1:length(unique(stock_tot$day))){
  #Se calcula la descomposición de la horquilla.
  desc_hor[i,] = c(-coef[i,4], coef[i,3] + coef[i,4], coef[i,5], 
                       (coef[i,3] + coef[i,4])/coef[i,3], -coef[i,4]/coef[i,3])
}



for(i in 1:length(unique(stock_tot$day))){
  #Se calcula la descomposición de la volatilidad del precio eficiente.
  desc_vol[i,c(1,2)] = c((desc_hor[i,2])^2 / ((desc_hor[i,2])^2 + desc_hor[i,3]), 
                       (2*desc_hor[i,1]*(desc_hor[i,2] + desc_hor[i,1]) + desc_hor[i,2]^2 + desc_hor[i,3]))
  desc_vol[i,c(3,4)] = c((2*desc_hor[i,1]*(desc_hor[i,2] + desc_hor[i,1]) / desc_vol[i,2]), 
                             desc_hor[i,3] / desc_vol[i,2])
}


#Se crean nuevas columnas necesarias en el dataset.
stock_tot$q5 = NA
stock_tot$q30 = NA
stock_tot$q60 = NA
stock_tot$Se = NA
stock_tot$Srz5 = NA
stock_tot$Srz30 = NA
stock_tot$Srz60 = NA
stock_tot$ip5 = NA
stock_tot$ip30 = NA
stock_tot$ip60 = NA

#Se calculan los puntos medios con un a, por lo menos, t segundos.
for(i in 1:nrow(stock_tot)){
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 5)),]$day) != 0){
    stock_tot$q5[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 5)),]$bid[1] +
                        stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 5)),]$ask[1]) / 2) 
  }else{next}
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 30)),]$day) != 0){
    stock_tot$q30[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 30)),]$bid[1] +
                         stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 30)),]$ask[1]) / 2) 
  }else{next}
  if(length(stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 60)),]$day) != 0){
    stock_tot$q60[i] = ((stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 60)),]$bid[1] +
                         stockNBBO[which(stockNBBO$day == stock_tot$day[i] & stockNBBO$time >= (stock_tot$time[i] + 60)),]$ask[1]) / 2) 
  }else{next}
  print(paste(i, " 2", sep = ""))
}

#Se eliminan los registros que no tienen punto medio a, por lo menos, t segundos.
stock_tot = stock_tot[!is.na(stock_tot$q5 & stock_tot$q30 & stock_tot$q60),]

#Se calculan la horquilla efectiva, horquilla realizada y el impacto en precios.
for(i in 1:nrow(stock_tot)){
  if(!is.na(stock_tot$buysell[i])){
    if(stock_tot$buysell[i] == 1){
      stock_tot$Se[i] = 2*(stock_tot$ask[i] - stock_tot$q[i])
      stock_tot$Srz5[i] = 2*(stock_tot$ask[i] - stock_tot$q5[i])
      stock_tot$Srz30[i] = 2*(stock_tot$ask[i] - stock_tot$q30[i])
      stock_tot$Srz60[i] = 2*(stock_tot$ask[i] - stock_tot$q60[i])
    }else{
      stock_tot$Se[i] = -2*(stock_tot$bid[i] - stock_tot$q[i])
      stock_tot$Srz5[i] = -2*(stock_tot$bid[i] - stock_tot$q5[i])
      stock_tot$Srz30[i] = -2*(stock_tot$bid[i] - stock_tot$q30[i])
      stock_tot$Srz60[i] = -2*(stock_tot$bid[i] - stock_tot$q60[i])
    }
    stock_tot$ip5[i] = stock_tot$Se[i] - stock_tot$Srz5[i]
    stock_tot$ip30[i] = stock_tot$Se[i] - stock_tot$Srz30[i]
    stock_tot$ip60[i] = stock_tot$Se[i] - stock_tot$Srz60[i]
  }
}

#Se almacenan las variables para no sobreescribirlas con el siguiente activo.
stock_tot_102 = stock_tot
coef_102 = coef
desc_hor_102 = desc_hor
desc_vol_102 = desc_vol
#Se asignan nombres a las columnas.
colnames(coef_102) = c("day", "intercept", "Xt", "Xt-1", "RSE^2")
colnames(desc_hor_102) = c("gamma", "alpha", "RSE^2", "SA", "CO")
colnames(desc_vol_102) = c("SA_PE", "var_dif_PO", "NI_PO", "IP_PO")
