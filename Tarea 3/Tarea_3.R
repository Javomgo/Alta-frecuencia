#Se cargan las librerías necesarias.
library(tidyverse)

#Se leen los datos y almacenan en dataframes.
stock = read_tsv("Stock3.txt")
stockNBBO = read_tsv("Stock3NBBO.txt")

#Se crean las variables que serán necesarias en el algoritmo
#coef_101 = matrix(nrow = 22, ncol = 5)
#desc_hor_101 = matrix(nrow = 22, ncol = 5)
#desc_vol_101 = matrix(nrow = 22, ncol = 4)
#coef_102 = matrix(nrow = 22, ncol = 5)
#desc_hor_102 = matrix(nrow = 22, ncol = 5)
#desc_vol_102 = matrix(nrow = 22, ncol = 4)
#coef_103 = matrix(nrow = 22, ncol = 5)
#desc_hor_103 = matrix(nrow = 22, ncol = 5)
#desc_vol_103 = matrix(nrow = 22, ncol = 4)
#coef_201 = matrix(nrow = 22, ncol = 5)
#desc_hor_201 = matrix(nrow = 22, ncol = 5)
#desc_vol_201 = matrix(nrow = 22, ncol = 4)
#coef_202 = matrix(nrow = 22, ncol = 5)
#desc_hor_202 = matrix(nrow = 22, ncol = 5)
#desc_vol_202 = matrix(nrow = 22, ncol = 4)
#coef_203 = matrix(nrow = 22, ncol = 5)
#desc_hor_203 = matrix(nrow = 22, ncol = 5)
#desc_vol_203 = matrix(nrow = 22, ncol = 4)
med_coef = matrix(nrow = 6, ncol = 4)
desc_hor_media = matrix(nrow = 6, ncol = 2)
desc_vol_SA = matrix(nrow = 6, ncol = 1)
desc_vol_media = matrix(nrow = 6, ncol = 2)
media_Se_Srz_ip_101 = matrix(nrow = 22, ncol = 3)
media_Se_Srz_ip_102 = matrix(nrow = 22, ncol = 3)
media_Se_Srz_ip_103 = matrix(nrow = 22, ncol = 3)
media_Se_Srz_ip_201 = matrix(nrow = 22, ncol = 3)
media_Se_Srz_ip_202 = matrix(nrow = 22, ncol = 3)
media_Se_Srz_ip_203 = matrix(nrow = 22, ncol = 3)
med_mens_Se_Srz_ip = matrix(nrow = 6, ncol = 3)

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
stock_tot_003 = stock_tot
coef_003 = coef
desc_hor_003 = desc_hor
desc_vol_003 = desc_vol
#Se asignan nombres a las columnas.
colnames(coef_003) = c("day", "intercept", "Xt", "Xt-1", "RSE^2")
colnames(desc_hor_003) = c("gamma", "alpha", "RSE^2", "SA", "CO")
colnames(desc_vol_003) = c("SA_PE", "var_dif_PO", "NI_PO", "IP_PO")

#Se calculan las medias de coeficientes por activo.
med_coef[1,] = c(mean(coef_101[,2]),mean(coef_101[,3]),mean(coef_101[,4]),mean(coef_101[,5]))
med_coef[2,] = c(mean(coef_102[,2]),mean(coef_102[,3]),mean(coef_102[,4]),mean(coef_102[,5]))
med_coef[3,] = c(mean(coef_103[,2]),mean(coef_103[,3]),mean(coef_103[,4]),mean(coef_103[,5]))
med_coef[4,] = c(mean(coef_201[-14,2]),mean(coef_201[-14,3]),mean(coef_201[-14,4]),mean(coef_201[-14,5]))
med_coef[5,] = c(mean(coef_202[c(-4,-9),2]),mean(coef_202[c(-4,-9),3]),mean(coef_202[c(-4,-9),4]),mean(coef_202[c(-4,-9),5]))
med_coef[6,] = c(mean(coef_203[,2]),mean(coef_203[,3]),mean(coef_203[,4]),mean(coef_203[,5]))


#Se calcula la media de porcentajes de descomposición de la horquilla.
desc_hor_media[1,] = c(mean(desc_hor_101[,4]),mean(desc_hor_101[,5]))
desc_hor_media[2,] = c(mean(desc_hor_102[,4]),mean(desc_hor_102[,5]))
desc_hor_media[3,] = c(mean(desc_hor_103[,4]),mean(desc_hor_103[,5]))
desc_hor_media[4,] = c(mean(desc_hor_201[-14,4]),mean(desc_hor_201[-14,5]))
desc_hor_media[5,] = c(mean(desc_hor_202[-9,4]),mean(desc_hor_202[-9,5]))
desc_hor_media[6,] = c(mean(desc_hor_203[,4]),mean(desc_hor_203[,5]))

#Se calcula la media de porcentajes de descomposición de la volatilidad del precio eficiente.
desc_vol_SA[1,] = mean(desc_vol_101[,1])
desc_vol_SA[2,] = mean(desc_vol_102[,1])
desc_vol_SA[3,] = mean(desc_vol_103[,1])
desc_vol_SA[4,] = mean(desc_vol_201[-14,1])
desc_vol_SA[5,] = mean(desc_vol_202[c(-4,-9),1])
desc_vol_SA[6,] = mean(desc_vol_203[,1])

#Se calcula la media de porcentajes de descomposición de la volatilidad del precio observado.
desc_vol_media[1,] = c(mean(desc_vol_101[,3]),mean(desc_vol_101[,4]))
desc_vol_media[2,] = c(mean(desc_vol_102[,3]),mean(desc_vol_102[,4]))
desc_vol_media[3,] = c(mean(desc_vol_103[,3]),mean(desc_vol_103[,4]))
desc_vol_media[4,] = c(mean(desc_vol_201[-14,3]),mean(desc_vol_201[-14,4]))
desc_vol_media[5,] = c(mean(desc_vol_202[c(-4,-9),3]),mean(desc_vol_202[c(-4,-9),4]))
desc_vol_media[6,] = c(mean(desc_vol_203[,3]),mean(desc_vol_203[,4]))

#Se calcula la media de la horquilla efectiva, realizada y el impacto en precios.
#Se almacenan unicamente las columnas que nos interesan y se omiten NAs
Se_Srz_ip_101 = na.omit(stock_tot_101[,c(1,11:17)])
Se_Srz_ip_102 = na.omit(stock_tot_102[,c(1,11:17)])
Se_Srz_ip_103 = na.omit(stock_tot_103[,c(1,11:17)])
Se_Srz_ip_201 = na.omit(stock_tot_201[,c(1,11:17)])
Se_Srz_ip_202 = na.omit(stock_tot_202[,c(1,11:17)])
Se_Srz_ip_203 = na.omit(stock_tot_203[,c(1,11:17)])
#Se calculan las medias
j=0
for (i in unique(Se_Srz_ip_101$day)){
  j=j+1
  media_Se_Srz_ip_101[j,] = c(mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),3]),
                                mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),4]),
                                mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),6]),
                                mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),7]),
                                mean(Se_Srz_ip_101[which(Se_Srz_ip_101$day == i),8]))))
  media_Se_Srz_ip_102[j,] = c(mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),3]),
                                mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),4]),
                                mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),6]),
                                mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),7]),
                                mean(Se_Srz_ip_102[which(Se_Srz_ip_102$day == i),8]))))
  media_Se_Srz_ip_103[j,] = c(mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),3]),
                                mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),4]),
                                mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),6]),
                                mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),7]),
                                mean(Se_Srz_ip_103[which(Se_Srz_ip_103$day == i),8]))))
  media_Se_Srz_ip_201[j,] = c(mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),3]),
                                mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),4]),
                                mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),6]),
                                mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),7]),
                                mean(Se_Srz_ip_201[which(Se_Srz_ip_201$day == i),8]))))
  media_Se_Srz_ip_202[j,] = c(mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),3]),
                                mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),4]),
                                mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),6]),
                                mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),7]),
                                mean(Se_Srz_ip_202[which(Se_Srz_ip_202$day == i),8]))))
  media_Se_Srz_ip_203[j,] = c(mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),2]), 
                              mean(c(mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),3]),
                                mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),4]),
                                mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),5]))),
                              mean(c(mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),6]),
                                mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),7]),
                                mean(Se_Srz_ip_203[which(Se_Srz_ip_203$day == i),8]))))
}

#Se calculan las medias mensuales por activo.
med_mens_Se_Srz_ip[1,] = c(mean(media_Se_Srz_ip_101[,1]),mean(media_Se_Srz_ip_101[,2]),mean(media_Se_Srz_ip_101[,3]))
med_mens_Se_Srz_ip[2,] = c(mean(media_Se_Srz_ip_102[,1]),mean(media_Se_Srz_ip_102[,2]),mean(media_Se_Srz_ip_102[,3]))
med_mens_Se_Srz_ip[3,] = c(mean(media_Se_Srz_ip_103[,1]),mean(media_Se_Srz_ip_103[,2]),mean(media_Se_Srz_ip_103[,3]))
med_mens_Se_Srz_ip[4,] = c(mean(media_Se_Srz_ip_201[,1]),mean(media_Se_Srz_ip_201[,2]),mean(media_Se_Srz_ip_201[,3]))
med_mens_Se_Srz_ip[5,] = c(mean(media_Se_Srz_ip_202[,1]),mean(media_Se_Srz_ip_202[,2]),mean(media_Se_Srz_ip_202[,3]))
med_mens_Se_Srz_ip[6,] = c(mean(media_Se_Srz_ip_203[,1]),mean(media_Se_Srz_ip_203[,2]),mean(media_Se_Srz_ip_203[,3]))

#Se asignan nombres a las tablas
dimnames(desc_hor_media) = list(c("101","102","103","201","202",
                            "203"), c("SA","CO"))
dimnames(desc_vol_SA) = list(c("101","102","103","201","202",
                                  "203"), c("SA"))
dimnames(desc_vol_media) = list(c("101","102","103","201","202",
                               "203"), c("NI", "IP"))
dimnames(media_Se_Srz_ip_101) = list(unique(Se_Srz_ip_101$day), c("Se", "Srz", "IP"))
dimnames(media_Se_Srz_ip_102) = list(unique(Se_Srz_ip_102$day), c("Se", "Srz", "IP"))
dimnames(media_Se_Srz_ip_103) = list(unique(Se_Srz_ip_103$day), c("Se", "Srz", "IP"))
dimnames(media_Se_Srz_ip_201) = list(unique(Se_Srz_ip_201$day), c("Se", "Srz", "IP"))
dimnames(media_Se_Srz_ip_202) = list(unique(Se_Srz_ip_202$day), c("Se", "Srz", "IP"))
dimnames(media_Se_Srz_ip_203) = list(unique(Se_Srz_ip_203$day), c("Se", "Srz", "IP"))
dimnames(med_mens_Se_Srz_ip) = list(c("101","102","103","201","202",
                                  "203"), c("Se", "Srz", "IP"))
dimnames(med_coef) = list(c("101","102","103","201","202",
                                      "203"), c("intercept", "Xt", "Xt-1", "RSE^2"))

#Se muestran las tablas.
knitr::kable(coef_101)
knitr::kable(coef_102)
knitr::kable(coef_103)
knitr::kable(coef_201)
knitr::kable(coef_202)
knitr::kable(coef_203)
knitr::kable(med_coef)
knitr::kable(desc_hor_media)
knitr::kable(desc_vol_SA)
knitr::kable(desc_vol_media)
knitr::kable(media_Se_Srz_ip_101)
knitr::kable(media_Se_Srz_ip_102)
knitr::kable(media_Se_Srz_ip_103)
knitr::kable(media_Se_Srz_ip_201)
knitr::kable(media_Se_Srz_ip_202)
knitr::kable(media_Se_Srz_ip_203)
knitr::kable(med_mens_Se_Srz_ip)
