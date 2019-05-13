library(tidyverse)
library(MLmetrics)
library(forecast)
library(huxtable)

stock_201 = read_tsv("stock2.txt")

#Se crean todas las variables que serán necesarias en el algoritmo
stock_201$tick_rule = NA
stock_201$quote_rule = NA
stock_201$Lee_Ready = NA
tick_prec = NA
quote_prec = NA
LR_prec = NA
vol_compra_real = rep(0,13)
vol_venta_real = rep(0,13)
vol_compra_tick = rep(0,13)
vol_venta_tick = rep(0,13)
vol_compra_quote = rep(0,13)
vol_venta_quote = rep(0,13)
vol_compra_LR = rep(0,13)
vol_venta_LR = rep(0,13)
vol_cc_tick = rep(0,13)
vol_cc_quote = rep(0,13)
vol_cc_LR = rep(0,13)
vol_tot_real = rep(0,13)
tabla1 = matrix(nrow=30,ncol=3)
tabla2 = matrix(nrow=30,ncol=18)

for(i in 1:nrow(stock_201)){
  
  #Algoritmo de clasificación por los diferentes métodos  
  
  if(i != 1){
    if(stock_201$day[i] == stock_201$day[i-1]){
      if(stock_201$price[i] > stock_201$price[i-1]){
        stock_201$tick_rule[i] = 1
      }else if(stock_201$price[i] < stock_201$price[i-1]){
        stock_201$tick_rule[i] = -1
      }else{
        stock_201$tick_rule[i] = stock_201$tick_rule[i-1]
      }
    }else{stock_201$tick_rule[i] = NA}
  }else{stock_201$tick_rule[i] = NA}
  
  if(stock_201$price[i] > ((stock_201$ask[i] + stock_201$bid[i])/2)){
    stock_201$quote_rule[i] = 1
    stock_201$Lee_Ready[i] = 1
  }else if(stock_201$price[i] < ((stock_201$ask[i] + stock_201$bid[i])/2)){
    stock_201$quote_rule[i] = -1
    stock_201$Lee_Ready[i] = -1
  }else{
    stock_201$Lee_Ready[i] = stock_201$Lee_Ready[i-1]
  }
  
  #Algoritmo de cálculo de precisión
  
  if(is.na(stock_201$tick_rule[i]) == F){
    if(stock_201$tick_rule[i] == stock_201$buysell[i]){
      stock_201$tick_prec[i]=1
    }else{stock_201$tick_prec[i]=0}
  }else{stock_201$tick_prec[i]=0}
  
  if(is.na(stock_201$quote_rule[i]) == F){
    if(stock_201$quote_rule[i] == stock_201$buysell[i]){
      stock_201$quote_prec[i]=1
    }else{stock_201$quote_prec[i]=0}
  }else{stock_201$quote_prec[i]=0}
  
  if(is.na(stock_201$Lee_Ready[i]) == F){
    if(stock_201$Lee_Ready[i] == stock_201$buysell[i]){
      stock_201$LR_prec[i]=1
    }else{stock_201$LR_prec[i]=0}
  }else{stock_201$LR_prec[i]=0}

  #Algoritmo de cálculo de volumen
  #Periodo 1 [9:30, 10:00]
  if(stock_201$time[i] >= 34200.00 & stock_201$time[i] <= 36000.00){
    j=1
  }
  #Periodo 2 (10:00, 10:30]
  if(stock_201$time[i] > 36000.00 & stock_201$time[i] <= 37800.00){
    j=2
  }
  #Periodo 3 (10:30, 11:00]
  if(stock_201$time[i] > 37800.00 & stock_201$time[i] <= 39600.00){
    j=3
  }
  #Periodo 4 (11:00, 11:30]
  if(stock_201$time[i] > 39600.00 & stock_201$time[i] <= 41400.00){
    j=4
  }
  #Periodo 5 (11:30, 12:00]
  if(stock_201$time[i] > 41400.00 & stock_201$time[i] <= 43200.00){
    j=5
  }
  #Periodo 6 (12:00, 12:30]
  if(stock_201$time[i] > 43200.00 & stock_201$time[i] <= 45000.00){
    j=6
  }
  #Periodo 7 (12:30, 13:00]
  if(stock_201$time[i] > 45000.00 & stock_201$time[i] <= 46800.00){
    j=7
  }
  #Periodo 8 (13:00, 13:30]
  if(stock_201$time[i] > 46800.00 & stock_201$time[i] <= 48600.00){
    j=8
  }
  #Periodo 9 (13:30, 14:00]
  if(stock_201$time[i] > 48600.00 & stock_201$time[i] <= 50400.00){
    j=9
  }
  #Periodo 10 (14:00, 14:30]
  if(stock_201$time[i] > 50400.00 & stock_201$time[i] <= 52200.00){
    j=10
  }
  #Periodo 11 (14:30, 15:00]
  if(stock_201$time[i] > 52200.00 & stock_201$time[i] <= 54000.00){
    j=11
  }
  #Periodo 12 (15:00, 15:30]
  if(stock_201$time[i] > 54000.00 & stock_201$time[i] <= 55800.00){
    j=12
  }
  #Periodo 13 (15:30, 16:00]
  if(stock_201$time[i] > 55800.00 & stock_201$time[i] <= 57600.00){
    j=13
  }
    #Volumen compra/venta real
    if(stock_201$buysell[i] == 1){
      vol_compra_real[j] = vol_compra_real[j] + stock_201$vol[i]
    }else{vol_venta_real[j] = vol_venta_real[j] + stock_201$vol[i]}
    
    #Volumen compra/venta regla de tick
    if(is.na(stock_201$tick_rule[i]) == F){
      if(stock_201$tick_rule[i] == 1){
        vol_compra_tick[j] = vol_compra_tick[j] + stock_201$vol[i]
      }else if(stock_201$tick_rule[i] == -1){
        vol_venta_tick[j] = vol_venta_tick[j] + stock_201$vol[i]
      }
    }
    #Volumen compra/venta regla de quote
  if(is.na(stock_201$quote_rule[i]) == F){
    if(stock_201$quote_rule[i] == 1){
      vol_compra_quote[j] = vol_compra_quote[j] + stock_201$vol[i]
    }else if(stock_201$quote_rule[i] == -1){
      vol_venta_quote[j] = vol_venta_quote[j] + stock_201$vol[i]
    }
  }
    #Volumen compra/venta regla de Lee y Ready
    if(stock_201$Lee_Ready[i] == 1){
      vol_compra_LR[j] = vol_compra_LR[j] + stock_201$vol[i]
    }else{vol_venta_LR[j] = vol_venta_LR[j] + stock_201$vol[i]}
}

#Volumenes correctamente clasificados por algoritmo
for (i in 1:13){
  vol_cc_tick[i] = min(vol_compra_real[i], vol_compra_tick[i]) + min(vol_venta_real[i], vol_venta_tick[i])
  vol_cc_quote[i] = min(vol_compra_real[i], vol_compra_quote[i]) + min(vol_venta_real[i], vol_venta_quote[i])
  vol_cc_LR[i] = min(vol_compra_real[i], vol_compra_LR[i]) + min(vol_venta_real[i], vol_venta_LR[i])
  vol_tot_real[i] = vol_compra_real[i] + vol_venta_real[i]
}

#Medida agregada y relativa de precisión por algoritmo
tick_agr_prec = sum(vol_cc_tick)/sum(vol_tot_real)
quote_agr_prec = sum(vol_cc_quote)/sum(vol_tot_real)
LR_agr_prec = sum(vol_cc_LR)/sum(vol_tot_real)


#Calculo de precisión de cada algoritmo
for (i in 1:30){
  tick_prec[i] = mean(stock_201[which(stock_201$day == i),]$tick_prec)
  quote_prec[i] = mean(stock_201[which(stock_201$day == i),]$quote_prec)
  LR_prec[i] = mean(stock_201[which(stock_201$day == i),]$LR_prec)
}
#Se almacenan los resultados
#Precisión diaria por algoritmo
prec_stock_2 = array(c(tick_prec, quote_prec, LR_prec), c(30,3))
dimnames(prec_stock_2) = list(c(),c("tick_prec", "quote_prec", "LR_prec"))
#Volumen correctamente clasificado, y de compra/venta por algoritmo
vol_stock_2 = array(c(vol_cc_tick, vol_cc_quote, vol_cc_LR, vol_tot_real, vol_compra_real, vol_venta_real, vol_compra_tick, vol_venta_tick, vol_compra_quote, vol_venta_quote, vol_compra_LR, vol_venta_LR), c(13,12))
dimnames(vol_stock_2) = list(c(),c("vol_cc_tick", "vol_cc_quote", "vol_cc_LR", "vol_tot_real", "vol_compra_real", "vol_venta_real", "vol_compra_tick", "vol_venta_tick", "vol_compra_quote", "vol_venta_quote", "vol_compra_LR", "vol_venta_LR"))
#Precisión agregada por algoritmo
agr_prec_stock_2 = c(tick_agr_prec, quote_agr_prec, LR_agr_prec)

#Creación de las tablas
for (i in 1:30){
  
  #Cálculo de la precisión de cada algoritmo y cada día (Tabla I)
  tabla1[i,1] =  (prec_stock_1[i,1] + prec_stock_2[i,1] + prec_stock_3[i,1] + prec_stock_101[i,1] + prec_stock_102[i,1] 
                  + prec_stock_103[i,1] + prec_stock_201[i,1] + prec_stock_202[i,1] + prec_stock_203[i,1])/9
  tabla1[i,2] =  (prec_stock_1[i,2] + prec_stock_2[i,2] + prec_stock_3[i,2] + prec_stock_101[i,2] + prec_stock_102[i,2] 
                  + prec_stock_103[i,2] + prec_stock_201[i,2] + prec_stock_202[i,2] + prec_stock_203[i,2])/9
  tabla1[i,3] =  (prec_stock_1[i,3] + prec_stock_2[i,3] + prec_stock_3[i,3] + prec_stock_101[i,3] + prec_stock_102[i,3] 
                  + prec_stock_103[i,3] + prec_stock_201[i,3] + prec_stock_202[i,3] + prec_stock_203[i,3])/9
  
  #Cálculo de la precisión media diaría por activo y su desviación estandard (Tabla II)
  tabla2[i,1] = mean(prec_stock_1[i,])
  tabla2[i,2] = sd(prec_stock_1[i,])
  tabla2[i,3] = mean(prec_stock_2[i,])
  tabla2[i,4] = sd(prec_stock_2[i,])
  tabla2[i,5] = mean(prec_stock_3[i,])
  tabla2[i,6] = sd(prec_stock_3[i,])
  tabla2[i,7] = mean(prec_stock_101[i,])
  tabla2[i,8] = sd(prec_stock_101[i,])
  tabla2[i,9] = mean(prec_stock_102[i,])
  tabla2[i,10] = sd(prec_stock_102[i,])
  tabla2[i,11] = mean(prec_stock_103[i,])
  tabla2[i,12] = sd(prec_stock_103[i,])
  tabla2[i,13] = mean(prec_stock_201[i,])
  tabla2[i,14] = sd(prec_stock_201[i,])
  tabla2[i,15] = mean(prec_stock_202[i,])
  tabla2[i,16] = sd(prec_stock_202[i,])
  tabla2[i,17] = mean(prec_stock_203[i,])
  tabla2[i,18] = sd(prec_stock_203[i,])
}

#Creación de la tabla III juntando en una matrix los valores de precisión agregara anteriormente calculados.
tabla3 = matrix(rbind(agr_prec_stock_1, agr_prec_stock_2, agr_prec_stock_3, agr_prec_stock_101, agr_prec_stock_102, agr_prec_stock_103, 
                      agr_prec_stock_201, agr_prec_stock_202, agr_prec_stock_203), nrow = 9, ncol = 3, )


dimnames(tabla1) = list(c(),c("tick_rule", "quote_rule", "Lee y Ready"))
dimnames(tabla2) = list(c(),c("mean_stock_1", "sd_stock_1","mean_stock_2", "sd_stock_2","mean_stock_3", "sd_stock_3",
                              "mean_stock_101", "sd_stock_101","mean_stock_102", "sd_stock_102","mean_stock_103", "sd_stock_103",
                              "mean_stock_201", "sd_stock_201","mean_stock_202", "sd_stock_202","mean_stock_203", "sd_stock_203"))
dimnames(tabla3) = list(c("stock_1", "stock_2", "stock_3", "stock_101", "stock_102", "stock_103", 
                          "stock_201", "stock_202", "stock_203"),c("tick_rule", "quote_rule", "Lee y Ready"))




knitr::kable(tabla1)
knitr::kable(tabla2)
knitr::kable(tabla3)
