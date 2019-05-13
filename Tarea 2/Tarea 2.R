library(tidyverse)

stock = read_tsv("Stock201LOB.txt")

#Se crean todas las variables que serán necesarias en el algoritmo
hor_rel = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)

#Cálculo horquilla relativa
for (i in 1:length(unique(stock$date))){
  for (j in 1:length(unique(stock$time))){
    m=1
    for (k in c(100,500,1000)){
      for(l in 1:10){
        
        if(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:l)),]$dvol,  
            stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:l)),]$hvol)  >= k &
           !(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol,  
            stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol)  >= k)){
          
          hor_rel[(j*i),m] = sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$quote *
                  (stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol +
                   stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol), 
                  stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(l)),]$quote *
                  (k - sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol,
                           stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol)))/k
        }
      }
      m = m + 1
    }
  }
}




