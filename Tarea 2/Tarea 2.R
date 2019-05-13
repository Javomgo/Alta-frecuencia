library(tidyverse)

stock = read_tsv("Stock201LOB.txt")

#Se crean todas las variables que serán necesarias en el algoritmo
hor_rel = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
q = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 1)
a = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
b = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)

#Cálculo horquilla relativa
for (i in 1:length(unique(stock$date))){
  for (j in 1:length(unique(stock$time))){
    #Variable creada para facilitar el almacenamiento en la matriz
    m=1
    #Cálculo del punto medio.
    q[j*i] = (stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == 1),]$dvol + 
                stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == -1),]$dvol)/2
    
    for (k in c(100,500,1000)){
      for(l in 1:10){
        #Cálculo del bid ponderado
        if(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:l)),]$dvol,  
            stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:l)),]$hvol)  >= k &
           !(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol,  
            stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol)  >= k)){
          
          b[(j*i), m] = sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$quote *
                  (stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol +
                   stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol), 
                  stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(l)),]$quote *
                  (k - sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$dvol,
                           stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(1:(l-1))),]$hvol)))/k
        }
      }
      for(l in -1:-10){
        #Cálculo ask ponderado
        if(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:l)),]$dvol,  
               stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:l)),]$hvol)  >= k &
           !(sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$dvol,  
                 stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$hvol)  >= k)){
          
          a[(j*i), m] = sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$quote *
                             (stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$dvol +
                                stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$hvol), 
                           stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(l)),]$quote *
                             (k - sum(stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$dvol,
                                      stock[which(stock$time == stock$time[j] & stock$date == stock$date[i] & stock$sign == c(-1:(l+1))),]$hvol)))/k
        }
      }
      #Cálculo de la horquilla relativa
      hor_rel[(j*i), m] = (a[(j*i), m] - b[(j*i), m])/q[j*i]
      
      m = m + 1
    }
  }
}


