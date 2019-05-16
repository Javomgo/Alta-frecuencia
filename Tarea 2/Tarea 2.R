library(tidyverse)

stock = read_tsv("Stock201LOB.txt")

#Se crean todas las variables que serán necesarias en el algoritmo
hor_rel = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
q = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 1)
a = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
b = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
n = 1
#Cálculo horquilla relativa
for (i in 1:length(unique(stock$date))){
  for (j in 1:length(unique(stock$time))){
    #Variable creada para facilitar el almacenamiento en la matriz
    m=1
    #Cálculo del punto medio.
    for(l in 1:10){
    if(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign > 0),][l,]$dvol != 0){
      for(p in 1:10){
      if(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign == -p),]$dvol != 0){
        
        q[n] = (stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][l,]$quote + 
                  stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign == -p),]$quote)/2
        break
      }
      }
      break
    }   
  }
    
    for (k in c(100,500,1000)){
      for(l in 1:10){
        #Cálculo del bid ponderado
        if(sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l,]$dvol,  
            stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l,]$hvol)  >= k){
          
          b[n, m] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l-1,]$quote *
                  (stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l-1,]$dvol +
                   stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l-1,]$hvol), 
                  stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][l,]$quote *
                  (k - sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l-1,]$dvol,
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][1:l-1,]$hvol)))/k
          break
        }else if(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][l,3] == -1){
          #stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][,3] = NA
        }
      }
      if(is.na(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][l,3])){break}
      for(l in 1:10){
        #Cálculo ask ponderado
        if(sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l,]$dvol,  
               stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l,]$hvol)  >= k){
          
          a[n, m] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l-1,]$quote *
                             (stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l-1,]$dvol +
                                stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l-1,]$hvol), 
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][l,]$quote *
                             (k - sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l-1,]$dvol,
                                      stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$sign < 0),][1:l-1,]$hvol)))/k
          break
        }else{
          #stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),][,3] = NA
        }
      }
      #Cálculo de la horquilla relativa
      hor_rel[n, m] = (a[n, m] - b[n, m])/q[n]
      print(c(a[n,m],b[n,m],q[n],hor_rel[n,m]))
      m = m + 1
    }
    n = n + 1
    cont = cont + 1
    print(cont)
  }
  
}
stock = drop_na(stock)
cont = 0


