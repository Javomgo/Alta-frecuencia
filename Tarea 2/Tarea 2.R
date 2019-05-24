library(tidyverse)

stock = read_tsv("Stock2LOB.txt")

#Se crean todas las variables que serán necesarias en el algoritmo
hor_rel2 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
prof2 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 4)
prof_ocul2 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 11)
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
      hor_rel2[n, m] = (a[n, m] - b[n, m])/q[n]
      m = m + 1
    }
    cont = cont + 1
    print(cont)
    
    #Cálculo de la profundidad cotizada
    r = 4
    for (k in c(0.2, 0.1, 0.05, 0.01)){
      if(is.na(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),][1,1]) == F & 
         is.na(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote < q[n] & stock$quote > q[n] - k),][1,1]) == F){
        
        prof2[n, r] = sum(sum((stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$dvol + 
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$hvol) *
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$quote),
                        sum((stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote < q[n] & stock$quote > q[n] - k),]$dvol + 
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote < q[n] & stock$quote > q[n] - k),]$hvol) *
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote < q[n] & stock$quote > q[n] - k),]$quote))/2
        
      }else{break}
      r = r - 1
    }
    prof_ocul2[n, 1] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 2] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(1,11)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 3] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(2,12)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 4] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(3,13)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)   
    prof_ocul2[n, 5] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(4,14)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 6] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(5,15)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 7] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(6,16)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 8] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(7,17)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 9] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(8,18)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 10] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(9,19)]) /
                          sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul2[n, 11] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(10,20)]) /
                          sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    n = n + 1
  }
}
stock = drop_na(stock)
cont = 0


