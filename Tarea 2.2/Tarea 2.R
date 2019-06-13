library(tidyverse)
library(ggplot2)

stock = read_tsv("Stock3LOB.txt")

#Se crean todas las variables que serán necesarias en el algoritmo

hor_rel_001 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_002 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_003 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_101 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_102 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_103 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_201 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_202 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
hor_rel_203 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 5)
prof_001 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_002 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_003 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_101 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_102 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_103 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_201 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_202 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_203 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 6)
prof_ocul_001 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_002 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_003 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_101 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_102 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_103 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_201 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_202 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
prof_ocul_203 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 13)
media_diaria_001 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_002 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_003 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_101 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_102 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_103 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_201 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_202 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_diaria_203 = matrix(nrow = (length(unique(stock$date))), ncol = 2)
media_15min_001 = matrix(nrow = 389, ncol = 3)
media_15min_002 = matrix(nrow = 389, ncol = 3)
media_15min_003 = matrix(nrow = 389, ncol = 3)
media_15min_101 = matrix(nrow = 389, ncol = 3)
media_15min_102 = matrix(nrow = 389, ncol = 3)
media_15min_103 = matrix(nrow = 389, ncol = 3)
media_15min_201 = matrix(nrow = 389, ncol = 3)
media_15min_202 = matrix(nrow = 389, ncol = 3)
media_15min_203 = matrix(nrow = 389, ncol = 3)
hor_rel3 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
prof3 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 4)
prof_ocul3 = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 11)
q = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 1)
a = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
b = matrix(nrow = (length(unique(stock$date)) * length(unique(stock$time))), ncol = 3)
tabla1 = matrix(nrow = 9, ncol = 4)
tabla2.1 = matrix(nrow = 1, ncol = 1)
tabla2.2 = matrix(nrow = 3, ncol = 1)
tabla2.3 = matrix(nrow = 9, ncol = 1)
tabla2.4 = matrix(nrow = 10, ncol = 1)
imagen1 = matrix(nrow = 25, ncol = 18)
tabla4.1 = matrix(nrow = 15, ncol = 3)
tabla4.2 = matrix(nrow = 15, ncol = 3)
n = 1

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
      #Cálculo de la horquilla relativa (se convierte a puntos básicos más abajo para no reejecutar todo el algoritmo debido a los costes de computación)
      hor_rel3[n, m] = (a[n, m] - b[n, m])/q[n]
      m = m + 1
    }
    cont = cont + 1
    print(cont)
    
    #Cálculo de la profundidad cotizada
    r = 4
    for (k in c(0.2, 0.1, 0.05, 0.01)){
      if(is.na(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),][1,1]) == F & 
         is.na(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote < q[n] & stock$quote > q[n] - k),][1,1]) == F){
        
        prof3[n, r] = log(sum((stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$dvol + 
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$hvol) *
                              stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i] & stock$quote > q[n] & stock$quote < q[n] + k),]$quote))
                        
      }else{prof3[n, r] = 0}
      r = r - 1
    }
    
    #Cálculo de la profundidad cotizada oculta
    prof_ocul3[n, 1] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 2] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(1,11)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 3] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(2,12)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 4] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(3,13)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)   
    prof_ocul3[n, 5] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(4,14)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 6] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(5,15)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 7] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(6,16)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 8] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(7,17)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 9] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(8,18)]) /
                         sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                          stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 10] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(9,19)]) /
                          sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    prof_ocul3[n, 11] = sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol[c(10,20)]) /
                          sum(stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$dvol, 
                           stock[which(stock$time == unique(stock$time)[j] & stock$date == unique(stock$date)[i]),]$hvol)
    n = n + 1
  }
}

#Se añaden las columnas date y time a las estimaciones calculadas y se pasa el valor de las horquillas a puntos basicos.
n = 1

for (i in 1:length(unique(stock$date))){
  for (j in 1:length(unique(stock$time))){
    
    hor_rel_001[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel1[n,]*100)
    hor_rel_002[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel2[n,]*100)
    hor_rel_003[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel3[n,]*100)
    hor_rel_101[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel101[n,]*100)
    hor_rel_102[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel102[n,]*100)
    hor_rel_103[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel103[n,]*100)
    hor_rel_201[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel201[n,]*100)
    hor_rel_202[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel202[n,]*100)
    hor_rel_203[n,] = c(unique(stock$date)[i], unique(stock$time)[j], hor_rel203[n,]*100)
    prof_001[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof1[n,])
    prof_002[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof2[n,])
    prof_003[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof3[n,])
    prof_101[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof101[n,])
    prof_102[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof102[n,])
    prof_103[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof103[n,])
    prof_201[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof201[n,])
    prof_202[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof202[n,])
    prof_203[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof203[n,])
    prof_ocul_001[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul1[n,])
    prof_ocul_002[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul2[n,])
    prof_ocul_003[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul3[n,])
    prof_ocul_101[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul101[n,])
    prof_ocul_102[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul102[n,])
    prof_ocul_103[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul103[n,])
    prof_ocul_201[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul201[n,])
    prof_ocul_202[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul202[n,])
    prof_ocul_203[n,] = c(unique(stock$date)[i], unique(stock$time)[j], prof_ocul203[n,])
 n = n + 1
  }
}


#Cálculo de la media diaria tanto de la horquilla relativa como de la profundidad del ask.
for (i in 1:length(unique(stock$date))){
  media_diaria_001[i,] = c(mean(na.omit(hor_rel_001[which(hor_rel_001[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_001[which(prof_001[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_002[i,] = c(mean(na.omit(hor_rel_002[which(hor_rel_002[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_002[which(prof_002[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_003[i,] = c(mean(na.omit(hor_rel_003[which(hor_rel_003[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_003[which(prof_003[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_101[i,] = c(mean(na.omit(hor_rel_101[which(hor_rel_101[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_101[which(prof_101[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_102[i,] = c(mean(na.omit(hor_rel_102[which(hor_rel_102[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_102[which(prof_102[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_103[i,] = c(mean(na.omit(hor_rel_103[which(hor_rel_103[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_103[which(prof_103[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_201[i,] = c(mean(na.omit(hor_rel_201[which(hor_rel_201[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_201[which(prof_201[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_202[i,] = c(mean(na.omit(hor_rel_202[which(hor_rel_202[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_202[which(prof_202[,1] == unique(stock$date)[i]),3:6]))
  media_diaria_203[i,] = c(mean(na.omit(hor_rel_203[which(hor_rel_203[,1] == unique(stock$date)[i]),3:5])),
                           mean(prof_203[which(prof_203[,1] == unique(stock$date)[i]),3:6]))
}

#Se almacena la media diaria y el rank-sum test de Wilcoxon en una tabla para facilitar su visionado.
tabla1[1,1] = mean(media_diaria_001[,1])
tabla1[1,2] = wilcox.test(media_diaria_001[,1], paired = F)$p.value
tabla1[1,3] = mean(media_diaria_001[,2])
tabla1[1,4] = wilcox.test(media_diaria_001[,2], paired = F)$p.value
tabla1[2,1] = mean(media_diaria_002[,1])
tabla1[2,2] = wilcox.test(media_diaria_002[,1], paired = F)$p.value
tabla1[2,3] = mean(media_diaria_002[,2])
tabla1[2,4] = wilcox.test(media_diaria_002[,2], paired = F)$p.value
tabla1[3,1] = mean(media_diaria_003[,1])
tabla1[3,2] = wilcox.test(media_diaria_003[,1], paired = F)$p.value
tabla1[3,3] = mean(media_diaria_003[,2])
tabla1[3,4] = wilcox.test(media_diaria_003[,2], paired = F)$p.value
tabla1[4,1] = mean(media_diaria_101[,1])
tabla1[4,2] = wilcox.test(media_diaria_101[,1], paired = F)$p.value
tabla1[4,3] = mean(media_diaria_101[,2])
tabla1[4,4] = wilcox.test(media_diaria_101[,2], paired = F)$p.value
tabla1[5,1] = mean(media_diaria_102[,1])
tabla1[5,2] = wilcox.test(media_diaria_102[,1], paired = F)$p.value
tabla1[5,3] = mean(media_diaria_102[,2])
tabla1[5,4] = wilcox.test(media_diaria_102[,2], paired = F)$p.value
tabla1[6,1] = mean(media_diaria_103[,1])
tabla1[6,2] = wilcox.test(media_diaria_103[,1], paired = F)$p.value
tabla1[6,3] = mean(media_diaria_103[,2])
tabla1[6,4] = wilcox.test(media_diaria_103[,2], paired = F)$p.value
tabla1[7,1] = mean(media_diaria_201[,1])
tabla1[7,2] = wilcox.test(media_diaria_201[,1], paired = F)$p.value
tabla1[7,3] = mean(media_diaria_201[,2])
tabla1[7,4] = wilcox.test(media_diaria_201[,2], paired = F)$p.value
tabla1[8,1] = mean(media_diaria_202[,1])
tabla1[8,2] = wilcox.test(media_diaria_202[,1], paired = F)$p.value
tabla1[8,3] = mean(media_diaria_202[,2])
tabla1[8,4] = wilcox.test(media_diaria_202[,2], paired = F)$p.value
tabla1[9,1] = mean(media_diaria_203[,1])
tabla1[9,2] = wilcox.test(media_diaria_203[,1], paired = F)$p.value
tabla1[9,3] = mean(media_diaria_203[,2])
tabla1[9,4] = wilcox.test(media_diaria_203[,2], paired = F)$p.value
dimnames(tabla1) = list(c("001","002","003","101","102","103","201","202","203"),c("horquilla relativa", "p-valor", "profundidad", "p-valor"))



#Cálculo de las medias en intervalos de 15 minutos.
for (i in 1:389){
  media_15min_001[i,] = c(hor_rel_001[i*15,1], mean(c(na.omit(hor_rel_001[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_001[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_001[c((i-1)*15+(1:15)),4], prof_001[c((i-1)*15+(1:15)),6])))
  media_15min_002[i,] = c(hor_rel_002[i*15,1], mean(c(na.omit(hor_rel_002[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_002[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_002[c((i-1)*15+(1:15)),4], prof_002[c((i-1)*15+(1:15)),6])))
  media_15min_003[i,] = c(hor_rel_003[i*15,1], mean(c(na.omit(hor_rel_003[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_003[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_003[c((i-1)*15+(1:15)),4], prof_003[c((i-1)*15+(1:15)),6])))
  media_15min_101[i,] = c(hor_rel_101[i*15,1], mean(c(na.omit(hor_rel_101[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_101[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_101[c((i-1)*15+(1:15)),4], prof_101[c((i-1)*15+(1:15)),6])))
  media_15min_102[i,] = c(hor_rel_102[i*15,1], mean(c(na.omit(hor_rel_102[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_102[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_102[c((i-1)*15+(1:15)),4], prof_102[c((i-1)*15+(1:15)),6])))
  media_15min_103[i,] = c(hor_rel_103[i*15,1], mean(c(na.omit(hor_rel_103[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_103[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_103[c((i-1)*15+(1:15)),4], prof_103[c((i-1)*15+(1:15)),6])))
  media_15min_201[i,] = c(hor_rel_201[i*15,1], mean(c(na.omit(hor_rel_201[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_201[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_201[c((i-1)*15+(1:15)),4], prof_201[c((i-1)*15+(1:15)),6])))
  media_15min_202[i,] = c(hor_rel_202[i*15,1], mean(c(na.omit(hor_rel_202[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_202[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_202[c((i-1)*15+(1:15)),4], prof_202[c((i-1)*15+(1:15)),6])))
  media_15min_203[i,] = c(hor_rel_203[i*15,1], mean(c(na.omit(hor_rel_203[c((i-1)*15+(1:15)),3]), na.omit(hor_rel_203[c((i-1)*15+(1:15)),5]))),
                          mean(c(prof_203[c((i-1)*15+(1:15)),4], prof_203[c((i-1)*15+(1:15)),6])))
}
#Cálculo de las medias por activo e intervalo en sección cruzada.
for (i in 1:length(media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[1]),2])){
  
  imagen1[i,1] = mean(media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[1]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[2]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[3]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[4]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[5]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[6]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[7]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[8]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[9]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[10]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[11]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[12]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[13]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[14]),2][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[15]),2][i])
  imagen1[i,2] = mean(media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[1]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[2]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[3]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[4]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[5]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[6]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[7]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[8]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[9]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[10]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[11]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[12]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[13]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[14]),3][i],
                      media_15min_001[which(media_15min_001[,1] == unique(media_15min_001[,1])[15]),3][i])
  
  imagen1[i,3] = mean(media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[1]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[2]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[3]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[4]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[5]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[6]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[7]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[8]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[9]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[10]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[11]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[12]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[13]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[14]),2][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[15]),2][i])
  imagen1[i,4] = mean(media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[1]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[2]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[3]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[4]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[5]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[6]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[7]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[8]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[9]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[10]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[11]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[12]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[13]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[14]),3][i],
                      media_15min_002[which(media_15min_002[,1] == unique(media_15min_002[,1])[15]),3][i])
  
  imagen1[i,5] = mean(media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[1]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[2]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[3]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[4]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[5]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[6]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[7]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[8]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[9]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[10]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[11]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[12]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[13]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[14]),2][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[15]),2][i])
  imagen1[i,6] = mean(media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[1]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[2]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[3]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[4]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[5]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[6]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[7]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[8]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[9]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[10]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[11]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[12]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[13]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[14]),3][i],
                      media_15min_003[which(media_15min_003[,1] == unique(media_15min_003[,1])[15]),3][i])
  
  imagen1[i,7] = mean(media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[1]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[2]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[3]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[4]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[5]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[6]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[7]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[8]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[9]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[10]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[11]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[12]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[13]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[14]),2][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[15]),2][i])
  imagen1[i,8] = mean(media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[1]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[2]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[3]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[4]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[5]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[6]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[7]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[8]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[9]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[10]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[11]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[12]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[13]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[14]),3][i],
                      media_15min_101[which(media_15min_101[,1] == unique(media_15min_101[,1])[15]),3][i])
  
  imagen1[i,9] = mean(media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[1]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[2]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[3]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[4]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[5]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[6]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[7]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[8]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[9]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[10]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[11]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[12]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[13]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[14]),2][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[15]),2][i])
  imagen1[i,10] = mean(media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[1]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[2]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[3]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[4]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[5]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[6]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[7]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[8]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[9]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[10]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[11]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[12]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[13]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[14]),3][i],
                      media_15min_102[which(media_15min_102[,1] == unique(media_15min_102[,1])[15]),3][i])
  
  imagen1[i,11] = mean(media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[1]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[2]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[3]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[4]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[5]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[6]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[7]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[8]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[9]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[10]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[11]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[12]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[13]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[14]),2][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[15]),2][i])
  imagen1[i,12] = mean(media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[1]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[2]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[3]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[4]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[5]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[6]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[7]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[8]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[9]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[10]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[11]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[12]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[13]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[14]),3][i],
                      media_15min_103[which(media_15min_103[,1] == unique(media_15min_103[,1])[15]),3][i])
  
  imagen1[i,13] = mean(media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[1]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[2]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[3]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[4]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[5]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[6]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[7]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[8]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[9]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[10]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[11]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[12]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[13]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[14]),2][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[15]),2][i])
  imagen1[i,14] = mean(media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[1]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[2]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[3]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[4]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[5]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[6]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[7]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[8]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[9]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[10]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[11]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[12]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[13]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[14]),3][i],
                       media_15min_201[which(media_15min_201[,1] == unique(media_15min_201[,1])[15]),3][i])
  
  imagen1[i,15] = mean(media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[1]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[2]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[3]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[4]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[5]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[6]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[7]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[8]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[9]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[10]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[11]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[12]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[13]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[14]),2][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[15]),2][i])
  imagen1[i,16] = mean(media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[1]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[2]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[3]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[4]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[5]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[6]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[7]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[8]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[9]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[10]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[11]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[12]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[13]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[14]),3][i],
                       media_15min_202[which(media_15min_202[,1] == unique(media_15min_202[,1])[15]),3][i])
  
  imagen1[i,17] = mean(media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[1]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[2]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[3]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[4]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[5]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[6]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[7]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[8]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[9]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[10]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[11]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[12]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[13]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[14]),2][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[15]),2][i])
  imagen1[i,18] = mean(media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[1]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[2]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[3]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[4]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[5]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[6]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[7]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[8]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[9]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[10]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[11]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[12]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[13]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[14]),3][i],
                       media_15min_203[which(media_15min_203[,1] == unique(media_15min_203[,1])[15]),3][i])
}

imagen1 = as.data.frame(imagen1)
imagen1$index <- as.numeric(row.names(imagen1))

#Cálculo de los estadísticos del volumen oculto.
#Volumen profundidad total oculta.
tabla2.1[1,1] = mean(c(mean(prof_ocul_001[,3]),
                       mean(prof_ocul_002[,3]),
                       mean(prof_ocul_003[,3]),
                       mean(prof_ocul_101[,3]),
                       mean(prof_ocul_102[,3]),
                       mean(prof_ocul_103[,3]),
                       mean(prof_ocul_201[,3]),
                       mean(prof_ocul_202[,3]),
                       mean(prof_ocul_203[,3])))

#Volumen de profundidad oculta por semanas.
tabla2.2[,1] = c(mean(mean(prof_ocul_001[which(prof_ocul_001[,1] == 80407 | prof_ocul_001[,1] == 80408 | prof_ocul_001[,1] == 80409 | prof_ocul_001[,1] == 80410 | prof_ocul_001[,1] == 80411),3]), 
                mean(prof_ocul_002[which(prof_ocul_002[,1] == 80407 | prof_ocul_002[,1] == 80408 | prof_ocul_002[,1] == 80409 | prof_ocul_002[,1] == 80410 | prof_ocul_002[,1] == 80411),3]),
                mean(prof_ocul_003[which(prof_ocul_003[,1] == 80407 | prof_ocul_003[,1] == 80408 | prof_ocul_003[,1] == 80409 | prof_ocul_003[,1] == 80410 | prof_ocul_003[,1] == 80411),3]),
                mean(prof_ocul_101[which(prof_ocul_101[,1] == 80407 | prof_ocul_101[,1] == 80408 | prof_ocul_101[,1] == 80409 | prof_ocul_101[,1] == 80410 | prof_ocul_101[,1] == 80411),3]),
                mean(prof_ocul_102[which(prof_ocul_102[,1] == 80407 | prof_ocul_102[,1] == 80408 | prof_ocul_102[,1] == 80409 | prof_ocul_102[,1] == 80410 | prof_ocul_102[,1] == 80411),3]),
                mean(prof_ocul_103[which(prof_ocul_103[,1] == 80407 | prof_ocul_103[,1] == 80408 | prof_ocul_103[,1] == 80409 | prof_ocul_103[,1] == 80410 | prof_ocul_103[,1] == 80411),3]),
                mean(prof_ocul_201[which(prof_ocul_201[,1] == 80407 | prof_ocul_201[,1] == 80408 | prof_ocul_201[,1] == 80409 | prof_ocul_201[,1] == 80410 | prof_ocul_201[,1] == 80411),3]),
                mean(prof_ocul_202[which(prof_ocul_202[,1] == 80407 | prof_ocul_202[,1] == 80408 | prof_ocul_202[,1] == 80409 | prof_ocul_202[,1] == 80410 | prof_ocul_202[,1] == 80411),3]),
                mean(prof_ocul_203[which(prof_ocul_203[,1] == 80407 | prof_ocul_203[,1] == 80408 | prof_ocul_203[,1] == 80409 | prof_ocul_203[,1] == 80410 | prof_ocul_203[,1] == 80411),3])),
           mean(mean(prof_ocul_001[which(prof_ocul_001[,1] == 80915 | prof_ocul_001[,1] == 80916 | prof_ocul_001[,1] == 80917 | prof_ocul_001[,1] == 80918 | prof_ocul_001[,1] == 80919),3]), 
                mean(prof_ocul_002[which(prof_ocul_002[,1] == 80915 | prof_ocul_002[,1] == 80916 | prof_ocul_002[,1] == 80917 | prof_ocul_002[,1] == 80918 | prof_ocul_002[,1] == 80919),3]),
                mean(prof_ocul_003[which(prof_ocul_003[,1] == 80915 | prof_ocul_003[,1] == 80916 | prof_ocul_003[,1] == 80917 | prof_ocul_003[,1] == 80918 | prof_ocul_003[,1] == 80919),3]),
                mean(prof_ocul_101[which(prof_ocul_101[,1] == 80915 | prof_ocul_101[,1] == 80916 | prof_ocul_101[,1] == 80917 | prof_ocul_101[,1] == 80918 | prof_ocul_101[,1] == 80919),3]),
                mean(prof_ocul_102[which(prof_ocul_102[,1] == 80915 | prof_ocul_102[,1] == 80916 | prof_ocul_102[,1] == 80917 | prof_ocul_102[,1] == 80918 | prof_ocul_102[,1] == 80919),3]),
                mean(prof_ocul_103[which(prof_ocul_103[,1] == 80915 | prof_ocul_103[,1] == 80916 | prof_ocul_103[,1] == 80917 | prof_ocul_103[,1] == 80918 | prof_ocul_103[,1] == 80919),3]),
                mean(prof_ocul_201[which(prof_ocul_201[,1] == 80915 | prof_ocul_201[,1] == 80916 | prof_ocul_201[,1] == 80917 | prof_ocul_201[,1] == 80918 | prof_ocul_201[,1] == 80919),3]),
                mean(prof_ocul_202[which(prof_ocul_202[,1] == 80915 | prof_ocul_202[,1] == 80916 | prof_ocul_202[,1] == 80917 | prof_ocul_202[,1] == 80918 | prof_ocul_202[,1] == 80919),3]),
                mean(prof_ocul_203[which(prof_ocul_203[,1] == 80915 | prof_ocul_203[,1] == 80916 | prof_ocul_203[,1] == 80917 | prof_ocul_203[,1] == 80918 | prof_ocul_203[,1] == 80919),3])),
           mean(mean(prof_ocul_001[which(prof_ocul_001[,1] == 100222 | prof_ocul_001[,1] == 100223 | prof_ocul_001[,1] == 100224 | prof_ocul_001[,1] == 100225 | prof_ocul_001[,1] == 100226),3]), 
                mean(prof_ocul_002[which(prof_ocul_002[,1] == 100222 | prof_ocul_002[,1] == 100223 | prof_ocul_002[,1] == 100224 | prof_ocul_002[,1] == 100225 | prof_ocul_002[,1] == 100226),3]),
                mean(prof_ocul_003[which(prof_ocul_003[,1] == 100222 | prof_ocul_003[,1] == 100223 | prof_ocul_003[,1] == 100224 | prof_ocul_003[,1] == 100225 | prof_ocul_003[,1] == 100226),3]),
                mean(prof_ocul_101[which(prof_ocul_101[,1] == 100222 | prof_ocul_101[,1] == 100223 | prof_ocul_101[,1] == 100224 | prof_ocul_101[,1] == 100225 | prof_ocul_101[,1] == 100226),3]),
                mean(prof_ocul_102[which(prof_ocul_102[,1] == 100222 | prof_ocul_102[,1] == 100223 | prof_ocul_102[,1] == 100224 | prof_ocul_102[,1] == 100225 | prof_ocul_102[,1] == 100226),3]),
                mean(prof_ocul_103[which(prof_ocul_103[,1] == 100222 | prof_ocul_103[,1] == 100223 | prof_ocul_103[,1] == 100224 | prof_ocul_103[,1] == 100225 | prof_ocul_103[,1] == 100226),3]),
                mean(prof_ocul_201[which(prof_ocul_201[,1] == 100222 | prof_ocul_201[,1] == 100223 | prof_ocul_201[,1] == 100224 | prof_ocul_201[,1] == 100225 | prof_ocul_201[,1] == 100226),3]),
                mean(prof_ocul_202[which(prof_ocul_202[,1] == 100222 | prof_ocul_202[,1] == 100223 | prof_ocul_202[,1] == 100224 | prof_ocul_202[,1] == 100225 | prof_ocul_202[,1] == 100226),3]),
                mean(prof_ocul_203[which(prof_ocul_203[,1] == 100222 | prof_ocul_203[,1] == 100223 | prof_ocul_203[,1] == 100224 | prof_ocul_203[,1] == 100225 | prof_ocul_203[,1] == 100226),3])))

#Volumen de profundidad oculta por activo.
tabla2.3[1:9,1] = c(mean(prof_ocul_001[,3]),
                  mean(prof_ocul_002[,3]),
                  mean(prof_ocul_003[,3]),
                  mean(prof_ocul_101[,3]),
                  mean(prof_ocul_102[,3]),
                  mean(prof_ocul_103[,3]),
                  mean(prof_ocul_201[,3]),
                  mean(prof_ocul_202[,3]),
                  mean(prof_ocul_203[,3]))

#Volumen de profundidad oculta según cercania al punto medio.
tabla2.4[1:10,1] = c(mean(mean(prof_ocul_001[,4]), mean(prof_ocul_002[,4]), mean(prof_ocul_003[,4]), mean(prof_ocul_101[,4]), mean(prof_ocul_102[,4]),
                    mean(prof_ocul_103[,4]), mean(prof_ocul_201[,4]), mean(prof_ocul_202[,4]), mean(prof_ocul_203[,4])),
               mean(mean(prof_ocul_001[,5]), mean(prof_ocul_002[,5]), mean(prof_ocul_003[,5]), mean(prof_ocul_101[,5]), mean(prof_ocul_102[,5]),
                    mean(prof_ocul_103[,5]), mean(prof_ocul_201[,5]), mean(prof_ocul_202[,5]), mean(prof_ocul_203[,5])),
               mean(mean(prof_ocul_001[,6]), mean(prof_ocul_002[,6]), mean(prof_ocul_003[,6]), mean(prof_ocul_101[,6]), mean(prof_ocul_102[6]),
                    mean(prof_ocul_103[,6]), mean(prof_ocul_201[,6]), mean(prof_ocul_202[,6]), mean(prof_ocul_203[,6])),
               mean(mean(prof_ocul_001[,7]), mean(prof_ocul_002[,7]), mean(prof_ocul_003[,7]), mean(prof_ocul_101[,7]), mean(prof_ocul_102[,7]),
                    mean(prof_ocul_103[,7]), mean(prof_ocul_201[,7]), mean(prof_ocul_202[,7]), mean(prof_ocul_203[,7])),
               mean(mean(prof_ocul_001[,8]), mean(prof_ocul_002[,8]), mean(prof_ocul_003[,8]), mean(prof_ocul_101[,8]), mean(prof_ocul_102[,8]),
                    mean(prof_ocul_103[,8]), mean(prof_ocul_201[,8]), mean(prof_ocul_202[,8]), mean(prof_ocul_203[,8])),
               mean(mean(prof_ocul_001[,9]), mean(prof_ocul_002[,9]), mean(prof_ocul_003[,9]), mean(prof_ocul_101[,9]), mean(prof_ocul_102[,9]),
                    mean(prof_ocul_103[,9]), mean(prof_ocul_201[,9]), mean(prof_ocul_202[,9]), mean(prof_ocul_203[,9])),
               mean(mean(prof_ocul_001[,10]), mean(prof_ocul_002[,10]), mean(prof_ocul_003[,10]), mean(prof_ocul_101[,10]), mean(prof_ocul_102[,10]),
                    mean(prof_ocul_103[,10]), mean(prof_ocul_201[,10]), mean(prof_ocul_202[,10]), mean(prof_ocul_203[,10])),
               mean(mean(prof_ocul_001[,11]), mean(prof_ocul_002[,11]), mean(prof_ocul_003[,11]), mean(prof_ocul_101[,11]), mean(prof_ocul_102[,11]),
                    mean(prof_ocul_103[,11]), mean(prof_ocul_201[,11]), mean(prof_ocul_202[,11]), mean(prof_ocul_203[,11])),
               mean(mean(prof_ocul_001[,12]), mean(prof_ocul_002[,12]), mean(prof_ocul_003[,12]), mean(prof_ocul_101[,12]), mean(prof_ocul_102[,12]),
                    mean(prof_ocul_103[,12]), mean(prof_ocul_201[,12]), mean(prof_ocul_202[,12]), mean(prof_ocul_203[,12])),
               mean(mean(prof_ocul_001[,13]), mean(prof_ocul_002[,13]), mean(prof_ocul_003[,13]), mean(prof_ocul_101[,13]), mean(prof_ocul_102[,13]),
                    mean(prof_ocul_103[,13]), mean(prof_ocul_201[,13]), mean(prof_ocul_202[,13]), mean(prof_ocul_203[,13])))

#Se asignan nombres a las lineas y columnas en las tablas
dimnames(tabla2.1) = list(c(),c("Profundidad oculta total"))
dimnames(tabla2.2) = list(c("04/08", "09/08", "02/10"), c("Profundidad oculta por semanas"))
dimnames(tabla2.3) = list(c("001","002","003","101","102","103","201","202","203"), c("Profundidad oculta por activo"))
dimnames(tabla2.4) = list(c("Posición 1","Posición 2","Posición 3","Posición 4","Posición 5","Posición 6","Posición 7","Posición 8","Posición 9", "Posición 10"), 
                          c("Profundidad oculta por cercanía al punto medio"))

#Se generan las imagenes.
ggplot(imagen1, aes(x = index)) + 
  geom_line(aes(y = V1, colour = "001")) +
  geom_line(aes(y = V3, colour = "002")) +
  geom_line(aes(y = V5, colour = "003")) +
  geom_line(aes(y = V7, colour = "101")) +
  geom_line(aes(y = V9, colour = "102")) +
  geom_line(aes(y = V11, colour = "103")) +
  geom_line(aes(y = V13, colour = "201")) +
  geom_line(aes(y = V15, colour = "202")) +
  geom_line(aes(y = V17, colour = "203")) +
  labs(x = "Media de cada intervalo de 15 minutos", y = "Horquilla relativa")

ggplot(imagen1, aes(x = index)) + 
  geom_line(aes(y = V2, colour = "001")) +
  geom_line(aes(y = V4, colour = "002")) +
  geom_line(aes(y = V6, colour = "003")) +
  geom_line(aes(y = V8, colour = "101")) +
  geom_line(aes(y = V10, colour = "102")) +
  geom_line(aes(y = V12, colour = "103")) +
  geom_line(aes(y = V14, colour = "201")) +
  geom_line(aes(y = V16, colour = "202")) +
  geom_line(aes(y = V18, colour = "203")) +
  labs(x = "Media de cada intervalo de 15 minutos", y = "Profundidad")

#Almacenamiento de la tabla de las medias de cada día de tres activos.
tabla4.1[,c(1,2,3)] = c(media_diaria_001[,1], media_diaria_101[,1], media_diaria_201[,1])
tabla4.2[,c(1,2,3)] = c(media_diaria_001[,2], media_diaria_101[,2], media_diaria_201[,2])

#Asignación de los nombres de lineas y columnas
dimnames(tabla4.1) = list(c("07/04/08","08/04/08","09/04/08","10/04/08","11/04/08",
                            "15/09/08","16/09/08","17/09/08","18/09/08","19/09/08",
                            "22/02/10","23/02/10","24/02/10","25/02/10","26/02/10"), 
                          c("hor_rel_001","hor_rel_101","hor_rel_201"))
dimnames(tabla4.2) = list(c("07/04/08","08/04/08","09/04/08","10/04/08","11/04/08",
                            "15/09/08","16/09/08","17/09/08","18/09/08","19/09/08",
                            "22/02/10","23/02/10","24/02/10","25/02/10","26/02/10"), 
                          c("prof_001","prof_101","prof_201"))

#Se muestran las tablas.
knitr::kable(tabla1)
knitr::kable(tabla2.1)
knitr::kable(tabla2.2)
knitr::kable(tabla2.3)
knitr::kable(tabla2.4)
knitr::kable(tabla4.1)
knitr::kable(tabla4.2)
