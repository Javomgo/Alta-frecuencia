library(tidyverse)
stock = read_tsv("Stock201.txt")
stockNBBO = read_tsv("Stock201NBBO.txt")

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

for(i in 1:)  
lm(stock_tot$dif_precio ~ stock_tot$buysell + stock_tot$buysell[-1])


lm
