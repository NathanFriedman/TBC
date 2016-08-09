
library(quantmod)

#declaring variables
matixa <- 0
holdon <- 0
Indexed <- 0
penny <- 0 
early_success <- 0

for (i in c(1:54))
{
  x <- getSymbols(as.character(Data[i,3]),from = as.character(Data[i,13]),auto.assign = FALSE)
  if(as.numeric(x[1,1])<1)
  {
    penny[i] = 1
  } else
  {
    penny[i] = 0
  }
  y <- getSymbols("^GSPTSE",from = as.character(Data[i,13]),auto.assign = FALSE)

  for (j in c(1:24))
  {
  Indexed[j] = log(as.numeric(y[j+1,6])/as.numeric(y[j,6]))
  holdon[j] = log(as.numeric(x[j+1,6])/as.numeric(x[j,6]))
  } 
holdon = holdon - Indexed
if(i == 1)
{
  matixa = cbind(holdon)
} else
{
matixa = cbind(matixa,holdon)
}
}

#cleaning data#
matixa = data.frame(matixa)
names(matixa) <- c(Data[,4])
matixa = rbind(matixa,penny)
penny_stocks = matixa[,which(matixa[25,]==1)]
big_stocks = matixa[,which(matixa[25,]==0)]
matixa = matixa[-25,]
penny_stocks = penny_stocks[-25,]
big_stocks = big_stocks[-25,]
big_stocks= big_stocks[,1:10]
#clustering

dend1EUC <- as.dendrogram(hclust(diss(big_stocks,METHOD = "EUCL")))
dend_1EUC <-hclust(diss(big_stocks,METHOD = "EUCL"))
plot(dend_1EUC, main = 'Dendrogram for 25 day Time Series')

for (j in c(1:10))
{
  early_success[j] = exp(sum(big_stocks[,j])) - 1
}

names(early_success) = names(big_stocks)
early_success = early_success*100
early_success
#whichmin -> SUF, DAQ

#plotting cluster
ts.plot(big_stocks, gpars = list(col = 1:10), main = 'Time Series, 10 Random Stocks', ylab = 'Adjusted Returns')
legend("bottomright", c("H","APR"),lty = 1, col = c("Red","Green","Blue"))
abline(0,0)




#MIDTERM

matixa <- 0
holdon <- 0
Indexed <- 0
penny <- 0 
big_stocks <- 0
late_success <- 0
for (i in c(1:54))
{
  i = 5
  x <- getSymbols(as.character(Data[i,3]),from = as.character(Data[i,13]),auto.assign = FALSE)
  if(as.numeric(x[1,1])<1)
  {
    penny[i] = 1
  } else
  {
    penny[i] = 0
  }
  y <- getSymbols("^GSPTSE",from = as.character(Data[i,13]),auto.assign = FALSE)
  
  for (j in c(31:120))
  {
    Indexed[j - 30] = log(as.numeric(y[j+1,6])/as.numeric(y[j,6]))
    holdon[j - 30] = log(as.numeric(x[j+1,6])/as.numeric(x[j,6]))
  } 
  holdon = holdon - Indexed
  if(i == 1)
  {
    matixa = cbind(holdon)
  } else
  {
    matixa = cbind(matixa,holdon)
  }
}

#cleaning data#
matixa = matixa[-90,]
matixa = data.frame(matixa)
names(matixa) <- c(Data[,4])
matixa = rbind(matixa,penny)
penny_stocks = matixa[,which(matixa[90,]==1)]
big_stocks = matixa[,which(matixa[90,]==0)]
matixa = matixa[-90,]
penny_stocks = penny_stocks[-90,]
big_stocks = big_stocks[-90,]
big_stocks= big_stocks[,1:10]

#clustering
#interpdist <- dist(big_stocks)

#dend2 <- hclust(interpdist)
dend2 <- as.dendrogram(hclust(diss(big_stocks,METHOD = "EUCL")))
dend_2 <- hclust(diss(big_stocks,METHOD = "EUCL"))

for (j in c(1:10))
{
  late_success[j] = exp(sum(big_stocks[,j])) - 1
}

names(late_success) = names(big_stocks)
late_success = late_success*100
late_success


plot(dend_2, main = 'Dendrogram for 30 - 120 days post IPO')
dend_diff(dend1,dend2)


tanglegram(dend1EUC, dend2)


#Longer Term
for (i in c(1:54))
{
  i = 1
  x <- getSymbols(as.character(Data[i,3]),from = as.character(Data[i,13]),auto.assign = FALSE)
  if(as.numeric(x[1,1])<1)
  {
    penny[i] = 1
  } else
  {
    penny[i] = 0
  }
  y <- getSymbols("^GSPTSE",from = as.character(Data[i,13]),auto.assign = FALSE)
  
  for (j in c(1:125))
  {
    Indexed[j] = log(as.numeric(y[j,4])/as.numeric(y[j,1]))
    holdon[j] = log(as.numeric(x[j,4])/as.numeric(x[j,1]))
  } 
  holdon = holdon - Indexed
  if(i == 1)
  {
    matixa = cbind(holdon)
  } else
  {
    matixa = cbind(matixa,holdon)
  }
}

#cleaning data#
matixa = data.frame(matixa)
names(matixa) <- c(Data[,4])
matixa = rbind(matixa,penny)
penny_stocks = matixa[,which(matixa[12,]==1)]
big_stocks = matixa[,which(matixa[126,]==0)]
matixa = matixa[-126,]
penny_stocks = penny_stocks[-126,]
big_stocks = big_stocks[-126, 1:10]

#clustering
interpdist <- diss(big_stocks, METHOD= "COR")
plot(hclust(interpdist), main = 'Cluster non-penny stocks w/COR')

#whichmin -> SUF, DAQ
#plotting cluster
ts.plot(cbind(big_stocks$SUF,big_stocks$DAQ,big_stocks$SH), gpars = list(col = c("Red","Green","Blue")), main = 'plot - EUC stocks of interest')
legend("bottomright", c("SUF","DAQ","SH"),lty = 1, col = c("Red","Green","Blue"))











new_big_stocks = matrix(0,dim(big_stocks)[1], dim(big_stocks)[2])


#differnece in log returns#
for (k in c(1:dim(big_stocks)[2]))
{
for(q in c(1:dim(matixa)[1] -1))
{
  new_big_stocks[q,k] = big_stocks[q+1,k] - big_stocks[q,k]
}
}

new_big_stocks = data.frame(new_big_stocks)
names(new_big_stocks) = names(big_stocks)
ts.plot(cbind(new_big_stocks$H,new_big_stocks$ZZZ), gpars = list(col = c("Red","Green")))




