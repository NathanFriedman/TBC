getit <- list()
geter <- list()
DDR = data.frame(brokerinfo$V1)
DT <- fread("~/Desktop/x201501-trades.csv")
DT = data.frame(DT)
DT = DT[,-1]
DT = DT[,-c(2,5)]
DT = DT[,-c(4,9,10)]
names(DT) = c("Date","Ticker","Hour","P","Q","Buyer","Seller","Sector")
DT$P = DT$P/1000
DT[,"value"] = DT$P*DT$Q
f = unique(DT$Date)
for(k in c(1: length(unique(x$Date))))
{
  y = subset(DT, x$Date == unique(x$Date)[k])
  write.csv(y,paste("~/Desktop/Jans Own/",as.character(unique(x$Date)[k]),".csv",sep = ''))
}
brokers = data.frame(ftable(DT$Buyer))$Var1[data.frame(ftable(DT$Buyer))$Freq > 100]
brokers = as.numeric(as.character(brokers))



DDR <- 0
for(w in c(1:length(unique(DT$Date))))
{
print("a")
y = subset(DT, DT$Date == unique(DT$Date)[w])
zzz = matrix(0, nrow = length(brokers), ncol = length(unique(DT$Sector)) +2)
zzz[,1] = brokers

for(j in c(1:length(brokers)))
{
  for(l in c(2:16))
  {
    zzz[j,l] = sum(y$value[y$Buyer == zzz[j,1] & y$Sector == (l-2)])
  }
}
print("b")
z = apply(zzz[,2:dim(zzz)[2]],1,sum)
zzz[,2:dim(zzz)[2]] = zzz[,2:dim(zzz)[2]]/z
zzz = data.frame(zzz)
rownames(zzz) = zzz[,1]
zzz = zzz[,-1]
zzz[is.na(zzz)] <- 0
colnames(zzz) = 1:dim(zzz)[2]
print("c")
if(w == 1)
{
getit <- 0
getit <- list()
geter <- list()
clust = kmeans(zzz,4,iter.max = 20, nstart = 20)
getit[[1]]<- clust$centers
geter[[1]] <- zzz
} else
{
  clust = kmeans(zzz, centers = clust$centers,iter.max = 30, nstart = 20)
  getit[[length(getit)+1]] <- clust$centers
  geter[[length(geter)+1]] <- zzz
}
print(paste(w,"d"))
DDR = data.frame(DDR,clust$cluster)
}
DDR = DDR[,-1]

16,17,18


for(t in c(1:length(getit)))
{
  if(t==1)
  {
  plot(0,0,xlim = c(0,1), ylim = c(0,1), cex = 0, xlab = "Diversified Industries", ylab = "Oil & Gas", main = "Sectors")
  } 
  tfc = getit[[t]]
  tfc = data.frame(tfc)
  tfc = tfc[3,]
  #par(new = T)
  points(x = tfc[,5], y = tfc[,11], col = t, pch = 22:25)
  #if((t+2)%%3 == 0)
  #{
    text(x = tfc[,5], y = tfc[,11], labels = t, pos = 3, cex = 0.5)
  #}
}

d <- dist(DDR,method = "euclidean")
fit <- hclust(d, method = "complete")
plot(fit)
x = data.frame(DDR,brokers)
inter = subset(x,x$brokers == 90 | x$brokers == 95)
inter = t(inter[,-c(dim(inter)[2])])



lol = unique(DT$Date)



f <-0
for(k in c(16:18))
{
  k = 16
  ttt = geter[[k]]
  ttt = data.frame(ttt,brokers)
  ttt = subset(ttt,ttt$brokers == 90 | ttt$brokers == 95)
  ttt = t(ttt[,-c(dim(ttt)[2])])
  colnames(ttt) = c('90','95')
 for(j in c(6,10))
 {
   print(paste('Sec: ',j,'90: ',ttt[j,1],'95: ',ttt[j,2], 'on day: ', k))
 }
   
}



for(k in c(16:18))
{
  #png(filename = paste("~/Desktop/TMX Images/",k,'M&E','.png'))
  k = 1
  zzz = geter[[k]]
  zzz[,"clust"] = DDR[,k]
  zzz = zzz[,c(6,10,dim(zzz)[2])]
  zzz = data.frame(zzz,brokers)
  tryt = subset(zzz,zzz$brokers %in% c(90,95))
  tryt
  plot(x = zzz$X6[zzz$brokers != 90 & zzz$brokers != 95], y = zzz$X10[zzz$brokers != 90 & zzz$brokers != 95], pch = c(16,17,18,19)[as.numeric(zzz$clust)[zzz$brokers != 90 & zzz$brokers != 95]], col = c("red","green","blue","orange")[as.numeric(zzz$clust)[zzz$brokers != 90 & zzz$brokers != 95]], xlim = c(0,1), ylim =c(0,1), xlab = 'ETP', ylab = "Mining", main = paste("Date:",lol[k]))
  #text()
  points(x = getit[[k]][,6], y = getit[[k]][,10], col = c("red","green","blue","orange"), cex = 4, pch = "+", font = 4)
  text(x = getit[[k]][,6], y = getit[[k]][,10],labels = rownames(getit[[length(getit)]]), pos = 3, cex = 1.2, pch = 2)
  points(x = zzz$X6[zzz$brokers == 90 | zzz$brokers == 95], y = zzz$X10[zzz$brokers == 90 | zzz$brokers == 95], pch = c(16,17,18,19)[as.numeric(zzz$clust)[zzz$brokers == 90 | zzz$brokers == 95]], col = c("red","green","blue","orange")[as.numeric(zzz$clust)[zzz$brokers == 90 | zzz$brokers == 95]], font = 2, cex = 1.5)
  text(x = zzz$X6[zzz$brokers == 90 | zzz$brokers == 95], y = zzz$X10[zzz$brokers == 90 | zzz$brokers == 95], labels = zzz$brokers[zzz$brokers == 90 | zzz$brokers == 95], pos =3, pch =2, cex = 1.2)
  abline(1,-1)
  dev.off()
  #Sys.sleep(5)
}

five <-0 
zer <- 0

for(k in c(11:13))
{
 
  zzz = geter[[k]]
  zzz = data.frame(zzz,brokers)
  tryt = subset(zzz,zzz$brokers %in% c(95))
  tryt = t(tryt[,-dim(tryt)[2]])
  
  zer = data.frame(zer,tryt)
  if(dim(zer)[2] == 4)
  {
    zer = zer[,-1]
  }
}

colnames(zer) = c('a','b','c')
zer[,"delta1"] = abs(zer[,2] - zer[,1])
zer[,"delta2"] = abs(zer[,3] - zer[,2])
zer[,"delta1"] = zer$delta1/sum(zer$delta1)
zer[,"delta2"] = zer$delta2/sum(zer$delta2)
j=4
for(k in c(16:17))
{
print(data.frame((abs(getit[[k]][j,] - getit[[k+1]][j,])/sum(abs(getit[[k]][j,] - getit[[k+1]][j,])))))
print(k)
}


zer <- 0

for(k in c(16:18))
{
  
  zzz = geter[[k]]
  zzz = data.frame(zzz,brokers)
  tryt = subset(zzz,zzz$brokers %in% c(90))
  tryt = t(tryt[,-dim(tryt)[2]])
  
  zer = data.frame(zer,tryt)
  if(dim(zer)[2] == 4)
  {
    zer = zer[,-1]
  }
}

colnames(zer) = c('a','b','c')
zer[,"delta1"] = abs(zer[,2] - zer[,1])
zer[,"delta2"] = abs(zer[,3] - zer[,2])
zer[,"delta1"] = zer$delta1/sum(zer$delta1)
zer[,"delta2"] = zer$delta2/sum(zer$delta2)
zer[,c(4,5)]
for(l in c(1:2))
{
  for(j in c(11,7))
  {
    print(paste('day: ',l, 'sec: ',j,(zer[j,l+1] - zer[j,l])/zer[j,l]),sep = '')
  }  
}

for(l in c(11:12))
{
  for(j in c(11,7))
  {
  print(paste('day:',l,'  sec:',j,(getit[[l+1]][3,j] - getit[[l]][3,j])/ getit[[l]][3,j],sep = ' '))
  }
}


for(l in c(11:12))
{
  for(j in c(11,7))
  {
    print(paste('day:',l,'  sec:',j,(getit[[l+1]][3,j] - getit[[l]][3,j])/ getit[[l]][3,j],sep = ' '))
  }
}

Osuna = subset(DT, DT$Buyer == 95 & DT$Sector == 10 & DT$Date == unique(DT$Date)[17])
x = unique(Osuna$Ticker)
y<-0
for(l in c(1:length(x)))
{
  y[l] = sum(Osuna$value[Osuna$Ticker == x[l]])
}
