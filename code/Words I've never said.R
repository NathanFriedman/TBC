datafile = list(`20150527trades`,`e20150717trades`, `20150806trades`,`20150824trades`,`20151015trades`,`20151015trades`,`20151105trades`,`20151112trades`,`20151204trades`,`20151231trades`)
Boston <- read.csv("~/Desktop/Boston.csv")
Boston = data.frame(as.character(Boston$Root.Ticker), as.character(Boston$Sector),Boston$X.QMV.C...31.May.2016.,Boston$TSX..Venture..Grad)
colnames(Boston) = c("Ticker","Sector","QMV","VG")
head(Boston,2)
Boston$QMV = as.numeric(gsub(",","",(gsub(" ","",as.character(Boston$QMV)))))
Boston$Sector = gsub(" ","", Boston$Sector)
head(Boston,2)

Boston$Ticker = as.character(Boston$Ticker)
Boston$Sector = as.character(Boston$Sector)
    tr = datafile[[1]]
    #tr = as.data.frame(datafile[[floor((j+1)/2)]])
    #names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
    #tr$Ticker = as.character(tr$Ticker)
    #tr$Time = as.character(tr$Time)
    #tr[,"Time_P"] = as.POSIXct(strptime(tr[,"Time"],"%H:%M:%OS"))
    #day_start = min(tr$Time_P)
    #day_end = day_start + 6.5*60*60
    #freq_S = data.frame(ftable(tr$Ticker))
    #freq_S[,1]=as.character(freq_S[,1])
    #tr[,"Liq"] = apply(data.frame(tr$Ticker),1, function(x) freq_S[freq_S$Var1 == x,2])
   # tr[,"Value"] = (tr$Price/1000)*tr$Volume
  
tulo <- function(x)
{
  if(all.equal(which(strsplit(x,"")[[1]] == "."),integer()) == TRUE)
  {
    z = nchar(x)
  } else
  {
    z = which(strsplit(x,"")[[1]] == ".")-1
  }
  return(substr(x,1,z))
}  

#for(k in c(4:length(datafile)))
#{
 
  tr= datafile[[k]]
 names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What","X","Sector")
  tr$Ticker = as.character(tr$Ticker)
 tr$Ticker = apply(data.frame(tr$Ticker),1,tulo)
  tr[,"Sec"] =  apply(data.frame(tr$Ticker),1,function(x) x %in% Boston$Ticker)
  q = subset(tr,tr$Sec)
  lol = subset(tr,!tr$Sec)
  x<- 0
  for(z in c(1:dim(q)[1]))
  {
    x[z] = Boston[which(Boston$Ticker ==q$Ticker[z]),2]
  }
  q = data.frame(q,x)
  lol[,"Sector"] = "Unknown"
  colnames(lol) = colnames(q)
  q = rbind(q,lol)
  write.csv(q,paste("~/Desktop/Data/e20150527trades")
}
tr$Ticker = gsub(" ","",as.character(tr$Ticker))
   
    tulo <- function(x)
    {
    if(all.equal(which(strsplit(x,"")[[1]] == "."),integer()) == TRUE)
    {
      z = nchar(x)
    } else
    {
      z = which(strsplit(x,"")[[1]] == ".")-1
    }
   return(substr(x,1,z))
    }
     
   tr$Ticker = apply(data.frame(tr$Ticker),1,tulo)
   tr[,"Sec"] =  apply(data.frame(tr$Ticker),1,function(x) x %in% Boston$Ticker)
   q = subset(tr,tr$Sec)
   lol = subset(tr,!tr$Sec)
   lol[,"Sector"] = "Unknown"
   colnames(lol) = colnames(q)
   q = rbind(q,lol)
   x <- 0
  system.time(for(j in c(1:dim(q)[1]))
   {
     x[j] = Boston[which(Boston$Ticker ==q$Ticker[j]),2]
   })
   z = dim(q)[1]
  y <- 0
  for(j in c(647927:z))
   {
     y[j] = ifelse(all.equal(Boston[which(Boston$Ticker ==q$Ticker[j]),3],numeric()),0,Boston[which(Boston$Ticker ==q$Ticker[j]),3])
   }
  
   list.files("~/Desktop/Data")
   q = data.frame(q,y)
  
   brokerinfo <- read.csv('~/Desktop/brokerinfo.csv', header = FALSE)
   metrics <- 0
   metrics = data.frame(matrix(0, nrow = length(brokerinfo[,1]), ncol = 18))
   metrics[,1] = brokerinfo[,1]
   colnames(metrics) = append("Firm",as.character(unique(q$Sector)))
   
   for(s in c(1:length(brokerinfo[,1])))
   {
      metrics[s,2:18] = data.frame(ftable(q$Sector[q$Buyer == metrics[s,1]]))$Freq
   }
   rownames(metrics) = metrics$Firm
   metrics = metrics[,-1]
  
   metrics[,"Total"] = apply(metrics,1,sum)
   for(j in c(1:length(metrics[,1])))
   {
     if(metrics$Total[j])
     {
       metrics[j,] = metrics[j,]/metrics$Total[j]
     } 
   }
   metrics = metrics[,-dim(metrics)[2]]
   
   zzz = data.frame(matrix(0,length(brokerinfo$V1),1))
   zzz[,1] = brokerinfo[,1]
   
   zzz[,"Buy_QMV"] = apply(data.frame(zzz[,1]),1, function(x) mean(q$y[q$Buyer == x], na.rm = T))
   zzz[,"Sell_QMV"] = apply(data.frame(zzz[,1]),1, function(x) mean(q$y[q$Seller == x], na.rm = T))
   zzz$Buy_QMV[is.nan(zzz$Buy_QMV)]<- 0
   zzz$Sell_QMV[is.nan(zzz$Sell_QMV)] <-0
   Selmetrics <- 0
   Selmetrics = data.frame(matrix(0, nrow = length(brokerinfo[,1]), ncol = 18))
   Selmetrics[,1] = brokerinfo[,1]
   colnames(Selmetrics) = append("Firm",as.character(unique(q$Sector)))
   
   for(s in c(1:length(brokerinfo[,1])))
   {
     Selmetrics[s,2:18] = data.frame(ftable(q$Sector[q$Seller == Selmetrics[s,1]]))$Freq
   }
   rownames(Selmetrics) = Selmetrics$Firm
   Selmetrics = Selmetrics[,-1]
   
   Selmetrics[,"Total"] = apply(Selmetrics,1,sum)
   for(j in c(1:length(Selmetrics[,1])))
   {
     if(Selmetrics$Total[j])
     {
       Selmetrics[j,] = Selmetrics[j,]/Selmetrics$Total[j]
     } 
   }
   Selmetrics = Selmetrics[,-dim(Selmetrics)[2]]
   
   zzz[,2] = (zzz[,2] - min(zzz[,2]))/(max(zzz[,2]) - min(zzz[,2]))
   zzz[,3] = (zzz[,3] - min(zzz[,3]))/(max(zzz[,3]) - min(zzz[,3]))
              
   
  
   working = data.frame(metrics,zzz[,2:3]*2,Selmetrics)
   active = subset(working, working$Buy_QMV != 0 & working$Sell_QMV !=0)
   
   
   y =  kmeans(active,4)
   active = data.frame(active,y$cluster)
   
   par(mfrow = c(2,2))
   par(mar = c(1,1,1,1))
   for(k in c(1:4))
   {
     clusterk = subset(active,active$y.cluster == k)
     clusterk = clusterk[,-dim(clusterk)[2]]
     d <- dist(clusterk,method = "euclidean")
     fit <- hclust(d, method = "ward.D")
     plot(fit, main = paste("Sector",k,sep = " "))
   }
   
   
   
  
   cluster1 = subset(active2, active2$y.cluster == 1)
   cluster2 = subset(active2, active2$y.cluster == 2)
   cluster3 = subset(active2, active2$y.cluster == 3)
   cluster4 = subset(active2, active2$y.cluster == 4)
   cluster1 = cluster1[,-dim(cluster1)[2]]
   cluster2 = cluster2[,-dim(cluster1)[2]]
   cluster3 = cluster3[,-dim(cluster1)[2]]
   cluster4 = cluster4[,-dim(cluster1)[2]]
   
   d = dist(cluster2, method = "euclidean")
   fit = hclust(d, method = "ward.D")
   plot(fit, main = 'Cluster 2')
   names(working)
   metrics = metrics[,-(18:19)]
   active3 = active[,-(18:19)]
   
   d = dist(active3, method = "euclidean")
   fit = hclust(d, method = "ward.D")
   plot(fit, main = 'Sector Clustering')
   
   y =  kmeans(active3,4)
   active3 = data.frame(active3,y$cluster)
   
   par(mfrow = c(2,2))
   par(mar = c(1,1,1,1))
   for(k in c(1:4))
   {
     clusterk = subset(active3,active3$y.cluster == k)
     clusterk = clusterk[,-dim(clusterk)[2]]
     d <- dist(clusterk,method = "euclidean")
     fit <- hclust(d, method = "ward.D")
     plot(fit, main = paste("Sector",k,sep = " "))
   }
   
   
   
   d = dist(cluster4, method = "euclidean")
   fit = hclust(d, method = "ward.D")
   plot(fit, main = 'Cluster4')
   
   
   rownames(zzz) = zzz$matrix.0..length.brokerinfo.V1...1.
   zzz = zzz[,-1]
   zzz_a = subset(zzz, zzz$Buy_QMV >0 & zzz$Sell_QMV > 0)
   d <- dist(zzz_a,method = "euclidean")
   fit = hclust(d, method = "ward.D")
   plot(fit, main = 'QMV')
   
   y = kmeans(zzz_a,4)
   zzz_b = data.frame(zzz_a,y$cluster)
   par(mfrow = c(2,2))
   par(mar = c(1,1,1,1))
   for(k in c(1:4))
   {
     clusterk = subset(zzz_b,zzz_b$y.cluster == k)
     clusterk = clusterk[,-dim(clusterk)[2]]
     d <- dist(clusterk,method = "euclidean")
     fit <- hclust(d, method = "ward.D")
     plot(fit, main = paste("Sector",k,sep = " "))
   }
   
   x = matrix(0,length(unique(Boston$Sector)),3)
   
   for(k in c(1:length(unique(Boston$Sector))))
   {
   x[k,2] = mean(Boston$QMV[Boston$Sector == unique(Boston$Sector)[k]],na.rm = T)
   x[k,1] = unique(Boston$Sector)[k]
   x[k,3] = median(Boston$QMV[Boston$Sector == unique(Boston$Sector)[k]], na.rm = T)
   }
   data.frame(x) 
   anova()