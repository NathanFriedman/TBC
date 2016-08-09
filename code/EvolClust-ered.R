
datafile = list(`20150527trades`, `20150717trades`,`20150806trades`,`20150824trades`,`20151015trades`,`20151105trades`,`20151112trades`,`20151204trades`,`20151231trades`) 
Rhianna = list()
space <- data.frame(matrix(0,nrow = 222,ncol =5))
names(space) = c("buys", "liq_Buy", "Sells", "Liq_sells")

for(j in c(1:length(datafile)))
{
  tr = datafile[[j]]
  if(!(j-1))
  {
    super = tr
  } else{
    super = rbind(super, tr)
  }
}
tr = super
  

  
  names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
  tr$Ticker = as.character(tr$Ticker)
  tr$Time = as.character(tr$Time)
  tr[,"Time_P"] = as.POSIXct(strptime(tr[,"Time"],"%H:%M:%OS"))
  day_start = min(tr$Time_P)
  day_end = day_start + 6.5*60*60
  freq_S = data.frame(ftable(tr$Ticker))
  freq_S[,1]=as.character(freq_S[,1])
  tr[,"Liq"] = apply(data.frame(tr$Ticker),1, function(x) freq_S[freq_S$Var1 == x,2])
  tr[,"Value"] = (tr$Price/100)*tr$Volume
  k=2
  cp = 0.1
  
  for (m in (1:13*3))
  {
  ideal = subset(tr, tr$Time_P >= day_start + (m-1)*(30*60/3) & tr$Time_P < day_start + (m)*(30*60/3) +1)
  
  #metric 1
  buydata = data.frame(ftable(ideal$Buyer))
  #metric 2
  selldata = data.frame(ftable(ideal$Seller))
  
  metrics <- 0
  metrics = data.frame(matrix(0, nrow = 222, ncol = 6))
  metrics[,1] = 1:222
  
  names(metrics) = c("Firm", "buys", "liq_Buy", "Sells", "Liq_sells","Value")
 
  metrics[,"buys"] = apply(data.frame(metrics$Firm),1, vace) %>% as.numeric()
  metrics[,"Sells"] = apply(data.frame(metrics$Firm),1, selling) %>% as.numeric()
  metrics[,"liq_Buy"] = apply(data.frame(metrics$Firm),1, function (x) mean(ideal$Liq[ideal$Buyer == x])) %>% as.numeric()
  metrics[,"Liq_sells"] = apply(data.frame(metrics$Firm),1, function (x) mean(ideal$Liq[ideal$Seller == x])) %>% as.numeric()
  metrics[,"Value"] = apply(data.frame(metrics$Firm),1,function (x) mean(ideal$Value[ideal$Buyer == x | ideal$Seller ==x]))
  metrics[is.na(metrics)] <- 0
  
  
  space = data.frame(metrics)


  
names(space) = c("Firm", "buys", "liq_Buy", "Sells", "Liq_sells","Value")

rownames(space) = space$Firm
space = space[,-1]


no_action = subset(space, space$buys ==0 & space$Sells == 0)
no_buy = subset(space, space$buys ==0 & space$Sells >0)
no_sell = subset(space, space$Sells == 0 & space$buys >0) 

space = subset(space, space$buys >0 & space$Sells >0)

no_buy = no_buy[,3:5]
no_sell = no_sell[,-(3:4)]
no_buy = log(no_buy)
no_sell = log(no_sell)





for(q in c(1:3))
{
  no_buy[,q] = (no_buy[,q] - mean(no_buy[,q]))/sd(no_buy[,q])
}

for(q in c(1:3))
{
  no_sell[,q] = (no_sell[,q] - mean(no_sell[,q]))/sd(no_sell[,q])
}

for(q in c(1:5))
{
  space[,q] = sqrt((space[,q] - min(space[,q]))/(max(space[,q] - min(space[,q]))))
}
for(q in c(1:5))
{
  space[,q] = (space[,q] - mean(space[,q]))/(sd(space[,q]))
}

temp = space[1:17,]
if(m ==1)
{
  cl = kmeans(temp, nstart = 10, 2)
  clusters = cl$cluster
  clusters = as.data.frame(t(clusters))
  colnames(clusters) = colnames(t(temp))
  clusters.m = clusters
} 
if(m!= 1) 
  {
  cl.old = cl
  cl.old$cluster = fitted(cl.old, method = "classes")
  n_old = cl.old$size

  if(class(try(kmeans(temp,centers = cl.old$centers,nstart = 5,k)))=="kmeans")
  {
    cl.new = kmeans(temp, centers = cl.old$centers,k)
  }
  if(class(try(kmeans(temp,centers = cl.old$centers,k))) !="kmeans")
  {
    cl.new = kmeans(temp, centers = cl.old$centers)
  }
  
  cl.new$cluster = fitted(cl.new, method = "classes")
  n_new = cl.new$size
  gamma = n_new / (n_new + n_old)
 cl.evol.centers = cp*(1- gamma)*cl.old$centers + (1-cp)*gamma*cl.new$centers
  
 
  cluster <- function(x, centers)
  {
     tmp <- sapply(seq_len(nrow(x)), function (i) apply(centers,1, function (v) sum((x[i,])-v)^2))
     max.col(-t(tmp))
  }
 
    cl.final.cluster = cluster(temp, cl.evol.centers)
 
    cl$centers = cl.evol.centers
 
    cl$size = as.vector(table(cluster(temp, cl.evol.centers)))
 
    clusters = cl.final.cluster
    clusters = as.data.frame(t(clusters))
 colnames(clusters) = colnames(t(temp))
 
 clusters.m = rbind.fill(clusters.m, clusters)
}
}

  

clusters.relab = as.matrix(clusters.m)
colnames(clusters.relab) = colnames(clusters.m)
clusters.relab[is.na(clusters.relab)] <-3
d <- dist(t(clusters.relab), method = "euclidean")
fit <- hclust(d, method = "ward.D")
plot(fit)
clusters.relab
