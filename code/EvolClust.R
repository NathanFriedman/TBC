
datafile = list()
datafile = list(`20150527trades`,`20150717trades`, `20150806trades`,`20150824trades`,`20151015trades`,`20151015trades`,`20151105trades`,`20151112trades`,`20151204trades`,`20151231trades`)

tr = datafile[[1]]  

for(j in c(1:8))
{
   print(j)
   if((j+1)%%2 ==0)
{
  tr = as.data.frame(datafile[[floor((j+1)/2)]])
  names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
  tr$Ticker = as.character(tr$Ticker)
  tr$Time = as.character(tr$Time)
  tr[,"Time_P"] = as.POSIXct(strptime(tr[,"Time"],"%H:%M:%OS"))
  day_start = min(tr$Time_P)
  day_end = day_start + 6.5*60*60
  freq_S = data.frame(ftable(tr$Ticker))
  freq_S[,1]=as.character(freq_S[,1])
  tr[,"Liq"] = apply(data.frame(tr$Ticker),1, function(x) freq_S[freq_S$Var1 == x,2])
  tr[,"Value"] = (tr$Price/1000)*tr$Volume
 }
  
  cp = 0.1
  k =2
  
  
  ideal = subset(tr, tr$Time_P >= day_start+((j+1)%%2)*3.25*60*60 & tr$Time_P < day_end - (j%%2)*3.25*60*60 +1)
  head(ideal)
  #metric 1
  buydata = data.frame(ftable(ideal$Buyer))
  #metric 2
  selldata = data.frame(ftable(ideal$Seller))
  
  metrics <- 0
  metrics = data.frame(matrix(0, nrow = length(brokerinfo[,1]), ncol = 6))
  metrics[,1] = brokerinfo[,1]
  
  names(metrics) = c("Firm", "buys", "liq_Buy", "Sells", "Liq_sells","Value")
 
  metrics[,"buys"] = apply(data.frame(metrics$Firm),1, vace) %>% as.numeric()
  metrics[,"Sells"] = apply(data.frame(metrics$Firm),1, selling) %>% as.numeric()
  metrics[,"liq_Buy"] = apply(data.frame(metrics$Firm),1, function (x) mean(ideal$Liq[ideal$Buyer == x])) %>% as.numeric()
  metrics[,"Liq_sells"] = apply(data.frame(metrics$Firm),1, function (x) mean(ideal$Liq[ideal$Seller == x])) %>% as.numeric()
  metrics[,"Value"] = apply(data.frame(metrics$Firm),1,function (x) mean(ideal$Value[ideal$Buyer == x | ideal$Seller ==x]))
  metrics[is.na(metrics)] <- 0
  metrics[metrics == 0] <- 0.0001
  
  space = data.frame(metrics)


  
names(space) = c("Firm", "buys", "liq_Buy", "Sells", "Liq_sells","Value")

rownames(space) = space$Firm
space = space[,-1]

space = log(space)


for(q in c(1:5))
{
  space[,q] = (space[,q] - mean(space[,q]))/sd(space[,q])
}

temp = space
if(j ==1)
{
  cl = kmeans(temp, centers = 4, iter.max = 11)
  clusters = cl$cluster
  clusters = as.data.frame(t(clusters))
  colnames(clusters) = colnames(t(temp))
  clusters.m = clusters
  center = cl$centers
} 
if(j!= 1) 
  {
  cl.old = cl
  cl.old$cluster = fitted(cl.old, method = "classes")
  n_old = cl.old$size

  if(class(try(kmeans(temp,centers = cl.old$centers, iter.max = 11)))=="kmeans")
  {
    cl.new = kmeans(temp, centers = cl.old$centers, iter.max = 11)
  }
  if(class(try(kmeans(temp,centers = cl.old$centers, iter.max = 11))) !="kmeans")
  {
    cl.new = kmeans(temp, centers = cl.old$centers, iter.max = 11)
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
#clusters.relab[is.na(clusters.relab)] <-4
#clusters.relab[clusters.relab==4] <-3
d <- dist(t(clusters.relab), method = "euclidean")
fit <- hclust(d, method = "ward.D")
plot(fit)

###########################################################################################

closer = clusters.relab[,apply(data.frame(clusters.relab),2,function (x) sum(x)) == 40]
colnames(closer)
tr = rbind(datafile[[1]], datafile[[length(datafile)]])
names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
tr$Ticker = as.character(tr$Ticker)

freq_S = data.frame(ftable(tr$Ticker))
ideal = subset(tr, tr$Buyer %in% as.numeric(colnames(closer))| tr$Seller %in% as.numeric(colnames(closer)))

freq_S[,1]=as.character(freq_S[,1])
ideal[,"Liq"] = apply(data.frame(ideal$Ticker),1, function(x) freq_S[freq_S$Var1 == x,2])
ideal[,"Value"] = (ideal$Price/100)*ideal$Volume


colnames(closer)


sped <- data.frame(matrix(0,nrow = length(colnames(closer)),ncol = 6))
names(sped) = names(metrics)
sped[,"Firm"] = as.numeric(colnames(closer))
buydata = data.frame(ftable(ideal$Buyer))
selldata = data.frame(ftable(ideal$Seller))

sped[,"buys"] = apply(data.frame(sped$Firm),1, vace) %>% as.numeric()
sped[,"Sells"] = apply(data.frame(sped$Firm),1, selling) %>% as.numeric()
sped[,"liq_Buy"] = apply(data.frame(sped$Firm),1, function (x) mean(ideal$Liq[ideal$Buyer == x])) %>% as.numeric()
sped[,"Liq_sells"] = apply(data.frame(sped$Firm),1, function (x) mean(ideal$Liq[ideal$Seller == x])) %>% as.numeric()
sped[,"Value"] = apply(data.frame(sped$Firm),1,function (x) mean(ideal$Value[ideal$Buyer == x | ideal$Seller ==x]))
sped[is.na(sped)] <- 0

tstm = sped

rownames(tstm) = tstm$Firm
tstm = tstm[,-1]
tstm = log(tstm)
for(lol in c(1:5))
{
  tstm[,lol] = (tstm[,lol] - mean(tstm[,lol]))/sd(tstm[,lol])
}

d <- dist(tstm, method = "euclidean")
fit = hclust(d, method = "ward.D")
plot(fit)


