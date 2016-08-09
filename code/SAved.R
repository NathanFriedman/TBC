######## Setting pointers
datafile = list(`20150527trades`, `20150717trades`,`20150806trades`,`20150824trades`,`20151015trades`,`20151105trades`,`20151112trades`,`20151204trades`,`20151231trades`) 
Rhianna = list()
day_start = min(tr$Time_P)
day_end = day_start + 6.5*60*60
########## Loop to extra
par(mfrow = c(2,2))
par(mar = c(1,1,1,1))

for (k in c(1:4))
{
  tr <- 0
  tr = datafile[[k]]
  names(tr) = c("Ticker","Date","Time","Price","Volume","Buyer","Seller","What")
  tr$Ticker = as.character(tr$Ticker)
  tr$Time = as.character(tr$Time)
  tr[,"Time_P"] = as.POSIXct(strptime(tr[,"Time"],"%H:%M:%OS"))
  EOD = subset(tr, tr$Time_P >= day_end - (60*30) & tr$Time_P <= day_end)
  
  hist(EOD$Time_P, breaks = c(day_end - (60*30) + 18*(0:100)), main = paste("Day", k))
  abline(v= day_end - (20*60))
}
day_end - (60*30) + 18*(0:100)
