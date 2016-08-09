
##########################################################
cave <- function(x)
{
  if(is.na(metrics[metrics$Firm == x,"buys"]) | is.na(metrics[metrics$Firm == x,"Sells"]))
  {
    return(0)
  }
  suberoo = subset(tr,tr$Buyer ==x)
  r = mean(as.numeric(apply(data.frame(suberoo$Ticker),1,function(x) freq_S[as.character(freq_S$Var1)==x,2])))
  
  return(r)
}
##########################################################
cove <- function(x) 
{
  return(freq_S[as.character(freq_S$Var1)==x,2])
}
##########################################################
selling <- function(x)
{
  return(selldata[as.numeric(as.character(selldata$Var1)) == x,2])
}
##########################################################
vace <- function(x)
{
  return(buydata[as.numeric(as.character(buydata$Var1)) == x,2])
}
##########################################################
numa <-function(x)
{
  roundx/2
}