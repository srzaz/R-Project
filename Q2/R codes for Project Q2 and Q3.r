#Loading required packages
library("tseries")
library("zoo")
library("quantmod")
library("PerformanceAnalytics")

options(digits=4)
rm(list=ls(all=TRUE))

###############################################
#Downloading data. The data is stored as mydata
################################################
dateStart =as.Date("2009-03-01")              
dateEnd = as.Date("2021-08-31")
myassets=c("NFLX","AMZN","NVDA");
nassets = length(myassets)
mydata=get.hist.quote(instrument = myassets[1], start = dateStart, end = dateEnd, quote = "AdjClose",
                      origin="1970-01-01",compression = "m", retclass = "zoo")
dimnames(mydata)[[2]]= as.character(myassets[1])
for (i in 2:nassets) {
  cat("Downloading ", i, " out of ", nassets , "\n")
  tempdata = try(x <- get.hist.quote(instrument = myassets[i],start = dateStart, end = dateEnd, quote = "AdjClose",
                                     origin="1970-01-01",compression = "m", retclass = "zoo",quiet = T))
  if(class(tempdata) == "try-error") {
    next
  }
  else {
    dimnames(x)[[2]] = as.character(myassets[i])
    mydata = merge(mydata, x)                      
  } }

###############################################
#Answering Q2
################################################
datar=na.omit(Return.calculate(mydata, method="simple"))
#Answer to Q2(i)
firstobs=drop(coredata(first(mydata)))
dollorindex=t(apply(mydata,1, function(x) x/firstobs))
chart.TimeSeries(dollorindex,legend.loc = "topleft",main="Dollar Index")

#Answer to Q2(ii)
chart.CumReturns(datar,legend.loc = "topleft",main="Cumulative Returns")

#Answer to how much would be the value of 10k
10000*drop(coredata(last(dollorindex)))

###############################################
#Answering Q3
################################################
datar=na.omit(Return.calculate(mydata, method="log"))
rbind(table.Stats(datar),VaR(datar, p = 0.95, method = "historical"),ES(datar, p = 0.95, method = "historical"))