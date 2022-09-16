#Loading required packages

library("tseries")
library("zoo")
library("xts")
library("Quandl")
library("quantmod")
library("PerformanceAnalytics")
rm(list=ls())

###############################
#run the following function, it accepts univariate zoo time series
mysortingf=function(xx){
  tickerSymbol=colnames(xx)
  nobs=length(xx)
  prices=coredata(xx)
  fobs=coredata(xx[1])
  lobs=coredata(xx[nobs])
  stdate=time(first(xx))
  enddata=time(last(xx))
  pfirst=coredata(fobs,use.names = FALSE)
  minv=min(xx)
  maxv=max(xx)
  plast=coredata(lobs)
  hprret=100*(plast[1]/pfirst[1]-1)
  rets=coredata(na.omit(Return.calculate(xx, method="simple")))
  cumret=sum(rets)
  lweekret=100*sum(tail(rets,5))
  lmonthret=100*sum(tail(rets,22))
if (nobs>50){
  mv50d=mean(prices[(nobs-50):(nobs-1)])
  pabove50dmv=100*(plast/mv50d-1)
}
else {
mv50d="NA"
pabove50dmv="NA"
}
if (nobs>200){
  mv200d=mean(prices[(nobs-200):(nobs-1)])
   pabove200dmv=100*(plast/mv200d-1)
  
}
else {
  mv200d="NA" 
  pabove200dmv="NA"
}

if (nobs>1250){
  mv5yr=mean(prices[(nobs-1250):(nobs-1)])
  pabove5yrmv=100*(plast/mv5yr-1)
}
else {
mv5yr="NA"
pabove5yrmv="NA"
}

  myfrow=data.frame(tickerSymbol,stdate,enddata,nobs,pfirst,minv,maxv,plast,hprret,cumret,lweekret,
		lmonthret,mv50d,pabove50dmv,mv200d,pabove200dmv,mv5yr,pabove5yrmv)
  colnames(myfrow)=c("tickerSymbol","StartDate","EndDate","Nobs","Beginning Prind",
                      "Min Price","Max Price","Last price","HPR","Cum Ret","Lweek Ret",
                      "Last Month Ret","MA50D","Pabove50dmav","MA200D","Pabove200mav","MA5Yr","PaboveMA5Yr")
  return(myfrow)  
}


#############################################
#Specify date, frequency and download data (I chose 2015 as startind date instead of 2016. Please do the same)
dateStart =as.Date("2015-09-01")              
dateEnd = as.Date("2021-09-15")
myfrequency="d"

myassets=c("AAPL","ADBE","ADI","ADP","ADSK","AEP","ALGN","AMAT","AMD","AMGN","AMZN","ANSS",
		"ASML","ATVI","AVGO","BIDU","BIIB","BKNG","CDNS","CDW","CERN","CHKP","CHTR",
		"CMCSA","COST","CPRT","CRWD","CSCO","CSX","CTAS","CTSH","DLTR","DOCU","DXCM",
		"EA","EBAY","EXC","FAST","FB","FISV","FOX","FOXA","GILD","GOOG","GOOGL","HON",
		"IDXX","ILMN","INCY","INTC","INTU","ISRG","JD","KDP","KHC","KLAC","LRCX","LULU",
		"MAR","MCHP","MDLZ","MELI","MNST","MRNA","MRVL","MSFT","MTCH","MU","NFLX","NTES",
		"NVDA","NXPI","OKTA","ORLY","PAYX","PCAR","PDD","PEP","PTON","PYPL","QCOM","REGN",
		"ROST","SBUX","SGEN","SIRI","SNPS","SPLK","SWKS","TCOM","TEAM","TMUS","TSLA","TXN",
		"VRSK","VRSN","VRTX","WBA","WDAY","XEL","XLNX","ZM")


nassets = length(myassets)
mydata=get.hist.quote(instrument = myassets[1], start = dateStart, end = dateEnd, quote = "AdjClose",
                      origin="1970-01-01",compression = myfrequency, retclass = "zoo")
dimnames(mydata)[[2]]= as.character(myassets[1])
mystats=mysortingf(mydata)           
for (i in 2:nassets) {
  cat("Downloading ", i, " out of ", nassets , "\n")
  tempdata = try(x <- get.hist.quote(instrument = myassets[i],start = dateStart, end = dateEnd, quote = "AdjClose",
                                     origin="1970-01-01",compression = myfrequency, retclass = "zoo",quiet = T))
  if(class(tempdata) == "try-error") {
    next
  }
  else {
    dimnames(x)[[2]] = as.character(myassets[i])
    mystats = rbind(mystats, mysortingf(x))                      
  } }


########################
#All statistics are stored in mystats . Export it to csv file and use it to answer questions asked
write.csv(mystats,file = "mystats.csv")
