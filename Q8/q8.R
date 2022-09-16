#Loading required packages
library("tseries")
library("zoo")
library("quantmod")
library("PerformanceAnalytics")
library(corrplot)
options(digits=4)
rm(list=ls(all=TRUE))

###############################################
#Answer to Q9: Fund Screening and Performance Analysis
###########################################
#Specify date and download monthly data [We dropped one fund "JAVLX", no data]
dateStart <-as.Date("2016-10-01")              
dateEnd <- as.Date("2021-10-01")
myassets=c("SPY","AGRYX","CAAPX","BUFSX","FMAGX","GCMAX","NVSOX","PGSGX","PRDGX","PSLAX","VASVX")
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

index(mydata) = as.yearmon(index(mydata))

##############
#Calculating different measures
fundsr=Return.calculate(mydata, method="discrete")*100
fundsr=na.omit(fundsr)
Mean_return=matrix(colMeans(fundsr),nrow=ncol(fundsr), ncol = 1)
Geo_mean=matrix(mean.geometric(fundsr/100)*100,nrow=ncol(fundsr), ncol = 1)
Cum_returns=matrix(Return.cumulative(fundsr,geometric=FALSE),nrow=ncol(fundsr), ncol = 1)
all_returns= cbind(Mean_return,Geo_mean,Cum_returns)
colnames(all_returns)=c("Average Returns","Geometric Mean","Cumulative Returns")

RF_mean=0.0016
nfunds=ncol(fundsr)
SRmat=matrix(0,nrow=nfunds,ncol=1)
SKmat=matrix(0,nrow=nfunds,ncol=1)
Kmat=matrix(0,nrow=nfunds,ncol=1)
ASRmat=matrix(0,nrow=nfunds,ncol=1)
TEmat=matrix(0,nrow=nfunds,ncol=1)
IRmat=matrix(0,nrow=nfunds,ncol=1)
AIRmat=matrix(0,nrow=nfunds,ncol=1)
TRmat=matrix(0,nrow=nfunds,ncol=1)
SRmat=matrix(0,nrow=nfunds,ncol=1)
ADmat=matrix(0,nrow=nfunds,ncol=1)
PDRmat=matrix(0,nrow=nfunds,ncol=1)
VaR.matg=matrix(0,nrow=nfunds,ncol=1)
VaR.math=matrix(0,nrow=nfunds,ncol=1)
CVaR.matg=matrix(0,nrow=nfunds,ncol=1)
CVaR.math=matrix(0,nrow=nfunds,ncol=1)
BMassets=1
for (i in 1:ncol(fundsr)){
  SRmat[i,]=(mean(fundsr[,i])-RF_mean)/sd(fundsr[,i])
  skness=length(fundsr[,i])/((length(fundsr[,i])-1)*(length(fundsr[,i])-2))*sum(((fundsr[,i]-mean(fundsr[,i]))/sd(fundsr[,i]))^3)
  kness=((length(fundsr[,i])*(length(fundsr[,i])+1))/((length(fundsr[,i])-1)*(length(fundsr[,i])-2)*(length(fundsr[,i])-3))*sum(((fundsr[,i]-mean(fundsr[,i]))/sd(fundsr[,i]))^4))-(3*(length(fundsr[,i])-1)^2)/((length(fundsr[,i])-2)*(length(fundsr[,i])-3))
  SKmat[i,]=skness
  Kmat[i,]=kness
  ASRmat[i,]=SRmat[i,]*(1+skness/6*SRmat[i,]-kness/24*SRmat[i,]^2)
  TEmat[i,]=sd(fundsr[,i]-fundsr[,BMassets])
  IRmat[i,]=(mean(fundsr[,i])-mean(fundsr[,BMassets]))/TEmat[i,]
  AIRmat[i,]=IRmat[i,]*(1+skness/6*IRmat[i,]-kness/24*IRmat[i,]^2)
  TRmat[i,]=(mean(fundsr[,i])-RF_mean)/(cov(fundsr[,i], fundsr[,BMassets])/var(fundsr[,BMassets]))
  SRmat[i,]=SortinoRatio(fundsr[,i],MAR =RF_mean)
  ADmat[i,]=sum((fundsr[,i]<0)*1*fundsr[,i])/sum((fundsr[,i]<0)*1)
  PDRmat[i,]=sqrt(sum((fundsr[,i]<0)*1*fundsr[,i]^2)/(length(fundsr[,i])-1))
  VaR.math[i,]=100*VaR(fundsr[,i]/100, p = 0.95, method = "historical")
  VaR.matg[i,]=100*VaR(fundsr[,i]/100, p = 0.95, method = "gaussian")
  CVaR.math[i,]=100*ES(fundsr[,i]/100, p=.95, method="historical")
  CVaR.matg[i,]=100*ES(fundsr[,i]/100, p=.95, method="gaussian")
}

SimpleMeasures=cbind(SKmat,Kmat,SRmat,ASRmat,TEmat,IRmat,AIRmat,TRmat)
colnames(SimpleMeasures)=c("Sample Skewness","Kurtosis","Sharp Ratios","Adj. Sharp Ratios","Treacking Error","Information Ratios",
                           "Adj. Information Ratios", "Treynor Ratios")
OtherMeasures=cbind(ADmat,PDRmat,VaR.math,VaR.matg,CVaR.math,CVaR.matg)
colnames(OtherMeasures)=c("Average Drawdown","Pure downside risk","Historical 95% VaR","Normal 95% VaR",
                          "Historical 95% CVaR","Normal 95% CVaR")
AllMeasures=cbind(all_returns,SimpleMeasures,OtherMeasures)
rownames(AllMeasures)=myassets
AllMeasuresfinal=t(AllMeasures)
AllMeasuresfinal

#Export AllMeasuresfinal and make a nice table to include in your word file. 
write.csv(AllMeasuresfinal,"AllMeasuresfinal.csv")

