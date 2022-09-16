#Loading Packages
library("zoo")
library("xts")
library("tseries")
library("quantmod")
library("PerformanceAnalytics")
options(digits=4)
rm(list = ls())
##############################
#Download Factor Data from Fama French Website
#Step 1: Specify five factor and download directory
#############################
ff5f_url="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
ffmomf_url="https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip"
ff5fdata=tempfile()
ffmomfdata=tempfile()
###################
#Step 2: DownLoad Data
download.file(ff5f_url,ff5fdata,quiet = TRUE)
download.file(ffmomf_url,ffmomfdata,quiet = TRUE)

###################
#Step 3: Load data
ff5fdata=read.csv(unzip(ff5fdata,"F-F_Research_Data_5_Factors_2x3.csv"),skip =3)
ffmomfdata=read.csv(unzip(ffmomfdata,"F-F_Momentum_Factor.CSV"),skip =13)
######################
#Step 4: Conver factor data to time series
#####################
#Five factors
stDate= as.Date("1963/07/01")
lastDate = as.Date(Sys.time())-120
dseq=seq(stDate, lastDate, by = "month")
ff5fdata.5f=apply(ff5fdata[1:length(dseq),],2,as.numeric)
dseq.ff5f=as.Date(paste(substr(ff5fdata.5f[,1],1,4),"/",substr(ff5fdata.5f[,1],5,6),"/01",sep =""))
ff5fdata.5fts=zoo(ff5fdata.5f[,2:7],order.by=dseq.ff5f)
index(ff5fdata.5fts) = as.yearmon(index(ff5fdata.5fts)) #changes format of the dates
colnames(ff5fdata.5fts)=c("Mkt-RF", "SMB", "HML", "RMW", "CMA","RF")#assing column names
factorsfinal=ff5fdata.5fts[,c(6,1:5)]
#########################
#Downloading Stock Prices Data
######################
dateStart="2010-01-01"
dateEnd="2021-10-01"

myassets=c("AAPL", "MSFT", "WMT", "TGT", "WFC", "JPM", "AAL", "BA", "XOM", "CVX");
nassets = length(myassets)
mystocks=get.hist.quote(instrument = myassets[1], start = dateStart, end = dateEnd, quote = "AdjClose",
                      origin="1970-01-01",compression = "m", retclass = "zoo",quiet = T)
dimnames(mystocks)[[2]]= as.character(myassets[1])
for (i in 2:nassets) {
  cat("Downloading ", i, " out of ", nassets , "\n")
  tempdata = try(x <- get.hist.quote(instrument = myassets[i],start = dateStart, end = dateEnd, quote = "AdjClose",
                                     origin="1970-01-01",compression = "m", retclass = "zoo",quiet = T))
  if(class(tempdata) == "try-error") {
    next
  }
  else {
    dimnames(x)[[2]] = as.character(myassets[i])
    mystocks = merge(mystocks, x)                      
  } }

index(mystocks) = as.yearmon(index(mystocks))

#######################
#Computing Returns
mystocksreturns=100*na.omit(Return.calculate(mystocks, method="log"))

##########################################
#Answer to Q4 (i) Computing CAPM Alphas,Betas, pvalue and R-square
#########################################
nstocks=ncol(mystocksreturns)
CAPMmat=matrix(0,nrow=nstocks,ncol=5)
for (i in 1:nstocks){
mydata=merge(mystocksreturns[,i],factorsfinal,all = FALSE)
capm.model=lm(mydata[,1]-mydata[,2]~mydata[,3])
alphav=summary(capm.model)$coefficients[,1][1]
palpha=summary(capm.model)$coefficients[,4][1]
beta=summary(capm.model)$coefficients[,1][2]
pbeta=summary(capm.model)$coefficients[,4][2]
rsquare=summary(capm.model)$adj.r.squared
CAPMmat[i,]=c(alphav,palpha,beta,pbeta,rsquare)
remove(capm.model,alphav,palpha,beta,pbeta,rsquare)
}
colnames(CAPMmat)=c("Alphas","P-val","Betas","P-val","R-Squred")
row.names(CAPMmat)=colnames(mystocksreturns)
CAPMmat
##################################
#If you want to export CAPM Alphas,Betas, pvalue and R-square, use the following codes
write.csv(CAPMmat,file = "CAPMRanking.csv")


###################################
#Answer to Q4(ii) Three Factors model
##################################
nstocks=ncol(mystocksreturns)
FF3Fmat=matrix(0,nrow=nstocks,ncol=9)
mydata=merge(mystocksreturns[,1],factorsfinal,all = FALSE)
mydata=merge(mystocksreturns[,1],factorsfinal,all = FALSE)
for (i in 1:nstocks){
mydata=merge(mystocksreturns[,i],factorsfinal,all = FALSE)
ff3f.model=lm(mydata[,1]-mydata[,2]~mydata[,3:5])
b1=summary(ff3f.model)$coefficients[,1][1]
b1p=summary(ff3f.model)$coefficients[,4][1]
b2=summary(ff3f.model)$coefficients[,1][2]
b2p=summary(ff3f.model)$coefficients[,4][2]
b3=summary(ff3f.model)$coefficients[,1][3]
b3p=summary(ff3f.model)$coefficients[,4][3]
b4=summary(ff3f.model)$coefficients[,1][4]
b4p=summary(ff3f.model)$coefficients[,4][4]
rsquare=summary(ff3f.model)$adj.r.squared

FF3Fmat[i,]=c(b1,b1p,b2,b2p,b3,b3p,b4,b4p,rsquare)
remove(ff3f.model,b1,b1p,b2,b2p,b3,b3p,b4,b4p,rsquare)
}
colnames(FF3Fmat)=c("Alpha","Pvalue","MKT","Pvalue","SMB","Pvalue","HML","Pvalue","R-Squred")
row.names(FF3Fmat)=colnames(mystocksreturns)
FF3Fmat

##################################
#If you want to export Three Factors Alphas,Betas, pvalue and R-square, use the following codes
write.csv(FF3Fmat,file = "FF3FmatRanking.csv")


