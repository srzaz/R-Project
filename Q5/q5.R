library("quantmod")
library("data.table")
library("xts")
library("PerformanceAnalytics")
library("corrplot")
library("psych")
library("ggplot2")

#########  
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)


# Monthly Prices from January 2010 to October 2021
###############
# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)


# Downloading Microsoft price using quantmod
getSymbols("MSFT", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)


# Downloading Walmart price using quantmod
getSymbols("WMT", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Target price using quantmod
getSymbols("TGT", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Wells Fargo price using quantmod
getSymbols("WFC", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading JPMorgan price using quantmod
getSymbols("JPM", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading American Airlines price using quantmod
getSymbols("AAL", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Boeing price using quantmod
getSymbols("BA", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Exxon price using quantmod
getSymbols("XOM", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Chevron price using quantmod
getSymbols("CVX", from = '2010-01-01',
           to = "2021-10-01", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)


#Calculate continuously compounded returns with Performance Analytics
#Store output into returns
AAPLreturns = Return.calculate(AAPL, method = "log")
MSFTreturns = Return.calculate(MSFT, method = "log")
WMTreturns = Return.calculate(WMT, method = "log")
TGTreturns = Return.calculate(TGT, method = "log")
WFCreturns = Return.calculate(WFC, method = "log")
JPMreturns = Return.calculate(JPM, method = "log")
AALreturns = Return.calculate(AAL, method = "log")
BAreturns = Return.calculate(BA, method = "log")
XOMreturns = Return.calculate(XOM, method = "log")
CVXreturns = Return.calculate(CVX, method = "log")



AAPLreturns = na.omit(AAPLreturns)
MSFTreturns = na.omit(MSFTreturns)
WMTreturns = na.omit(WMTreturns)
TGTreturns = na.omit(TGTreturns)
WFCreturns = na.omit(WFCreturns)
JPMreturns = na.omit(JPMreturns)
AALreturns = na.omit(AALreturns)
BAreturns = na.omit(BAreturns)
XOMreturns = na.omit(XOMreturns)
CVXreturns = na.omit(CVXreturns)


AAPLreturns = AAPLreturns[, "AAPL.Adjusted"]
MSFTreturns = MSFTreturns[, "MSFT.Adjusted"]
WMTreturns = WMTreturns[, "WMT.Adjusted"]
TGTreturns = TGTreturns[, "TGT.Adjusted"]
WFCreturns = WFCreturns[, "WFC.Adjusted"]
JPMreturns = JPMreturns[, "JPM.Adjusted"]
AALreturns = AALreturns[, "AAL.Adjusted"]
BAreturns = BAreturns[, "BA.Adjusted"]
XOMreturns = XOMreturns[, "XOM.Adjusted"]
CVXreturns = CVXreturns[, "CVX.Adjusted"]



#I am selecting Apple, Walmart, JPMorgan, Boeing, and Exxon
stocks = as.xts(data.frame(AAPLreturns,
                           WMTreturns,
                           JPMreturns,
                           BAreturns,
                           XOMreturns))

stocks = data.frame(Date=index(stocks), coredata(stocks))

stocks$Date <- as.Date(as.yearmon(stocks$Date))
#Pair-wise scatter plot
pairs(stocks[,c(2:6)],
      labels = c('Apple','Walmart', 'JPMorgan', 'Boeing', 'Exxon'),
      pch = 5,
      main = 'Scatterplot for AAPL, WMT, JPM, BA, and XOM')

pairs.panels(stocks[,c(2:6)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             labels = c('Apple','Walmart', 'JPMorgan', 'Boeing', 'Exxon'),
             main = "Correlation between AAPL, WMT, JPM, BA, and XOM"
)

#Correlation matrix

cormat = cor(stocks[,c(2:6)])
round(cormat, 2)

#
stocks = as.xts(data.frame(AAPLreturns,
                           WMTreturns,
                           JPMreturns,
                           BAreturns,
                           XOMreturns))

#####################################
# Individual mean returns and Covariance matrix

mu=colMeans(stocks)
stdv=apply(stocks, 2, sd)
sigma=cov(stocks)
as.data.frame(mu)
as.data.frame(sigma)

############################
#Calculate equally weighted portfolio mean and variance
nr=dim(stocks)[1]
nc=dim(stocks)[2]
wt=matrix(rep(1/nc,nc),nrow=nc,ncol=1)
portmean=t(wt)%*%mu
portvar=t(wt)%*%sigma%*%wt
portstd=sqrt(portvar)
meanandcovar=round(rbind(portmean,portvar,portstd),3)
rownames(meanandcovar)=c("Equally Weighted Portfolio Mean","Equally WeightedPortfolio Variance","Equally Weighted Portfolio Standard Dev")
colnames(meanandcovar)="Value"
meanandcovar

# Calculating global minimum variance port
gmin.port = globalMin.portfolio( mu, sigma)

gminweight = data.frame(gmin.port$weights)
colnames(gminweight) <- ("Weight")
gminweight$Asset <- row.names(gminweight)

ggplot(gminweight, aes( y=Weight, x=Asset))+
   geom_bar(stat = "identity", aes(fill=Asset))+
   theme_minimal()+
   guides(fill="none")+
   scale_x_discrete(labels=c("Apple", "Boeing", "JP Morgan", "Walmart", "Exxon"))+
   geom_text(aes(label=round(Weight,2)), 
             position=position_dodge(width=0.9), 
             vjust=-0.25)+
   ggtitle("Global Minimum Portfolio Weights")


#Tangency portfolio
rk.free=0.0004167
tan.port = tangency.portfolio(mu, sigma, rk.free)

tanweights = data.frame(tan.port$weights)
colnames(tanweights) <- ("Weight")
tanweights$Asset <- row.names(tanweights)

ggplot(tanweights, aes( y=Weight, x=Asset))+
   geom_bar(stat = "identity", aes(fill=Asset))+
   theme_minimal()+
   guides(fill="none")+
   scale_x_discrete(labels=c("Apple", "Boeing", "JP Morgan", "Walmart", "Exxon"))+
   geom_text(aes(label=round(Weight,2)), 
             position=position_dodge(width=0.9), 
             vjust=-0.25)+
   ggtitle("Tangency Portfolio Weights")
   
shrp = tan.port$er/tan.port$sd
shrp

#Answer to Q5(g)
VaR.95pct=tan.port$er-1.645*tan.port$sd
VaR.95pct
Var.99pct=tan.port$er-2.326*tan.port$sd
Var.99pct





