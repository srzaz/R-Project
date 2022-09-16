library("tseries")
library("quantmod")
library("data.table")
library("xts")
library("zoo")
library("readxl")
library("timetk")
library("PerformanceAnalytics")
library("readr")
library('ggplot2')
library("Quandl")
library("lmtest")
library("gridExtra")
#########  
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)


# Daily Prices from Dec. 31 2016 to Oct. 31 2021
###############
# Downloading QQQ price using quantmod

getSymbols("QQQ", from = '2016-12-31',
           to = "2021-10-31", periodicity = "daily", warnings = FALSE,
           auto.assign = TRUE)

######

Invesco = QQQ[,"QQQ.Adjusted"]

Invesco = as.xts(data.frame(Invesco))
colnames(Invesco) <- ("Price")
Invesco = data.frame(Date=index(Invesco), coredata(Invesco))

# Plot SMA 50-day, 200-day
temp.zoo <- zoo(Invesco$Price, Invesco$Date)

m.av50 <- frollmean(temp.zoo,50, fill=NA)
m.av200 <- frollmean(temp.zoo,200, fill=NA)
Invesco$MA50 = coredata(m.av50)
Invesco$MA200 = coredata(m.av200)

ggplot(Invesco, aes(x=(as.Date(Date)))) +
  geom_line( aes(y=Price, colour = "Price"))+
  geom_line(aes(y=MA50, colour = "MA50"))+
  geom_line(aes(y=MA200, colour = "MA200"))+
  scale_y_continuous(breaks = seq(100,500,25)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("Years")+
  ylab("Price ($)")+
  theme(legend.title = element_blank())+
  ggtitle("Invesco QQQ Historical Prices with Moving Averages")
  


#Bollinger Bands
Invesco = QQQ[,"QQQ.Adjusted"]

Invesco = as.xts(data.frame(Invesco))
colnames(Invesco) <- ("Price")
Invesco <- BBands(Invesco, n=20,sd=2)
Invesco = data.frame(Date=index(Invesco), coredata(Invesco))
Invesco$Price <-QQQ$QQQ.Adjusted


#plot

ggplot(Invesco, aes(x=(as.Date(Date)))) +
  geom_ribbon(aes(ymin=dn, ymax=up),fill = "grey80")+
  geom_line(aes(y=dn))+
  geom_line(aes(y=up))+
  geom_line(aes(y=mavg, colour = "Moving Average"))+
  geom_line( aes(y=Price, colour = "Price"))+
  scale_y_continuous(breaks = seq(100,500,25)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("Years")+
  ylab("Price ($)")+
  theme(legend.title = element_blank())+
  ggtitle("Invesco QQQ Bollinger Bands")


#Relative Strength Index
Invesco = QQQ[,"QQQ.Adjusted"]

Invesco = as.xts(data.frame(Invesco))
InvescoRSI = RSI(Invesco,n=14)

Invesco = data.frame(Date=index(Invesco), coredata(Invesco))
Invesco$Price <-QQQ$QQQ.Adjusted
InvescoRSI =  data.frame(Date=index(InvescoRSI), coredata(InvescoRSI))

rsiplot <- ggplot(InvescoRSI, aes(x=(as.Date(Date)))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  geom_line( aes(y=rsi, colour = "RSI"))+
  geom_hline(aes(yintercept = 70))+
  geom_hline(aes(yintercept = 30))+
  xlab("Years")+
  ylab("Relative Strength Index")+
  theme(legend.title = element_blank())+
  ggtitle("Invesco QQQ RSI")

priceplot <- ggplot(Invesco, aes(x=(as.Date(Date)))) +
  geom_line( aes(y=Price, colour = "Price"))+
  scale_y_continuous(breaks = seq(100,500,25)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("Years")+
  ylab("Price ($)")+
  theme(legend.title = element_blank())+
  ggtitle("Invesco QQQ Historical Price")


grid.arrange(priceplot, rsiplot)

