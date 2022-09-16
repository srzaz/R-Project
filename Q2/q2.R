library("quantmod")
library("ggplot2")
library("scales")
library("data.table")
library('xts')
library("PerformanceAnalytics")


options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Netflix price using quantmod

getSymbols("NFLX", from = '2009-03-01',
           to = "2021-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading NVIDIA price using quantmod
getSymbols("NVDA", from = '2009-03-01',
           to = "2021-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Amazon price using quantmod
getSymbols("AMZN", from = '2009-03-01',
           to = "2021-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

#normalize each stock
AMZNnorm = AMZN
NFLXnorm = NFLX
NVDAnorm = NVDA

AMZNnorm = as.xts(t(apply(AMZNnorm, 1, function(x) x/first(AMZNnorm))))
NFLXnorm = as.xts(t(apply(NFLXnorm, 1, function(x) x/first(NFLXnorm))))
NVDAnorm = as.xts(t(apply(NVDAnorm, 1, function(x) x/first(NVDAnorm))))

#store normalized

stocks = as.xts(data.frame(AMZNnorm = AMZNnorm[, "AMZN.Adjusted"],
                           NFLXnorm = NFLXnorm[, "NFLX.Adjusted"],
                           NVDAnorm = NVDAnorm[, "NVDA.Adjusted"]))

stocks = data.frame(Date=index(stocks), coredata(stocks))

stocks$Date <- as.Date(as.yearmon(stocks$Date))


#plot Normalized prices
ggplot(stocks, aes(x = Date)) + 
  geom_line(aes(y = AMZN.Adjusted, colour = "Amazon")) + 
  geom_line(aes(y = NFLX.Adjusted, colour = "Netflix")) +
  geom_line(aes(y = NVDA.Adjusted, colour = "NVIDIA")) +
  guides(fill=guide_legend(title="Companies"))+
  scale_y_continuous(breaks = seq(0,200,10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  ylab("Historical Normalized Prices")+
  xlab("Year")+
  labs(colour = "Company")+
  ggtitle("Historical Normalized Prices for AMZN, NFLX, and NVDA")



#calculate returns
AMZNreturns = Return.calculate(AMZN)
NVDAreturns = Return.calculate(NVDA)
NFLXreturns = Return.calculate(NFLX)

AMZNreturns = na.omit(AMZNreturns)
NVDAreturns = na.omit(NVDAreturns)
NFLXreturns = na.omit(NFLXreturns)

stocks2 = as.xts(data.frame(AMZNreturns = AMZNreturns[, "AMZN.Adjusted"],
                            NFLXreturns = NFLXreturns[, "NFLX.Adjusted"],
                            NVDAreturns = NVDAreturns[, "NVDA.Adjusted"]))
stocks2 = data.frame(Date=index(stocks2), coredata(stocks2))

stocks2$Date <- as.Date(as.yearmon(stocks2$Date))

#Cumulative returns
ggplot(stocks2, aes(x = Date) ) + 
  geom_line(aes(y = cumsum(AMZN.Adjusted), colour = "Amazon")) + 
  geom_line(aes(y = cumsum(NFLX.Adjusted), colour = "Netflix")) +
  geom_line(aes(y = cumsum(NVDA.Adjusted), colour = "NVIDIA")) +
  guides(fill=guide_legend(title="Companies"))+ 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = seq(0,7,0.5),labels = scales::percent)+
  ylab("Cumulative Returns")+
  xlab("Year")+
  labs(colour = "Company")+
  ggtitle("Cumulative Returns of AMZN, NFLX, and NVDA")


#If $10,000 was invested, how much would it be worth?

AMZNcumret = cumsum(stocks2$AMZN.Adjusted)
AMZNten = 10000 * last(AMZNcumret)

NFLXcumret = cumsum(stocks2$NFLX.Adjusted)
NFLXten = 10000 * last(NFLXcumret)

NVDAcumret = cumsum(stocks2$NVDA.Adjusted)
NVDAten = 10000 * last(NVDAcumret)




