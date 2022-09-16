library("tseries")
library("quantmod")
library("ggplot2")
library("scales")
library("data.table")
library('xts')
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library("IntroCompFinR")


options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# Monthly Prices from March 2009 to August 2020
# Downloading Netflix price using quantmod

getSymbols("NFLX", from = '2009-03-01',
           to = "2020-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Apple price using quantmod

getSymbols("AAPL", from = '2009-03-01',
           to = "2020-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)

# Downloading Microsoft price using quantmod
getSymbols("MSFT", from = '2009-03-01',
           to = "2020-08-31", periodicity = "monthly", warnings = FALSE,
           auto.assign = TRUE)


#calculate continuously compounded returns with Performance Analytics
AAPLreturns = Return.calculate(AAPL, method = "log")
MSFTreturns = Return.calculate(MSFT, method = "log")
NFLXreturns = Return.calculate(NFLX, method = "log")

AAPLreturns = na.omit(AAPLreturns)
MSFTreturns = na.omit(MSFTreturns)
NFLXreturns = na.omit(NFLXreturns)


AAPLreturns = AAPLreturns[, "AAPL.Adjusted"]
NFLXreturns = NFLXreturns[, "NFLX.Adjusted"]
MSFTreturns = MSFTreturns[, "MSFT.Adjusted"]


stocks = as.xts(data.frame(AAPLreturns,
                          MSFTreturns,
                           NFLXreturns))


stocks = data.frame(Date=index(stocks), coredata(stocks))

stocks$Date <- as.Date(as.yearmon(stocks$Date))

#holding period return  (End of period Value - Initial value) / (Initial value)
AAPLhpr = (last(stocks$AAPL.Adjusted) - first(stocks$AAPL.Adjusted)) / (first(stocks$AAPL.Adjusted))
MSFThpr = (last(stocks$MSFT.Adjusted) - first(stocks$MSFT.Adjusted)) / (first(stocks$MSFT.Adjusted))
NFLXhpr = (last(stocks$NFLX.Adjusted) - first(stocks$NFLX.Adjusted)) / (first(stocks$NFLX.Adjusted))


#Arithmetic mean
AAPL_arithmean = mean(AAPLreturns, trim = 0)
MSFT_arithmean = mean(MSFTreturns, trim = 0)
NFLX_arithmean = mean(NFLXreturns, trim = 0)

#Geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

AAPL_geomean = gm_mean(AAPLreturns)
MSFT_geomean = gm_mean(MSFTreturns)
NFLX_geomean = gm_mean(NFLXreturns)


#Cumulative returns
AAPL_cumret = Return.cumulative(AAPLreturns)
MSFT_cumret = Return.cumulative(MSFTreturns)
NFLX_cumret = Return.cumulative(NFLXreturns)

#Standard Deviation
AAPL_sd = sd(AAPLreturns)
MSFT_sd = sd(MSFTreturns)
NFLX_sd = sd(NFLXreturns)

#Min and max return
AAPL_min = min(AAPLreturns)
MSFT_min = min(MSFTreturns)
NFLX_min = min(NFLXreturns)

AAPL_max = max(AAPLreturns)
MSFT_max = max(MSFTreturns)
NFLX_max = max(NFLXreturns)


#Skewness
AAPL_skew = skewness(AAPLreturns)
MSFT_skew = skewness(MSFTreturns)
NFLX_skew = skewness(NFLXreturns)


#Kurtosis
AAPL_kurt = kurtosis(AAPLreturns)
MSFT_kurt = kurtosis(MSFTreturns)
NFLX_kurt = kurtosis(NFLXreturns)

#Value at Risk
AAPL_VaR = VaR(AAPLreturns)
MSFT_VaR = VaR(MSFTreturns)
NFLX_VaR = VaR(NFLXreturns)

#CVaR
AAPL_CVaR = CVaR(AAPLreturns)
MSFT_CVaR = CVaR(MSFTreturns)
NFLX_CVaR = CVaR(NFLXreturns)

############ APPLE ###############

AAPLstats = rbind(AAPLhpr,
                  AAPL_arithmean,
                  AAPL_geomean,
                  AAPL_cumret,
                  AAPL_sd,
                  AAPL_min,
                  AAPL_max,
                  AAPL_skew,
                  AAPL_kurt,
                  AAPL_VaR,
                  AAPL_CVaR)
rownames(AAPLstats) = c("Holding period return",
                        "Arithmetic mean",
                        "Geometric return",
                        "Cumulative return",
                        "Standard deviation",
                        "Minimum return",
                        "Maximum return",
                        "Skewness",
                        "Kurtosis",
                        "Value at Risk",
                        "CVaR")
round(AAPLstats, digits = 4)

############ MICROSOFT ###############

MSFTstats = rbind(MSFThpr,
                  MSFT_arithmean,
                  MSFT_geomean,
                  MSFT_cumret,
                  MSFT_sd,
                  MSFT_min,
                  MSFT_max,
                  MSFT_skew,
                  MSFT_kurt,
                  MSFT_VaR,
                  MSFT_CVaR)
rownames(MSFTstats) = c("Holding period return",
                        "Arithmetic mean",
                        "Geometric return",
                        "Cumulative return",
                        "Standard deviation",
                        "Minimum return",
                        "Maximum return",
                        "Skewness",
                        "Kurtosis",
                        "Value at Risk",
                        "CVaR")
round(MSFTstats, digits = 4)

############ NETFLIX ###############

NFLXstats = rbind(NFLXhpr,
                  NFLX_arithmean,
                  NFLX_geomean,
                  NFLX_cumret,
                  NFLX_sd,
                  NFLX_min,
                  NFLX_max, 
                  NFLX_skew,
                  NFLX_kurt,
                  NFLX_VaR,
                  NFLX_CVaR)
rownames(NFLXstats) = c("Holding period return",
                        "Arithmetic mean",
                        "Geometric return",
                        "Cumulative return",
                        "Standard deviation",
                        "Minimum return",
                        "Maximum return",
                        "Skewness",
                        "Kurtosis",
                        "Value at Risk",
                        "CVaR")
round(NFLXstats, digits = 4)


