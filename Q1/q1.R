library("ggplot2")
library("ggforce")
library("quantmod")
library("readxl")
library("tidyr")
library("scales")
library("data.table")
library("melt")
library("readxl")

#Prep data
Q1APPL <- read_excel("Q1APPL.xlsx")
Q1WMT <- read_excel("Q1WMT.xlsx")
Q1BAC <- read_excel("Q1BAC.xlsx")
Q1FB <- read_excel("Q1FB.xlsx")


#Annual Revenue growth Apple

ggplot(Q1APPL, aes(x=Year, y=`Revenue growth`, fill=`Revenue growth`>0)) +
  geom_bar( stat="identity") +
  scale_y_continuous(breaks = seq(-1,1,0.05),labels = scales::percent)+
  scale_x_continuous(breaks = Q1APPL$Year)+
  geom_hline(aes(yintercept = 0))+
  geom_text(aes(label=scales::percent(`Revenue growth`)), 
            position=position_dodge(width=0.9), 
            vjust=-0.25)+
  guides(fill=FALSE)+
  ggtitle("Apple Annual Revenue Growth in %")
  

#Annual Return on Assets
ggplot(Q1APPL, aes(x=Year, y=`Return on Assets`, fill=`Return on Assets`>0)) +
  geom_bar( stat="identity") +
  geom_hline(aes(yintercept = 0))+
  geom_text(aes(label=scales::percent(`Return on Assets`)),
            position=position_dodge(width=0.9), 
            vjust=-0.25)+
  scale_y_continuous(breaks = seq(-1,1,0.05),labels = scales::percent)+
  scale_x_continuous(breaks = Q1APPL$Year)+
  guides(fill=FALSE)+
  ggtitle("Apple Annual Return on Assets in %")

#Revenue growth for Apple, Facebook, Walmart, Bank of America
dfr <- data.frame(
  Year = c(2020:2008),
  APPL = Q1APPL$`Revenue growth`,
  FB = Q1FB$`Revenue Growth`,
  WMT = Q1WMT$`Revenue Growth`,
  BAC = Q1BAC$`Revenue Growth`
)
dfr = na.omit(dfr)

dfr1 <- melt(dfr[,c('Year','APPL','FB', 'WMT', 'BAC')],id.vars = 1)

ggplot(dfr1,aes(x = Year,y = value)) + 
  geom_bar(aes(fill = variable),
           stat = "identity",
           position = position_dodge(0.7))+
  scale_y_continuous(breaks = seq(-1,2,0.05),labels = scales::percent)+
  geom_hline(aes(yintercept = 0))+
  guides(fill=guide_legend(title="Companies"))+
  scale_x_continuous(breaks = dfr1$Year)+
  ggtitle("Annual Revenue Growth for APPL, FB, WMT, and BAC in %")



