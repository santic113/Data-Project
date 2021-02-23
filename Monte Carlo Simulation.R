---
title: "Stock Analysis"
author: "Santiago Canon"
date: "1/30/2021"
output:
  html_document:
    highlight: monochrome
    theme: flatly
---
## **Summary**
<font size="4"> The goal of this analysis is to examine 7 stocks (**Wal-Mart, HomeDepot, Microsoft, Hilton, Gap, Tesla**). We are going to examine their historical stock prices and advanced quantative metrics. We are also going to attempt to forecast the stock prices **4 years** out using a Monte Carlo simulation. </font>

```{r setup, include=FALSE}
library(quantmod)   # Quantatative Financial Modeling Framework. ASsists in specifying, building, and analyszing financial trading strategies
library(TTR)       #  A collection of over 50 technical indicators for creating technical trading rules.
library(dplyr)    #   Data Manipulation
library(BatchGetSymbols)
library(xts)
library(stringr)
library(forcats)
library(kableExtra)
library(ggthemes)
library(PerformanceAnalytics)
library(lubridate)
library(plotly)
library(tidyverse)
```


```{r, include=FALSE}
stock_sample <- c("WMT","HD", "MSFT", "HLT", "GPS", "GM", "TSLA" )
```


```{r, include=FALSE}
#Setting the date ranges of what stock data you want to retrieve
first.date <- Sys.Date() - 1460
last.date <- Sys.Date()
```


```{r, warning=FALSE, include=FALSE}
#Library that retrieves the stock symbol
symbols_sample <- getSymbols(stock_sample, src='yahoo')
```


```{r, echo=FALSE, warning=FALSE, include=FALSE}
#Retrieving stock information from your tickers and the date ranges you choose
stocks_sample <- BatchGetSymbols(tickers = symbols_sample,
                         first.date = first.date,
                         last.date = last.date)

#Daily Log Returns of Wal-Mart Stock for the past 5 years
sample_log_returns_WMT <- WMT%>%Ad()%>%dailyReturn(type='log')  #getting log return from WMT

```

### **Plotting 2020 Day-over-Day Stock Prices & Quantitative Metrics for Wal-Mart in 2020**
* <font size="4"> **Historical Price**- Day-over-day stock price for Wal-Mart in 2020
* **Quant Metrics** - Bollinger bands, MCAD, MAVG, Volume, Singal
</font>
```{r,  echo=FALSE, warning=FALSE, fig.width=15,fig.height=9}

#Advanced quantatitaive metrics from stock
WMT%>%Ad()%>%chartSeries(subset ='2020')

#What type of charts you want to include in your chart
WMT%>%chartSeries(TA='addBBands();addVo();addMACD()',subset='2020', theme= 'white')
```
<br><br><br>

### **Wal-Mart Price Simulation in 4 Years**
* <font size="4"> **Simulation**- One Monte Carlo Simulation for the next 4 years
* **Comments** - Since the stock market is volitale and unpredictable it is better to look at this through a series of simulations for the next 4 years
</font>
```{r,  echo=FALSE, warning=FALSE, fig.width=15,fig.height=8}
#  Calculating the Mean Log
WMT_mean_log<-mean(sample_log_returns_WMT)

#Store in a vector and round
mean_log_WMT <-c(WMT_mean_log)
mean_log_WMT <-round(WMT_mean_log,4)


#standard deviation of log stock returns
WMT_sd_log<-sd(sample_log_returns_WMT)

#Renaming variable for simplicity
mu_wmt  <-WMT_mean_log # mean of log returns
sig_wmt <-WMT_sd_log # sd of log returns

#generate random daily exponent increase rate using WMT's mean,sd, & log returns
#one year 252 trading days, simulate for 4 years
price_wmt<-rep(NA,252*4)
testsim_wmt <-rep(NA,1000)

#most recent price
price_wmt[1]<-as.numeric(WMT$WMT.Adjusted[length(WMT$WMT.Adjusted),])

t <-  for(i in 2:length(testsim_wmt)){
  price_wmt[i]<-price_wmt[i-1]*exp(rnorm(1,mu_wmt,sig_wmt))
}

random_data<-cbind(price_wmt,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)


random_data%>%ggplot(aes(Day,Price))+
  geom_line()+
  labs(title="Wal-Mart price simulation for 4 years")+
  theme_economist()
```
<br><br><br>

### **Wal-Mart Price 500 Monte Carlo Simulations**
* <font size="4"> **Simulations**- 500 Monte Carlo Simulations for the Next 4 Years
* **Comments** - Since the stock market is volitale and unpredictable it is better to look at this through a series of simulations for the next 4 years
</font>
```{r,  echo=FALSE, warning=FALSE, fig.width=15,fig.height=8}
N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(WMT$WMT.Adjusted[length(WMT$WMT.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(WMT$WMT.Adjusted[length(WMT$WMT.Adjusted),])
for(i in 2:nrow(mc_matrix)){
  mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu_wmt,sig_wmt))
}
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name


final_mat%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+
  geom_line(alpha=0.2)+
  labs(title="Wal-Mart: 500 Monte Carlo Simulations for 4 Years")+
  theme_economist()
```
<br><br><br>


### **Quantiles of Simulations**
* <font size="4"> **Quantiles**- The Monte Carlo time-series graph can be a bit hard to interpret so let's look at the data through
* **Comments** - We are looking at the following quantiles from the Monte Carlo Simulation **.005%, 0.025%, 0.25%, 0.50%, 0.75%, 0.975%. 0.995%**)

</font>
```{r,  echo=FALSE, warning=FALSE, fig.width=15,fig.height=8}
probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)


final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), position = "left")
```
