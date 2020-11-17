###############
###Project: Quantative Strategy in Stocks
###Author: Santiago Canon
###Date: August 29th, 2020
###############

#Installing needed libraries for financial analysis
library(TTR)
library(qmao)
library(httr)
library(dplyr)
library(gargle)
library(fmpapi)
library(pander)
library(RJSONIO)
library(devtools)
library(quantmod)
library(jsonlite)
library(bigQueryR)
library(tidyverse)
library(bigrquery)
library(tidyquant)
library(data.table)
library(BatchGetSymbols)
library(FinancialInstrument)

install.packages("RMySQL")
library(RMySQL)
library(DBI)

con <- dbConnect(MySQL(),
                 dbname = 'standard_and_poors_stock_data',
                 host = '127.0.0.1',
                 port = 3306,
                 user = 'root')

dbListTables(con)

dbWriteTable(con, value = s_p_rename, name = "STANDARD_POOR_DAILY_TRACKER", append = TRUE , overwrite = FALSE)

#Remove Scientific Notation from Data Frames
options(scipen = 50)

#Specifying BQ Account we want to use and creating authentication
options(gargle_oauth_email = 'santiagocanondata0113@gmail.com')
bq_auth(email = "santiagocanondata0113@gmail.com")

#Crating our BigQuery Project ID
project_id = 'stable-arch-295502'

#Retriving our Stock Ticker Symbols and setting what range of dates to retrieve
first.date <- '2020-01-01'
last.date <- Sys.Date()
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers

#S&P 500 Index Needs to be Pulled Seperatley
sp = c('^GSPC')

#Retrieving Stock Data Based off of Stock Ticker Symbols in Tickers
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)

#S&p 500 Index
l.out_test <- BatchGetSymbols(tickers = sp,
                         first.date = first.date,
                         last.date = last.date)

#Getting the table from the list to get S&P 500 Stock Data
s_p <-  l.out_test$df.tickers

s_p_rename = s_p %>%
  rename(
    price_open = price.open,
    price_high = price.high,
    price_low = price.low,
    price_close = price.close,
    price_adjusted = price.adjusted,
    ref_date = ref.date,
    ret_adjusted_prices = ret.adjusted.prices,
    ret_closing_prices = ret.closing.prices,
  )


#Print out our results
YTD_Stock_Info = (l.out$df.tickers)
tickers <- l.out$ticker

#Getting Information on Companies API
res = GET("json object")#email about this if interested


#Insertting our API Key
api_key <- 'api key' #email about this if interested
fmp_api_key(api_key)
readRenviron('~/.Renviron')

#Retreving descriptive and financial information on stocks
my_stocks <- c(tickers)
Company_Facts <- fmp_profile(my_stocks)
print(Company_Facts)
glimpse(Company_Facts)

#Retreving Key Stock Metrics
Key_Stock_Metrics <- fmp_key_metrics(my_stocks)
View(Key_Stock_Metrics)

#Retreving Balance Sheet Information for Filing Periods
Balance_Sheet <- fmp_balance_sheet(my_stocks, quarterly = TRUE)
View(Balance_Sheet)

#Getting our Data from Yahoo API in Order to pull Quantatitive Stock Data
stockEnv <- new.env()
symbols <- getSymbols(tickers, src='yahoo', env=stockEnv)
datalist <- list()
tickers

#Creating for loop to get our data from the stocks in a table format
for(symbols in ls(stockEnv)){
  table <- as.data.frame( na.omit(stockEnv[[symbols]]))
  date = rownames(table)
  rownames(table) <- NULL
  colnames(table) <- c("Open","High","Low","Close","Volume","Adjusted")
  bound.table <- data.frame(Symbol = symbols, date ,table)
  datalist[[symbols]] <-  bound.table
}

#Getting the data in a Tabular format
Result <- rbindlist(datalist,fill=TRUE)

#Binding our other data points we want to see in a tabular format | Advanced Quant Stock Metrics
Result[,dn := lapply(.SD,function(x){BBands(x)[,1]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,mavg := lapply(.SD,function(x){BBands(x)[,2]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,up := lapply(.SD,function(x){BBands(x)[,3]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,pctB := lapply(.SD,function(x){BBands(x)[,4]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,CCI := lapply(.SD,function(x){CCI(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")]
#Result[,MACD := lapply(.SD,function(x){MACD(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")]
#Result[,signal := lapply(.SD,function(x){signal(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")] --use other instead
Result[,(c("RSI")):= list(as.vector(RSI(.SD, maType="EMA"))),by = "Symbol", .SDcols = c("Close")]
Result[,(c("SMA")):= list(as.vector(SMA(.SD))),by = "Symbol", .SDcols = c("Close")]
#Result[,(c("SMI")):= list(as.vector(SMI(.SD))),by = "Symbol", .SDcols = c("Close")]
Result[,(c("WPR")):= list(as.vector(WPR(.SD))),by = "Symbol", .SDcols = c("Close")]
#Result[,(c("macd","signal")):= apply(MACD(.SD,type="EMA"),2,function(x){as.list(x)}),by = "Symbol",.SDcols = c("Close")]

#Creating Final Data Frame to Upload to BigQuery
Result = data.frame(lapply(Result, as.character), stringsAsFactors=FALSE)

#Fix Issue with Balance Sheet Data Frame
Balance_Sheet$total_non_current_assets <-  NULL
Balance_Sheet$deferred_revenue <-  NULL


#Uploading the Tables Google BigQuery Enviroment
bigrquery_job <- bq_perform_upload("stable-arch-295502.STOCK_DATA.Key_Stock_metrics", Key_Stock_Metrics, billing = project_id)
bigrquery_job <- bq_perform_upload("stable-arch-295502.STOCK_DATA.Balance_Sheet_Data", Balance_Sheet, billing = project_id)
bigrquery_job <- bq_perform_upload("stable-arch-295502.STOCK_DATA.Company_Facts", Company_Facts)
bigrquery_job <- bq_perform_upload("stable-arch-295502.STOCK_DATA.Advanced_Quant_Metrics", Result)
bigrquery_job <- bq_perform_upload("stable-arch-295502.STOCK_DATA.S_P_500", s_p_rename)
