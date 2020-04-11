#Downloading Needed Libraries for Analysis
library(stocks)
library(BatchGetSymbols)
library(bigrquery)
library(bigQueryR)
library(quantmod)
library(dplyr)
library(data.table)

#Getting our NASDAQ 100 Stock Tickers
stocks <- c('ADBE',
            'AMD',
            'ALXN',
            'ALGN',
            'GOOG',
            'GOOGL',
            'AMZN',
            'AAL',
            'AMGN',
            'ADI',
            'ANSS',
            'AAPL',
            'AMAT',
            'ASML',
            'ADSK',
            'ADP',
            'BIDU',
            'BIIB',
            'BMRN',
            'BKNG',
            'AVGO',
            'CDNS',
            'CDW',
            'CERN',
            'CHTR',
            'CHKP',
            'CTAS',
            'CSCO',
            'CTXS',
            'CTSH',
            'CMCSA',
            'CPRT',
            'CSGP',
            'COST',
            'CSX',
            'DLTR',
            'EBAY',
            'EA',
            'EXC',
            'EXPE',
            'FB',
            'HD',
            'FAST',
            'FISV',
            'FOX',
            'FOXA',
            'GILD',
            'IDXX',
            'ILMN',
            'INCY',
            'INTC',
            'INTU',
            'ISRG',
            'JD',
            'KLAC',
            'LRCX',
            'LBTYA',
            'LBTYK',
            'LULU',
            'MAR',
            'MXIM',
            'MELI',
            'MCHP',
            'MU',
            'MSFT',
            'MDLZ',
            'MNST',
            'NTAP',
            'NTES',
            'NFLX',
            'NVDA',
            'NXPI',
            'ORLY',
            'PCAR',
            'PAYX',
            'PYPL',
            'PEP',
            'QCOM',
            'REGN',
            'ROST',
            'SGEN',
            'SIRI',
            'SWKS',
            'SPLK',
            'SBUX',
            'SNPS',
            'TMUS',
            'TTWO',
            'TSLA',
            'TXN',
            'KHC',
            'TCOM',
            'ULTA',
            'UAL',
            'VRSN',
            'VRSK',
            'VRTX',
            'WBA',
            'WDC',
            'WLTW',
            'WDAY',
            'XEL',
            'XLNX')

#Getting our Data from Yahoo API
stockEnv <- new.env()
symbols <- getSymbols(stocks, src='yahoo', env=stockEnv)
datalist <- list()

#Creating for loop to get our data from the stocks in a table format
for(stock in ls(stockEnv)){
  table <- as.data.frame( na.omit(stockEnv[[stock]]))
  date = rownames(table)
  rownames(table) <- NULL
  colnames(table) <- c("Open","High","Low","Close","Volume","Adjusted")
  bound.table <- data.frame(Symbol = stock, date ,table)
  datalist[[stock]] <-  bound.table
}

#Getting the data in a Tabular format
Result <- rbindlist(datalist,fill=TRUE)
View(Result)

#Binding our other data points we want to see in a tabular format
Result[,dn := lapply(.SD,function(x){BBands(x)[,1]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,mavg := lapply(.SD,function(x){BBands(x)[,2]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,up := lapply(.SD,function(x){BBands(x)[,3]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,pctB := lapply(.SD,function(x){BBands(x)[,4]}), by="Symbol", .SDcols = c("High","Low","Close")]
Result[,CCI := lapply(.SD,function(x){CCI(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")]
#Result[,MACD := lapply(.SD,function(x){MACD(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")]
#Result[,signal := lapply(.SD,function(x){signal(x)}[,1]), by="Symbol", .SDcols = c("High","Low","Close")] --use other instead
Result[,(c("RSI")):= list(as.vector(RSI(.SD, maType="EMA"))),by = "Symbol", .SDcols = c("Close")]
Result[,(c("SMA")):= list(as.vector(SMA(.SD))),by = "Symbol", .SDcols = c("Close")]

#Use the ones above instead for the BBands, this one runs into issues
Result[,(c("dn","mavg","up","pctB")):=
         apply(BBands(.SD),2,function(x){as.list(x)}),
       by = "Symbol",
       .SDcols = c("High","Low","Close")]

#Use this one instead of the commented out ones instead. This does not run into issues
Result[,(c("macd","signal")):=
         apply(MACD(.SD,type="EMA"),2,function(x){as.list(x)}),
       by = "Symbol",
       .SDcols = c("Close")]

#Create the data into a dataframe instead of a list so that we can export it out as a CSV
Result2 = data.frame(lapply(Result, as.character), stringsAsFactors=FALSE)
write.csv(Result2,'NASDAQ100_10April2020.csv')

#I don't think this ne is needed but I am not sure
Result[,(c("CCI")):= list(as.vector(CCI(.SD))),by = "Symbol", .SDcols = c("High","Low","Close")]


#For loop for our plots of our data, also creates a PDF of our data and saves it for us
pdf('NASDAQ_100_10April2020.pdf', width = 11.5, height = 11.5)
for (stocks in ls(stockEnv)){
  chartSeries(na.omit(stockEnv[[stocks]]), theme="white", name=stocks,
              TA="addVo();addBBands();addCCI();addSMA(20, col='blue');
        addSMA(5, col='red');addMACD();addRSI();addROC()", subset='last 60 days')    2
}
dev.off()
job <- insert_upload_job("sunny-shadow-273304","stocks","NASDAQ_STOCKS", Result)
