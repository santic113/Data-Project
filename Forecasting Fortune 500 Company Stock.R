

#Packages required to get stock prices
install.packages('BatchGetSymbols')
library(BatchGetSymbols)

#Packages required to connect to Google Bigquery in order to upload data
install.packages('bigrquery')
library(bigrquery)
​
#Setting the date dimensions I want. Small example here and then we will expand it.
first.date <- Sys.Date() - 200
first.date
last.date <- Sys.Date()
last.date
freq.data <- 'daily'
​
#Testing and validating some stock tickers we know. The last ticker is not a real one so it should null out.
tickers <- c('FB','MMM','PETR4.SA','abcdef')
​
#Retrieving the data we want from our library
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(),
                                                  'BGS_Cache') )
print(l.out$df.control)

​
​#Setting the date ranges we want for all the stocks
first.date <- Sys.Date()-365
last.date <- Sys.Date()

#Getting all the fortune 500 stocks.
df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date)
print(l.out$df.control)
print(l.out$df.tickers)
​
#Taking a look and validating the data we just imported
test = (l.out$df.tickers)
test
View(test)
​
#Creating our table and some further validation
table_stock <- bind_rows(test2,test3, test4)
​test2 <- test$price.adjusted
​test3 <- test$ref.date
test4 <- test$ticker
​test2
​View(test2)
​
#Writing our table to our PC
write.table(test, file="insert your path here ", sep=",")

​#Reading the table
stock_table <- read.table("C:/Users/SXC3GFL/OneDrive - The Home Depot/Documents/Hugo Project Updates/Stock_Ticker_V2.xlsx", header= FALSE, sep = "",fill = TRUE )
​
​#Another check again
View(stock_table)
​
#Finally importing our data into Google bigquery using the packahges below and the commands below
library(bigQueryR)
bqr_auth()
bqr_upload_data(projectId = 'insert your project ID here',
                datasetId = 'insert your datasetID here', tableId = 'insert your table ID here ', upload_data = test ,
                create = c("CREATE_IF_NEEDED"), overwrite = TRUE,
                schema = NULL, sourceFormat = c("CSV", "DATASTORE_BACKUP",
                                                "NEWLINE_DELIMITED_JSON", "AVRO"), wait = TRUE, autodetect = TRUE,
                nullMarker = NULL, maxBadRecords = NULL, allowJaggedRows = FALSE,
                allowQuotedNewlines = FALSE, fieldDelimiter = ",")
