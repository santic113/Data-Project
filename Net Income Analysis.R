
# LOADING LIBRARIES -----
library(tidyquant)
library(dplyr)
library(fmpcloudr)
library(remotes)
library(fmpcloudr)
library(httr)
library(jsonlite)
library(xlsx)
library(openxlsx)

## Retreving API Keu to Retrieve Stock Data -----
api_key <- 'API KEY'
fmpc_set_token('API KEY')


## Retrieve all S&P500 Stock Tickers in a Vector Variable to Make Calls -----
sp500 <- tq_index("SP500") %>% select(symbol) %>% pull()



## Retrieve Income Statement Information for S&P Companies -----
financial_df <-  fmpc_financial_bs_is_cf(c(sp500))
stock_data <- financial_df


## Retrieve Comapnt Fact Information (i.e Industry) & Reformat -----
profile <- fmpc_security_profile(sp500)
profile_df <- profile %>%  
  select(symbol, industry, sector, ceo, description, fullTimeEmployees, companyName)


## Retrieve Most Recent 10Q Figures & Figures from Four Quarters Prior  -----
stock_data_df <- stock_data %>%
  left_join(x = stock_data, y = profile_df, by = "symbol") %>% 
 group_by(symbol) %>% 
 mutate(rank = dense_rank(desc(date))) %>% 
 filter(rank %in% c(1, 5))

View(stock_data_df)

write.xlsx(stock_data_df, file = 'your path file')




 
