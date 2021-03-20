## Summary
The goal of this project was the following:
* **Scrape** adjusted stock prices of all companies listed on the NASDAQ100, pull advanced quantitative metrics related to stocks,
* **Structure** the data into a table since the data comes in a list
* Upload the data into **Google BigQuery** for further manipulation and an easy connect for **Tableau**
* Create **Tableau** good-looking and interactive dashboard for users

## Approach
* Pull data in R using the [BatchGetSymbols](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Pull data and advanced metrics in R using the [quantmod](https://cran.r-project.org/web/packages/quantmod/quantmod.pdf) package
* Upload the data back into Google BigQuery using the [bigrquery](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Manipulate the data using the [apply](https://www.guru99.com/r-apply-sapply-tapply.html) family of functions
* Create [Tableau](https://public.tableau.com/profile/santiago.canon#!/vizhome/NASDAQ_100_Stock_Analysis/StockDashboard) dashboard

## Results
* [Tableau Dashboard](https://public.tableau.com/profile/santiago.canon#!/vizhome/NASDAQ_100_Stock_Analysis/StockDashboard  - That shows advanced quant metrics such as Bollinger bands, moving-average convergence divergence, signal, relative strength index, and CCI
* Google Bigquery Table that stores daily historical stock prices and advanced quant metrics for the past 365 from when I created this script

## Screenshot of Dashboard
![image](https://user-images.githubusercontent.com/43589961/111875748-bc655780-8971-11eb-8b53-efcfe3a97cd3.png)
