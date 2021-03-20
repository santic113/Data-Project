## Summary
The goal of this project was the following:
* Pull adjusted stock prices of all companies listed on the S&P500 for the past year from the date I created the script
* **Structure** the data into a table since the data comes in a list
* Upload the data into **Google BigQuery** for further manipulation and easy connect for Tableau
* Create **Prophet forecasting** model to forecasting the price of a given stock for the next 60 days

## Approach
* Pull data in R using the [BatchGetSymbols](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Upload the data back into Google BigQuery using the [bigrquery](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Manipulate the data using base R
* Create [Prophet](https://cran.r-project.org/web/packages/prophet/prophet.pdf) model using the Prophet package  

## Results
* **[Tableau Dashboard](https://public.tableau.com/profile/santiago.canon#!/vizhome/ForecastallFortune500CompayStock60DaysOut/ForecastingStockDashboard)**  - Colorful Tableau dashboard showing the our forecasting results 
