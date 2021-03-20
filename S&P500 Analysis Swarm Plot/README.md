## Summary
The goal of this project was the following:
* **Scrape** adjusted stock prices of all companies listed on the S&P500 for the past year from the date I created the script
* **Structure** the data into a table since the data comes in a list
* Upload the data into **Google BigQuery** for further manipulation and an easy connect for **Tableau**

## Approach
* Pull data in R using the [BatchGetSymbols](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Upload the data back into Google BigQuery using the [bigrquery](https://cran.r-project.org/web/packages/BatchGetSymbols/BatchGetSymbols.pdf) package
* Manipulate the data using dplyr and SQL 
* Create [Swarm Plot in Tableau](https://public.tableau.com/profile/santiago.canon#!/vizhome/SP500YTDAnalysisSwarmPlotbySector/SwarmPlot) in Tableau

## Results
* **[Tableau Dashboard](https://public.tableau.com/profile/santiago.canon#!/vizhome/SP500YTDAnalysisSwarmPlotbySector/SwarmPlot)**  - Swarm plot showing YTD% stock prices changes for all fortunate 500 stocks


## Screenshot of Dashboard
![image](https://user-images.githubusercontent.com/43589961/111876571-afe2fe00-8975-11eb-95c0-474414a622dc.png)
