

#Install Prophet algorithim package
install.packages('prophet')
library(prophet)

#Install packages to connect to GoogleBigquery
install.packages('bigrquery')
library(bigrquery)
library(dplyr)
project_id = 'insert your project ID here'

#Query that runs in the back end to pull in data
data_Frame1 = "
SELECT DISTINCT
ticker	AS CONCAT_NAME
FROM
  `insert your table name here`
"

#Query that runs in the back end to pull in data
data_frame_2 = "
SELECT
    price_adjusted as y
  , ref_date AS ds
  , ticker AS CONCAT_NAME
FROM
  `insert your table name here`

"
#Actual command to execute the two queries we wrote above
distinct_data_frame = query_exec(data_Frame1, project = project_id, use_legacy_sql = FALSE)
distinct_data_frame_2 = query_exec(data_frame_2, project = project_id, use_legacy_sql = FALSE, max_pages = Inf)

#Variables needed to run our for loop
i = NULL
dfreal = NULL

#Accounting for federal holidays
holiday2 <- data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2019-01-01', '2019-07-04', '2019-11-11',
                 '2019-01-21', '2019-09-02', '2019-11-28',
                 '2019-02-18', '2019-01-12',
                 '2019-05-27', '2019-12-25', '2019-12-24',
                 '2018-01-01', '2018-07-04', '2018-11-11',
                 '2018-01-21', '2018-09-02', '2018-11-28',
                 '2018-02-18', '2018-01-12',
                 '2018-05-27', '2018-12-25', '2018-12-24',
                 '2017-01-01', '2017-07-04', '2017-11-11',
                 '2017-01-21', '2017-09-02', '2017-11-28',
                 '2017-02-18', '2017-01-12',
                 '2017-05-27', '2017-12-25', '2017-12-24',
                 '2016-01-01', '2016-07-04', '2016-11-11',
                 '2016-01-21', '2016-09-02', '2016-11-28',
                 '2016-02-18', '2016-01-12',
                 '2016-05-27', '2016-12-25', '2016-12-24' ,
                 '2020-01-01', '2020-07-04', '2020-11-11',
                 '2020-01-21', '2020-09-02', '2020-11-28',
                 '2020-02-18', '2020-01-12',
                 '2020-05-27', '2020-12-25', '2020-12-24'  )),
  lower_window = 1,



  upper_window = 1
)


#For loop to run the forecast on all the stocks and so that it is in a tabular form.
for(i in 1:nrow(distinct_data_frame)){
  dftemp = NULL
  dftemp = distinct_data_frame_2[distinct_data_frame_2$CONCAT_NAME == distinct_data_frame$CONCAT_NAME[i],]
  dftemp2 = NULL
  dftemp2$ds = dftemp$ds
  dftemp2$y = dftemp$y
  dftemp2 = as.data.frame(dftemp2)

  #Creating the model
  m <- prophet(holidays2 = holidays2)
  m <- prophet(dftemp2,seasonality.mode = 'additive', weekly.seasonality="TRUE", yearly.seasonality="TRUE", daily.seasonality = "TRUE", n.changepoints
               = 10, interval.width = .70)

  #Forecasting 60 days, can be adjusted to more weeks.
  future <- make_future_dataframe(m, periods = 60, freq = 'day', include_history = TRUE)
  dftemp2$cap <- 10000000
  dftemp2$floor <- 1000
  future$cap <- 8.5
  future$floor <- 50

  #Running the actual forecast
  forecast <- predict(m, future)
  library(ggplot2)
  #Printing the visual for the time series and adding change point plots to the plot
  print(plot(m, forecast,xlabel =distinct_data_frame$CONCAT_NAME[i])  + add_changepoints_to_plot(m, '2019-06-10'), col = 'red')

  #dev.off()
  forecast$CONCAT_NAME = distinct_data_frame$CONCAT_NAME[i]
  dfreal = bind_rows(dfreal,forecast)


}


#Uploading the data back into big query so that we have it in a tabular form and can plug it into Tableau
library(bigQueryR)
bqr_auth()
bqr_upload_data(projectId = 'insert your project ID here',
                datasetId = 'insert your data set ID here', tableId = 'insert your table name here', upload_data = dfreal ,
                create = c("CREATE_IF_NEEDED"), overwrite = TRUE,
                schema = NULL, sourceFormat = c("CSV", "DATASTORE_BACKUP",
                                                "NEWLINE_DELIMITED_JSON", "AVRO"), wait = TRUE, autodetect = TRUE,
                nullMarker = NULL, maxBadRecords = NULL, allowJaggedRows = FALSE,
                allowQuotedNewlines = FALSE, fieldDelimiter = ",")
