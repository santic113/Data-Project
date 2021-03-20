#Install libraries needed

install.packages('bigrquery')
library(bigrquery)
install.packages('bigQueryR')
library(bigQueryR)
install.packages('dplyr')
library(dplyr)
install.packages('neuralnet')
library(neuralnet)
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)
project_id = 'insert your project ID here '


#Query that connects to BQ to pull in the data
query =
  "
SELECT
   CASE WHEN UC.Chance_of_Admit_ >= .45 THEN 1 ELSE 0 END AS CHANCE_OF_ADMISSION
 , UC.GRE_Score AS GRE_SCORE
 , UC.TOEFL_Score AS TOEFL_SCORE
 , UC.University_Rating AS UNIVERSITY_RATING
 , UC.SOP
 , UC.LOR_ AS LOR
 , UC.CGPA
FROM
  `insert your table here` AS UC
"

#Executing the query
distinct_data_frame = query_exec(query, project = project_id, use_legacy_sql = FALSE)


#Viewing our table
data <- (distinct_data_frame)
View(data)

#Scale and analyze the data
max_data <- apply(data[,2:7], 2, max)
min_data <- apply(data[,2:7], 2, min)
data_scaled <- scale(data[,2:7],center = min_data, scale = max_data - min_data)
View(data_scaled)

#Create binary classifers for closed v not closed quotes
Binary = data$CHANCE_OF_ADMISSION
data_scaled <- cbind(Binary,data_scaled)
View(data_scaled)

#Splitting out the training data and the testing data. 70/30 split. Training 70 percent and testing on 30 percent =
index <- sample(1:nrow(data),round(0.70*nrow(data)))
View(index)
train_data <- as.data.frame(data_scaled[index,])
View(train_data)
test_data <- as.data.frame(data_scaled[-index,])

#Run the neural network and plot out. 4 layers, then 3 additional layers
n <- names(train_data)
(n)
f <- as.formula(paste("Binary ~", paste(n[!n %in% "Binary"], collapse = " + ")))
View(f)
deep_net <- neuralnet(f,data=train_data,hidden=c(4,3),linear.output=F,  stepmax = 1000000)

plot(deep_net)



#Analyze the results of our model
predicted_data <- neuralnet::compute(deep_net,test_data[,2:7])
print(head(predicted_data$net.result))
predicted =  predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)
predicted_values_threshold =  predicted_data$net.result
View(predicted_values_threshold)


#Viewing some of our results
deep_net$result.matrix
actual_test_data_table = (test_data$Binary)
predicted_data_results_table = (predicted_data$net.result)
predicted_values_threshold =  predicted_data$net.result
binded_table_actuals_test_prediction_thresholds = cbind(actual_test_data_table, predicted_data_results_table, predicted_values_threshold)
View(binded_table_actuals_test_prediction_thresholds)

#Create table to measure what the model predicted was and what was sold and was what was not sold
Results = table(test_data$Binary,predicted_data$net.result)

#Confusion Matrx for our Results
confusionMatrix(Results)
