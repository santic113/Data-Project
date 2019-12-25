#Install packages & libraries required for nerual net and the data we are using...
install.packages('neuralnet')
library("neuralnet")

#Packages that contain the college data set we are using for this ecample
install.packages('ISLR')
library(ISLR)

#test
data <- College
View(data)

#Scale and analyze the data
max_data <- apply(data[,2:18], 2, max)
min_data <- apply(data[,2:18], 2, min)
data_scaled <- scale(data[,2:18],center = min_data, scale = max_data - min_data)

#Create binary classifers for private v public colleges
Private <- as.numeric(College$Private)-1
data_scaled <- cbind(Private,data_scaled)

#Splitting out the training data and the testing data
index <- sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

#Run the neural network and plot out
n <- names(train_data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
deep_net <- neuralnet(f,data=train_data,hidden=c(5,3),linear.output=F)
plot(deep_net)

#Analyze the results of our model
predicted_data <- compute(deep_net,test_data[,2:18])
print(head(predicted_data$net.result))
predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)

#Create table to measure what the model predicted was and was not a private or public college
table(test_data$Private,predicted_data$net.result)
