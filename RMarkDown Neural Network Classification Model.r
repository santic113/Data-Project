---
title: "Neural Network Classification Model"
author: "Santiago Canon"
date: "12/28/2019"
output:
  html_document:
    df_print: paged
keep_md: yes
---

```{r setup, include=FALSE}
library("neuralnet")
library(ISLR)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
data <- College
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
#Scale and analyze the data
max_data <- apply(data[,2:18], 2, max)
min_data <- apply(data[,2:18], 2, min)
data_scaled <- scale(data[,2:18],center = min_data, scale = max_data - min_data)

#Create binary classifers for private v public colleges
Private <- as.numeric(College$Private)-1
data_scaled <- cbind(Private,data_scaled)

#Splitting out the training data and the testing data. I split out the data 70% training and 30% testing.
index <- sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])
```


```{r}
#Run the neural network and plot out
n <- names(train_data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
deep_net <- neuralnet(f,data=train_data,hidden=c(5,3),linear.output=F)
```


```{r}
plot(deep_net, rep = 'best')
```

```{r}
#Analyze the results of our model
predicted_data <- compute(deep_net,test_data[,2:18])
print(head(predicted_data$net.result))
predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)
```


```{r}

#Create table to measure what the model predicted was and was not a private or public college
table(test_data$Private,predicted_data$net.result)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
