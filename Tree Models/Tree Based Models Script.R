#Introduction to Machine Learning Data Camp
install.packages('wage')
library(wage)
help(rpart)


##Creating  a 80/20 training and testing data


# Total number of rows in the credit data frame
n <- nrow(credit)

# Number of rows for the training set (80% of the dataset)
n_train <- round(.80 * n)

# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)

# Subset the credit data frame to training indices only
credit_train <- credit[train_indices, ]

# Exclude the training indices to create the test set
credit_test <- credit[-train_indices, ]


# Train the model (to predict 'default')
credit_model <- rpart(formula = default ~ .,
                      data = credit_train,
                      method = "class")

# Display the results
rpart.plot(x = credit_model, yesno = 2, type = 0, extra = 0)

# Look at the model output
print(credit_model)

# Generate predicted classes using the model object
class_prediction <- predict(object = credit_model,
                            newdata = credit_test,
                            type = "class")

# Calculate the confusion matrix for the test set
confusionMatrix(data = class_prediction,
                reference = credit_test$default)


# Train a gini-based model
credit_model1 <- rpart(formula = default ~ .,
                       data = credit_train,
                       method = "class",
                       parms = list(split = 'gini'))

# Train an information-based model
credit_model2 <- rpart(formula = default ~ .,
                       data = credit_train,
                       method = "class",
                       parms = list(split = 'information'))

# Generate predictions on the validation set using the gini model
pred1 <- predict(object = credit_model1,
                 newdata = credit_test,
                 type = 'class')

# Generate predictions on the validation set using the information model
pred2 <- predict(object = credit_model2,
                 newdata = credit_test,
                 type = 'class')

# Compare classification error
ce(actual = credit_test$default,
   predicted = pred1)
ce(actual = credit_test$default,
   predicted = pred2)

########################################################################################################################

#Using Regression Trees
#Splitting the data into three sets:  training, validation, & tesiing

# Look at the data
str(grade)

# Set seed and create assignment
set.seed(1)

assignment <- sample(1:3, size = nrow(grade), prob = c(0.7,.15,.15), replace = TRUE)
assignment
# Create a train, validation and tests from the original data frame
grade_train <- grade[assignment == 1, ]    # subset grade to training indices only
grade_valid <- grade[assignment == 2, ]  # subset grade to validation indices only
grade_test <- grade[assignment == 3, ]   # subset grade to test indices only


#Creating the regression tree model using "anova" and printing out the tree
# Train the model
grade_model <- rpart(formula = final_grade ~ .,
                     data = grade_train,
                     method = "anova")

# Look at the model output
print(grade_model)

# Plot the tree model
rpart.plot(x = grade_model, yesno = 2, type = 0, extra = 0)


# Generate predictions on a test set
pred <- predict(object = grade_model,   # model object
                newdata = grade_test)  # test dataset

# Compute the RMSE
rmse(actual = grade_test$final_grade,
     predicted = pred)


#Optimizing our model using the complexity parameter

# Plot the "CP Table"
plotcp(grade_model)

# Print the "CP Table"
print(grade_model$cptable)

# Retrieve optimal cp value based on cross-validated error
opt_index <- which.min(grade_model$cptable[, "xerror"])
cp_opt <- grade_model$cptable[opt_index, "CP"]

# Prune the model (to optimized cp value)
grade_model_opt <- prune(tree = grade_model,
                         cp = cp_opt)

# Plot the optimized model
rpart.plot(x = grade_model_opt, yesno = 2, type = 0, extra = 0)


####################################################################################################################
#Generate Grid of hyper parameter values

# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)


# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
grade_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {

  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]

  # Train a model and store in the list
  grade_models[[i]] <- rpart(formula = final_grade ~ .,
                             data = grade_train,
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}


#Selecting the best model

# Number of potential models in the grid
num_models <- length(grade_models)

# Create an empty vector to store RMSE values
rmse_values <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:num_models) {

  # Retrieve the i^th model from the list
  model <- grade_models[[i]]

  # Generate predictions on grade_valid
  pred <- predict(object = model,
                  newdata = grade_valid)

  # Compute validation RMSE and add to the
  rmse_values[i] <- rmse(actual = grade_valid$final_grade,
                         predicted = pred)
}

# Identify the model with smallest validation set RMSE
best_model <- grade_models[[which.min(rmse_values)]]

# Print the model paramters of the best model
best_model$control

# Compute test set RMSE on best_model
pred <- predict(object = best_model,
                newdata = grade_test)
rmse(actual = grade_test$final_grade,
     predicted = pred)
#######################################################################################################################

#Using Bagging for Decision Trees


# Bagging is a randomized model, so let's set a seed (123) for reproducibility
set.seed(123)

# Train a bagged model
credit_model <- bagging(formula = default ~ .,
                        data = credit_train,
                        coob = TRUE)
str(credit_train)


# Print the model
print(credit_model)

# Generate predicted classes using the model object
class_prediction <- predict(object = credit_model,
                            newdata = credit_test,
                            type = "class")  # return classification labels

# Print the predicted classes
print(class_prediction)

# Calculate the confusion matrix for the test set
confusionMatrix(data = class_prediction,
                reference = credit_test$default)

# Generate predictions on the test set
pred <- predict(object = credit_model,
                newdata = credit_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)

# Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
auc(actual = ifelse(credit_test$default == "yes", 1, 0),
    predicted = pred[,"yes"])


######################################
#Cross-validating our data
# Specify the training configuration
ctrl <- trainControl(method = "cv",     # Cross-validation
                     number = 5,      # 5 folds
                     classProbs = TRUE,                  # For AUC
                     summaryFunction = twoClassSummary)  # For AUC

# Cross validate the credit model using "treebag" method;
# Track AUC (Area under the ROC curve)
set.seed(1)  # for reproducibility
credit_caret_model <- train(default ~ .,
                            data = credit_train,
                            method = "treebag",
                            metric = "ROC",
                            trControl = ctrl)

# Look at the model object
print(credit_caret_model)

# Inspect the contents of the model list
names(credit_caret_model)

# Print the CV AUC
credit_caret_model$results[,"ROC"]

# Print ipred::bagging test set AUC estimate
print(credit_ipred_model_test_auc)

# Print caret "treebag" test set AUC estimate
print(credit_caret_model_test_auc)

# Compare to caret 5-fold cross-validated AUC
credit_caret_model$results[,"ROC"]

################################################################################################

#Random Forest Models & OOB Error

# Train a Random Forest
set.seed(1)  # for reproducibility
credit_model <- randomForest(formula = default ~ .,
                             data = credit_train)

# Print the model output
print(credit_model)

# Grab OOB error matrix & take a look
err <- credit_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(err)

# Add a legend since it doesn't have one by default
legend(x = "right",
       legend = colnames(err),
       fill = 1:ncol(err))

# Generate predicted classes using the model object
class_prediction <- predict(object = credit_model,   # model object
                            newdata = credit_test,  # test dataset
                            type = "class") # return classification labels

# Calculate the confusion matrix for the test set
cm <- confusionMatrix(data = class_prediction,       # predicted classes
                      reference = credit_test$default)  # actual classes
print(cm)

# Grab OOB error matrix & take a look
err <- credit_model$err.rate
head(err)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)

# Plot the model trained in the previous exercise
plot(credit_model)


# Since we converted the training response col, let's also convert the test response col
credit_test$default <- ifelse(credit_test$default == "yes", 1, 0)

# Generate predictions on the test set
preds1 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 10000)

# Generate predictions on the test set (scale to response)
preds2 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 10000,
                  type = "response")

# Compare the range of the two sets of predictions
range(preds1)
range(preds2)

# Add a legend since it doesn't have one by default
legend(x = "right",
       legend = colnames(err),
       fill = 1:ncol(err))

# Compare test set accuracy to OOB accuracy
paste0("Test Accuracy: ", cm$overall[1])
paste0("OOB Accuracy: ", 1 - oob_err)

# Generate predictions on the test set
pred <- predict(object = credit_model,
                newdata = credit_test,
                type = "prob")

# `pred` is a matrix
class(pred)

# Look at the pred format
head(pred)

# Compute the AUC (`actual` must be a binary 1/0 numeric vector)
auc(actual = ifelse(credit_test$default == "yes", 1, 0),
    predicted = pred[,"yes"])

# Execute the tuning process
set.seed(1)
res <- tuneRF(x = subset(credit_train, select = -default),
              y = credit_train$default,
              ntreeTry = 500)

# Look at results
print(res)

# Find the mtry value that minimizes OOB Error
mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
print(mtry_opt)

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.


# Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(credit_train) * 0.8, 2)
nodesize <- seq(3, 8, 2)
sampsize <- nrow(credit_train) * c(0.7, 0.8)

# Create a data frame containing all combinations
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {

  # Train a Random Forest model
  model <- randomForest(formula = default ~ .,
                        data = credit_train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])

  # Store OOB error for the model
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

####################################################################################################################

#GBM Model

# Convert "yes" to 1, "no" to 0
credit_train$default <- ifelse(credit_train$default == "yes", 1, 0)

# Train a 10000-tree GBM model
set.seed(1)
credit_model <- gbm(formula = default ~ .,
                    distribution = 'bernoulli',
                    data = credit_train,
                    n.trees = 10000)

# Print the model object
print(credit_model)

# summary() prints variable importance
summary(credit_model)


# Since we converted the training response col, let's also convert the test response col
credit_test$default <- ifelse(credit_test$default == "yes", 1, 0)

# Generate predictions on the test set
preds1 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 10000)

# Generate predictions on the test set (scale to response)
preds2 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 10000,
                  type = "response")

# Compare the range of the two sets of predictions
range(preds1)
range(preds2)

# Generate the test set AUCs using the two sets of preditions & compare
auc(actual = credit_test$default, predicted = preds1)  #default
auc(actual = credit_test$default, predicted = preds2)  #rescaled


# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = credit_model,
                          method = "OOB",
                          oobag.curve = TRUE)

# Train a CV GBM model
set.seed(1)
credit_model_cv <- gbm(formula = default ~ .,
                       distribution = "bernoulli",
                       data = credit_train,
                       n.trees = 10000,
                       cv.folds = 2,
                       n.cores = 1)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = credit_model_cv,
                         method = "cv")

# Compare the estimates
print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))
print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_oob number of trees
preds1 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 3233 )

# Generate the test set AUCs using the two sets of preditions & compare
auc1 <- auc(actual = credit_test$default, predicted = preds1)  #OOB
auc2 <- auc(actual = credit_test$default, predicted = preds2)  #CV

# Compare AUC
print(paste0("Test set AUC (OOB): ", auc1))
print(paste0("Test set AUC (CV): ", auc2))


# Generate predictions on the test set using ntree_opt_oob number of trees
preds1 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = ntree_opt_oob)

# Generate predictions on the test set using ntree_opt_cv number of trees
preds2 <- predict(object = credit_model,
                  newdata = credit_test,
                  n.trees = 3233 )

# Generate the test set AUCs using the two sets of preditions & compare
auc1 <- auc(actual = credit_test$default, predicted = preds1)  #OOB
auc2 <- auc(actual = credit_test$default, predicted = preds2)  #CV

# Compare AUC
print(paste0("Test set AUC (OOB): ", auc1))
print(paste0("Test set AUC (CV): ", auc2))


# Generate the test set AUCs using the two sets of predictions & compare
actual <- credit_test$default
dt_auc <- auc(actual = actual, predicted = dt_preds)
bag_auc <- auc(actual = actual, predicted = bag_preds)
rf_auc <- auc(actual = actual, predicted = rf_preds)
gbm_auc <- auc(actual = actual, predicted = gbm_preds)

# Print results
sprintf("Decision Tree Test AUC: %.3f", dt_auc)
sprintf("Bagged Trees Test AUC: %.3f", bag_auc)
sprintf("Random Forest Test AUC: %.3f", rf_auc)
sprintf("GBM Test AUC: %.3f", gbm_auc)


# List of predictions
preds_list <- list(dt_preds, bag_preds, rf_preds, gbm_preds)

# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(credit_test$default), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright",
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)
