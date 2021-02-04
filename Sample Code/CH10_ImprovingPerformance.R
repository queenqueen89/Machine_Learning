#' ---
#' title: "CHAPTER 10: IMPROVING PERFORMANCE"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 3rd, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 10")

# Install the packages needed for this code file
# install.packages(c("caret","rpart","DMwR","randomForest","caretEnsemble"))

#----------------------------------------------------------------
#' #1. Automated Parameter Tuning
#----------------------------------------------------------------
# Load the packages
library(caret)
library(rpart)
library(tidyverse)

# Check the available parameters.
modelLookup("rpart")

# Load the data and use the `col_types` argument to specify type of 
# all columns ('n' stands for numerical and 'f' stands for factor).
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# The set.seed() function ensures to get the same result every time we run a random sampling process.
set.seed(1234)
# Using the createDataPartition() function, we partition the data with a 75% to 25% split.
sample_set <-
  createDataPartition(y = income$income, p = .75, list = FALSE)
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

# We use the SMOTE() function from the DMwR package to generate a new balanced training data.
library(DMwR)
set.seed(1234)
income_train <-
  SMOTE(income ~ .,
        data.frame(income_train),
        perc.over = 100,
        perc.under = 200)

# Here we specify Accuracy as the metric to evaluate the model, rpart as the 
# training algorithm and 0.632 bootstrap as the resampling method.
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3)
)
income_mod

# Use the optimal model to create our prediction on the test data.
income_pred <- predict(income_mod, income_test)

# Generate the performance metrics to evaluate how well the model performs against the test data.
# We can use the `positive`` argument to specify the positive class as '<=50k'.
confusionMatrix(income_pred, income_test$income, positive = "<=50K")

#----------------------------------------------------------------
#' #2. Customized Parameter Tuning
#----------------------------------------------------------------
set.seed(1234)
# By default, the number of cp values evaluated is 3.
# We can specify the tuneLength as 20 to increase the number of cp values from 3 to 20.
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneLength = 20
)
income_mod

# Create a parameter grid for all possible values of alpha, beta and gamma.
expand.grid(
  .alpha = c(1, 2, 3),
  .beta = c(TRUE, FALSE),
  .gamma = seq(from = 4, to = 5, by = 0.5)
)

# Similarly, we can create a parameter grid for cp.
expand.grid(.cp = seq(from = 0.0001, to = 0.002, by = 0.0001))

# To put it all together, we can customize our parameter tuning process.
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3),
  tuneGrid = expand.grid(.cp = seq(
    from = 0.0001, to = 0.002, by = 0.0001
  ))
)
income_mod

# Create our prediction on the test data.
income_pred <- predict(income_mod, income_test)

# Let's see how our model performs.
confusionMatrix(income_pred, income_test$income, positive = "<=50K")

#----------------------------------------------------------------
#' #3. Ensembles - Random Forests (Bagging)
#----------------------------------------------------------------
library(randomForest)

# Check the available parameters for random forest.
modelLookup("rf")

# Specify Accuracy as the evaluation metric, random forest as the 
# training algorithm, no hyperparameter tuning and no resampling.
set.seed(1234)
rf_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rf",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(.mtry = 3)
)

rf_pred <- predict(rf_mod, income_test)

# Let's see how the random forest model performs.
confusionMatrix(rf_pred, income_test$income, positive = "<=50K")

#----------------------------------------------------------------
#' #4. Ensembles - Extreme Gradient Boosting (Boosting)
#----------------------------------------------------------------
library(xgboost)

# Check the available parameters for xgboost.
modelLookup("xgbTree")

# Specify Accuracy as the evaluation metric, xgboost as the 
# training algorithm, no hyperparameter tuning and no resampling.
set.seed(1234)
xgb_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta =  0.3,
    gamma = 0.01,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

xgb_pred <- predict(xgb_mod, income_test)

# Let's see how the XGBoost model performs.
confusionMatrix(xgb_pred, income_test$income, positive = "<=50K")

#----------------------------------------------------------------
#' #5. Ensembles - Stacking
#----------------------------------------------------------------
# Use recode() function to change the income value from starting with a special character.
income <- income %>%
  mutate(income = as.factor(recode(income, "<=50K" = "Below", ">50K" = "Above")))

# Check whether the data has been changed successfully.
glimpse(income)

# Using the createDataPartition() function, we partition the data.
set.seed(1234)
sample_set <-
  createDataPartition(y = income$income, p = .75, list = FALSE)
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

# We use the SMOTE() function from the DMwR package to generate a new balanced training data.
set.seed(1234)
income_train <-
  SMOTE(income ~ .,
        data.frame(income_train),
        perc.over = 100,
        perc.under = 200)

library(caretEnsemble)

# Create a list of the learners that we plan to put into our emsemble model.
ensembleLearners <- c("rpart", "glm", "knn")


# Specify the list of learners we want to use, choose 5 times repeated 10-fold cross-validation 
# as the resampling method, and save the class probabilities and predictions.
models <- caretList(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  methodList = ensembleLearners,
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    savePredictions = "final",
    classProbs = TRUE
  )
)

# Collect results from each chosen model.
results <- resamples(models)
summary(results)

# Evaluate correlation between models
modelCor(results)

# Combine the predictions using a random forest classifier.
stack_mod <- caretStack(
  models,
  method = "rf",
  metric = "Accuracy",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    savePredictions = "final",
    classProbs = TRUE
  )
)

stack_pred <- predict(stack_mod, income_test)

# Let's see how the stacking model performs.
confusionMatrix(stack_pred, income_test$income, positive = "Below")
