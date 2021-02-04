#' ---
#' title: "CHAPTER 9: EVALUATING PERFORMANCE"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 2nd, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 9")

# Install the packages needed for this code file
# install.packages(c("caret","DMwR","rpart","ROCR","e1071"))

# load the `tidyverse` package 
library(tidyverse)

#----------------------------------------------------------------
#' #1. Estimating Future Performance
#----------------------------------------------------------------
# Import the income data set from your working directory.
# Use the `col_types` argument to specify type of all columns ('n' stands for numerical and 'f' stands for factor).
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# Get a preview of the data.
glimpse(income)

library(caret)
# The set.seed() function ensures that we can get the same result every time we run a random sampling process.
set.seed(1234)
# Using the createDataPartition() function, we partition the data.
sample_set <-
  createDataPartition(y = income$income, p = .75, list = FALSE)
income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

# We use the SMOTE() function from the DMwR package to generate a new balanced training data.
library(DMwR)
set.seed(1234)
income_train <-
  SMOTE(income ~ .,
        data.frame(income_train),
        perc.over = 100,
        perc.under = 200)

## k-fold cross validation
# Here we specify Accuracy as the metric to evaluate the model, rpart as the
# training algorithm and 5-fold cross validation as the resampling method.
library(rpart)
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)

# Sort the result by the Resample column.
income_mod$resample %>%
  arrange(Resample)

# Get the average accuracy for all iterations
income_mod$resample %>%
  arrange(Resample) %>%
  summarise(AvgAccuracy = mean(Accuracy))

## Leave-One-Out cross validation
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "LOOCV")
)

income_mod$resample %>%
  arrange(Resample)

## Random/Monte Carlo cross validation
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "LGOCV", p = .1, number = 10)
)

income_mod$resample %>%
  arrange(Resample)

## Bootstrapping
set.seed(1234)
income_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3)
)

income_mod$resample %>%
  arrange(Resample)

#----------------------------------------------------------------
#' #2. Beyond Predictive Accuracy
#----------------------------------------------------------------
# .RData files are specific to R and can store as many objects as you'd like within a single file.
load("spam.RData")

# So far, we've used the table() function to create the confusion matrix for our models.
# We can also do the same using the confusionMatrix() function from the caret package.
# The output from this function gives some additional measures to consider.
spam_matrix <-
  confusionMatrix(email_pred, email_test$message_label, positive = "spam")
spam_matrix

# We can get the Accuracy and Kappa stats from the confusion matrix by using the 'overall' attribute.
spam_accuracy <- as.numeric(spam_matrix$overall["Accuracy"])
spam_accuracy

spam_kappa <- as.numeric(spam_matrix$overall["Kappa"])
spam_kappa


# We can get Sensitivity, Specificity, Precision and Recall directly using different functions.
spam_sensitivity <-
  sensitivity(email_pred, email_test$message_label, positive = "spam")
spam_sensitivity

spam_specificity <-
  specificity(email_pred, email_test$message_label, negative = "ham")
spam_specificity

spam_precision <-
  posPredValue(email_pred, email_test$message_label, positive = "spam")
spam_precision

spam_recall <- spam_sensitivity
spam_recall

# We can also compute the f-score based on precision and recall values.
spam_fmeasure <-
  (2 * spam_precision * spam_recall) / (spam_precision + spam_recall)
spam_fmeasure

# Note the use of the 'positive' and 'negative' parameters above.

#----------------------------------------------------------------
#' #3. Visualizing Model Performance
#----------------------------------------------------------------
# Visualizations, such as the ROC curve, are useful in performance evaluation.
# To generate an ROC curve from our predictions, we use the ROCR package.
library(ROCR)

# First, we need to create a prediction object. 
# This requires us to use the predicted probabilities of our model.
library(e1071)
email_pred_prob <- predict(email_mod, email_test,  type = "raw")
head(email_pred_prob)

# Now we can create our prediction object.
roc_pred <-
  prediction(
    predictions = email_pred_prob[, "spam"],
    labels = email_test$message_label
  )

# Then we can create the performance object.
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

# Now, we can plot the ROC curve.
plot(roc_perf, main = "ROC Curve", col = "green", lwd = 3)

# With a reference line.
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

# Using the performance object, we can get the Area Under the Curve (AUC)
auc_perf <- performance(roc_pred, measure = "auc")
spam_auc <- unlist(slot(auc_perf,"y.values"))
spam_auc

# We can also create ROC Curve with ggplot.
roc_data <- data.frame(cbind(y=unlist(slot(roc_perf,"y.values")), x=unlist(slot(roc_perf,"x.values"))))

roc_data %>%
  ggplot() +
  geom_line(mapping = aes(x=x,y=y),color="green",size=1)+
  geom_abline(slope=1, intercept=0, linetype="dashed",size=1)+
  labs(title="ROC Curve",
       x ="False positive rate", y = "True positive Rate")+
  theme_minimal()
