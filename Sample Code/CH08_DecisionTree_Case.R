#' ---
#' title: "CHAPTER 8: DECISION TREE"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 28th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 8")

# Install the packages needed for this code file
# install.packages(c("rpart","rpart.plot","DMwR"))

# load the `tidyverse` package 
library(tidyverse)

#----------------------------------------------------------------
#' #1. Collect the Data
#----------------------------------------------------------------
# Import the income data set from your working directory.
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# Get a preview of the data.
glimpse(income)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Show a set of descriptive statistics for every variable in the data.
summary(income)

# Using the sample() function, let's create our training and test datasets with a 75% to 25% split.
# The set.seed() function ensures to get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(income), round(nrow(income)*.75), replace = FALSE)
income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

# What is the class distribution?
# Check the proportions for the class between all 3 datasets.
round(prop.table(table(select(income, income), exclude = NULL)), 4) * 100
round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100
round(prop.table(table(select(income_test, income), exclude = NULL)), 4) * 100

# From the proportion tables, we learn that our data is imbalanced.
# To make our model more generalizable, we need to learn with a more balanced data set.
# We use the SMOTE() function from the DMwR package to generate a new balanced training data.
library(DMwR)
set.seed(1234)
income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

# Check the proportions for the training data.
round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
# Build the classification tree with income as the outcome variable and all other variables as predictors.
library(rpart)
income_mod <-
  rpart(
    income ~ .,
    method = "class",
    data = income_train
  )

# Plot the decision tree using the rpart.plot() function from the rpart.plot library. (Figure 8.8)
library(rpart.plot)
rpart.plot(income_mod)

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# Make predictions using our tree model against the test set.
income_pred <- predict(income_mod, income_test, type = "class")
head(income_pred)

# Create confusion matrix of our results.
income_pred_table <- table(income_test$income, income_pred)
income_pred_table

# What is the accuracy of our prediction?
sum(diag(income_pred_table)) / nrow(income_test)