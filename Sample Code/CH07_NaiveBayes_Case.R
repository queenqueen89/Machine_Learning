#' ---
#' title: "CHAPTER 7: NAIVE BAYES"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 14th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 7")

# Install the packages needed for this code file
# install.packages("e1071")

#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
library(tidyverse)

# Load the data and use the `col_types` argument to specify type of 
# all columns('n' stands for numerical and 'f' stands for factor).
heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")

# Get a glimpse of the data.
glimpse(heart)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Show a set of descriptive statistics for every variable in the data.
summary(heart)

# Using the sample() function, let's create our training and test datasets with a 75% to 25% split.
# The set.seed() function ensures to get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(heart), round(nrow(heart)*.75), replace = FALSE)
heart_train <- heart[sample_set, ]
heart_test <- heart[-sample_set, ]

# Check the proportions for the class between all 3 sets.
round(prop.table(table(select(heart, heartDisease))),2)
round(prop.table(table(select(heart_train, heartDisease))),2)
round(prop.table(table(select(heart_test, heartDisease))),2)

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
library(e1071)

# Train a new model using the naiveBayes() function.
heart_mod <- naiveBayes(heartDisease ~ ., data = heart_train, laplace = 1)
heart_mod

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# Use the model to predict the class of the test instances.
heart_pred <- predict(heart_mod, heart_test, type = "class")
head(heart_pred)

# Create confusion matrix of our results.
heart_pred_table <- table(heart_test$heartDisease, heart_pred)
heart_pred_table

# What is the accuracy of our prediction?
sum(diag(heart_pred_table)) / nrow(heart_test)

