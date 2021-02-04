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

# Load the data
email <- read_csv("email.csv")

# Get a glimpse of the data.
head(email)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Convert the message label to a factor.
email <- email %>%
  mutate(message_label = as.factor(message_label))

# Use gather() function to pivot the columns of our data into rows.
email %>%
  gather(word, count,-message_index, -message_label)

# Get top occurring words in the dataset.
email %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Get top occurring words among ham messages.
email %>%
  filter(message_label=='ham') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Get top occurring words among spam messages.
email %>%
  filter(message_label=='spam') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)


# Using the sample() function, let's create our training and test datasets.
# The set.seed() function ensures to get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(email), round(nrow(email)*.75), replace = FALSE)
email_train <- email[sample_set, ]
email_test <- email[-sample_set, ]

# Check the proportions for the class between all 3 sets.
round(prop.table(table(select(email, message_label))),2)
round(prop.table(table(select(email_train, message_label))),2)
round(prop.table(table(select(email_test, message_label))),2)

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
library(e1071)

# Using the naiveBayes() function in the e1071 package, we build up our naive bayes model.
# We use all variables except message_index to predict the message_label.
# The laplace argument is the the pseudocount value that should be used for Laplace smoothing.
email_mod <-
  naiveBayes(message_label ~ . - message_index,
             data = email_train,
             laplace = 1)

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# To predict the probability of spam or ham, we set type="raw".
email_pred <- predict(email_mod, email_test, type = "raw")

# Get a glimpse of the predictions.
head(email_pred)

# To get the predicted class labels, we set type="class".
email_pred <- predict(email_mod, email_test, type = "class")
head(email_pred)

# Create confusion matrix of our results.
email_pred_table <- table(email_test$message_label, email_pred)
email_pred_table

# What is the accuracy of our prediction?
sum(diag(email_pred_table)) / nrow(email_test)
