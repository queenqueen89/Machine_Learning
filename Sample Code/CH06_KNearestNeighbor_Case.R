#' ---
#' title: "CHAPTER 6: K-NEAREST NEIGHBOR"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 11th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 6")

# Install the packages needed for this code file
# install.packages(c("DMwR","class"))

#----------------------------------------------------------------
#' #1. Importing the Data
#----------------------------------------------------------------
library(tidyverse)

donors <-
  read_csv("donors.csv", col_types = "nnnnnnnnnnnnffffffffff")

# Get a preview of the data
glimpse(donors)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Keep only the numeric features and the class.
donors <- donors %>%
  select(
    age,
    numberChildren,
    incomeRating,
    wealthRating,
    mailOrderPurchases,
    totalGivingAmount,
    numberGifts,
    smallestGiftAmount,
    largestGiftAmount,
    averageGiftAmount,
    yearsSinceFirstDonation,
    monthsSinceLastDonation,
    respondedMailing
  )

summary(donors)

## Deal with the missing values for age, numberChildren, incomeRating and wealthRating.
# There are a lot of missing values for age. We will use mean imputation to resolve the NAs.
donors <- donors %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age))

summary(select(donors, age))

# For numberChildren, we will use median imputation to resolve the NAs.
donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = TRUE), numberChildren))

summary(select(donors, numberChildren))

# For incomeRating and wealthRating, we simply exclude the NA records.
# Note that for wealthRating, we have some bad data as well.
#  The scale is 1-9, but our summary shows that we have some records with wealthRating of 0.
donors <- donors %>%
  filter(!is.na(incomeRating) & !is.na(wealthRating) & wealthRating > 0)

summary(select(donors, incomeRating, wealthRating))

## Normalize the data.
# We first create a min-max normalization function.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Then we apply the function to our numeric features.
donors <- donors %>%
  mutate(age = normalize(age)) %>%
  mutate(numberChildren = normalize(numberChildren)) %>%
  mutate(incomeRating = normalize(incomeRating)) %>%
  mutate(wealthRating = normalize(wealthRating)) %>%
  mutate(mailOrderPurchases = normalize(mailOrderPurchases)) %>%
  mutate(totalGivingAmount = normalize(totalGivingAmount)) %>%
  mutate(numberGifts = normalize(numberGifts)) %>%
  mutate(smallestGiftAmount = normalize(smallestGiftAmount)) %>%
  mutate(largestGiftAmount = normalize(largestGiftAmount)) %>%
  mutate(averageGiftAmount = normalize(averageGiftAmount)) %>%
  mutate(yearsSinceFirstDonation = normalize(yearsSinceFirstDonation)) %>%
  mutate(monthsSinceLastDonation = normalize(monthsSinceLastDonation))

# Another way to apply the normalize function.
donors <- donors %>%
  mutate_at(
    c(
      "age",
      "numberChildren",
      "incomeRating",
      "wealthRating",
      "mailOrderPurchases",
      "totalGivingAmount",
      "numberGifts",
      "smallestGiftAmount",
      "largestGiftAmount",
      "averageGiftAmount",
      "yearsSinceFirstDonation",
      "monthsSinceLastDonation"
    ),
    ~ normalize(.)
  )

summary(donors)

donors <- data.frame(donors)

# Using the sample() function, let's create our training and test datasets using a 75% to 25% split.
set.seed(1234)
sample_index <-
  sample(nrow(donors), round(nrow(donors) * .75), replace = FALSE)
donors_train <- donors[sample_index,]
donors_test <- donors[-sample_index,]

# Evaluate the class distribution for all 3 datasets.
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# Use the SMOTE() function from the DMwR package to balance the training data.
library(DMwR)
set.seed(1234)
donors_train <-
  SMOTE(respondedMailing ~ .,
        donors_train,
        perc.over = 100,
        perc.under = 200)

# Evaluate the class distribution for the training data.
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100

# Split the class labels from the training and test sets.
donors_train_labels <-as.factor(pull(donors_train, respondedMailing))
donors_test_labels <- as.factor(pull(donors_test, respondedMailing))

# Convert the training and test datasets to a data frame (without labels).
donors_train <- data.frame(select(donors_train,-respondedMailing))
donors_test <- data.frame(select(donors_test,-respondedMailing))

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
# Let's classify the test data with k set to 5.
library(class)

donors_pred <-
  knn(
    train = donors_train,
    test = donors_test,
    cl = donors_train_labels,
    k = 5
  )

# What are our first six predictions?
head(donors_pred)

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# Create confusion matrix of our results.
donors_pred_table <- table(donors_test_labels, donors_pred)
donors_pred_table

# What is the accuracy of our prediction?
sum(diag(donors_pred_table)) / nrow(donors_test)
