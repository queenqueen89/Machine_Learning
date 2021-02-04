#' ---
#' title: "CHAPTER 6: K-NEAREST NEIGHBOR"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 11th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 6")

# Install the packages needed for this code file.
# install.packages(c("dummies","class"))

#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
library(tidyverse)

# Load the data and use the `col_types` argument to specify type of 
# all columns('n' stands for numerical and 'f' stands for factor).
heart <- read_csv("heart.csv", col_types = "nffnnffnfnfnff")

# Get a preview of the data.
glimpse(heart)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Let's take a look at the summary stats for the heart dataset.
summary(heart)

## Deal with missing Values
# We are working with medical data, so we want to be conservative with imputation.
# Instead of imputing the missing values, we exclude them from our dataset.
heart <- heart %>%
  filter(
    !is.na(restingBP) &
      !is.na(cholesterol) &
      !is.na(highBloodSugar) &
      !is.na(restingECG) &
      !is.na(restingHR) &
      !is.na(exerciseAngina) &
      !is.na(STdepression) &
      !is.na(STslope) &
      !is.na(coloredVessels) &
      !is.na(defectType)
  )

# Alternatively, we can use filter_at() to process multiple variables at one time.
heart <- heart %>%
  filter_at(
    c(
      "restingBP",
      "cholesterol",
      "highBloodSugar",
      "restingECG",
      "restingHR",
      "exerciseAngina",
      "STdepression",
      "STslope",
      "coloredVessels",
      "defectType"
    ),
    ~ !is.na(.)
  )

## Normalize the data
# The next thing we need to do is normalize the data for our numeric features.
# We first create a min-max normalization function.
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Then we apply the function to our numeric features.
heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))

# We can also use mutate_at() to apply the same function on multiple variables at one time.
heart <- heart %>%
  mutate_at(
    c(
      "age",
      "restingBP",
      "cholesterol",
      "restingHR",
      "STdepression",
      "coloredVessels"
    ),
    ~ normalize(.)
  ) 
  
# What do we have now?
summary(heart)


## Deal with Categorical Features
# Before we do so, let's convert the heart dataset to a data frame...
heart <- data.frame(heart)

# ...and split our labels from the rest of our data.
heart_labels <- heart %>% select(heartDisease)
heart <- heart %>% select(-heartDisease)

# Show the current list of features.
colnames(heart)

# Let's use the dummy.data.frame() function from the dummies package to create dummy variables for our data.
library(dummies)

# Now we can create our dummy variables.
heart <- dummy.data.frame(data = heart, sep = "_")

# Show the new list of features (including the dummy variables).
colnames(heart)

# Using the sample() function, let's create our training and test datasets using a 75% to 25% split.
# The use of set.seed() ensures that every time we run our code, we get the same results.
set.seed(1234)
sample_index <-
  sample(nrow(heart), round(nrow(heart) * .75), replace = FALSE)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

# We do the same for the class labels.
heart_train_labels <- as.factor(heart_labels[sample_index,])
heart_test_labels <- as.factor(heart_labels[-sample_index,])

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
# Let's classify the test data with k set to 15 (approx. the square root of 224).
# To do this, we use the knn() function from the class package.
library(class)

heart_pred1 <-
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 15
  )

# What are our first six predictions?
head(heart_pred1)

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# Create confusion matrix of our results.
heart_pred1_table <- table(heart_test_labels, heart_pred1)
heart_pred1_table

# What is the accuracy of our prediction?
sum(diag(heart_pred1_table)) / nrow(heart_test)

#----------------------------------------------------------------
#' #5. Improve the Model's Performance
#----------------------------------------------------------------
# Use k=1
heart_pred2 <-
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 1
  )

heart_pred2_table <- table(heart_test_labels, heart_pred2)

# What is the accuracy of our prediction?
sum(diag(heart_pred2_table)) / nrow(heart_test)

# Use k=40
heart_pred3 <-
  knn(
    train = heart_train,
    test = heart_test,
    cl = heart_train_labels,
    k = 40
  )

heart_pred3_table <- table(heart_test_labels, heart_pred3)

# What is the accuracy of our prediction?
sum(diag(heart_pred3_table)) / nrow(heart_test)
