#' ---
#' title: "CHAPTER 8: DECISION TREE"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "February 28th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 8")

# Install the packages needed for this code file
# install.packages(c("rpart","rpart.plot"))

# load the `tidyverse` package 
library(tidyverse)

#----------------------------------------------------------------
#' #1. Collect the Data
#----------------------------------------------------------------
# Load the data and use the `col_types` argument to specify type of 
# all columns('n' stands for numerical and 'f' stands for factor).
permits <- read_csv("permits.csv", col_types = "ffffffnnnnfffff")

# Get a preview of the data.
glimpse(permits)

#----------------------------------------------------------------
#' #2. Explore and Prepare the Data
#----------------------------------------------------------------
# Show a set of descriptive statistics for every variable in the data.
summary(permits)

# Fix the missing values issue.
# The mutate_at() function applies the same mutation to multiple variables at the same time.
permits <- permits %>%
  mutate_at(c('valuation', 'floorArea', 'numberUnits', 'stories'), ~ifelse(.< 1, NA, .))

# Fix the outliers issue.
permits <- permits %>%
  mutate(stories = ifelse(stories > 73, NA, stories))

summary(select(permits, valuation, floorArea, numberUnits, stories))

# Select only the first four features to build the decision tree.
permits <- permits %>%
  select(
    permitType,
    permitSubtype,
    initiatingOffice,
    permitCategory
  )

# Using the sample() function, let's create our training and test datasets with a 80% to 20% split.
# The set.seed() function is used to ensure that we can get the same result every time we run a random sampling process.
set.seed(1234)
sample_set <- sample(nrow(permits), round(nrow(permits)*.80), replace = FALSE)
permits_train <- permits[sample_set, ]
permits_test <- permits[-sample_set, ]

# Check the proportions for the class between all 3 sets.
round(prop.table(table(select(permits, permitCategory))),2)
round(prop.table(table(select(permits_train, permitCategory))),2)
round(prop.table(table(select(permits_test, permitCategory))),2)

#----------------------------------------------------------------
#' #3. Build the Model
#----------------------------------------------------------------
# The CART algorithm is in the `rpart` package.
library(rpart)

# Build the classification tree with permitcategory as the outcome variable and all other variables as predictors.
permits_mod <-
  rpart(
    permitCategory ~ .,
    method = "class",
    data = permits_train
  )

# Plot the decision tree using the rpart.plot() function from the rpart.plot library. (Figure 8.7)
library(rpart.plot)
rpart.plot(permits_mod)

#----------------------------------------------------------------
#' #4. Evaluate the Model's Performance
#----------------------------------------------------------------
# Make predictions using our tree model against the test set.
permits_pred <- predict(permits_mod, permits_test, type = "class")
head(permits_pred)

# Create confusion matrix of our results.
permits_pred_table <- table(permits_test$permitCategory, permits_pred)
permits_pred_table

# What is the accuracy of our prediction?
sum(diag(permits_pred_table)) / nrow(permits_test)