#' ---
#' title: "CHAPTER 3: MANAGING THE DATA"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "January 25th, 2020"
#' ---

# Change your working directory by specifing the path to the desired folder
# Below is an example.
# setwd("G:/My Drive/RA/Chapter 3")

# Install the packages needed for this code file
# install.packages(c("tidyverse","dummies","caTools"))

library(tidyverse)

#----------------------------------------------------------------
#' #1. Load the Data
#----------------------------------------------------------------
# Load the data and use the `col_types` argument to specify type of 
# all columns('n' stands for numerical and 'f' stands for factor).
vehicles <-
  read_csv(file = 'vehicles.csv', col_types = "nnnfnfffffnn")

# Now let's take a look at what our data looks like.
glimpse(vehicles)
head(vehicles)

#----------------------------------------------------------------
#' #2. Describe the Data
#----------------------------------------------------------------
# Let's try to describe our data.

# Get summary statistics of the data.
summary(vehicles)

# Select specific columns in the data and get the summary of the selected columns.
select(vehicles, class)
select(vehicles, class, cylinders)

summary(select(vehicles, class, cylinders))

# Get all the values and their associated counts for categorical features.
table(select(vehicles, class))

# Get the proportional distribution for all values in the categorical variable.
prop.table(table(select(vehicles, class)))

# We can use a pipe (%>%) to control the logical flow of our code.
vehicles %>%
  select(class) %>%
  table() %>%
  prop.table()

# Use filter() to specify logical conditions for rows.
vehicles %>%
  filter(drive == "2-Wheel Drive") %>%
  select(co2emissions) %>%
  summary()

#----------------------------------------------------------------
#' #3. Visualize the Data
#----------------------------------------------------------------
# Let's do some visual exploration against our data.

# What does the boxplot of emissions by class type tell us?
# Noted that The themes are used to customize the non-data components of your 
# plots: i.e. titles, labels, fonts, background, gridlines, and legends. 
vehicles %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = class, y = co2emissions), fill = "red") +
  labs(title = "Boxplot of C02 Emissions by Vehicle Class", x = "Class", y = "C02 Emissions") +
  theme_minimal() +
  theme(text = element_text(size = 14))

# Create a scatterplot of emissions vs. cylinders.
vehicles %>%
  ggplot() +
  geom_point(
    mapping = aes(x = citympg, y = co2emissions),
    color = "blue",
    size = 2
  ) +
  labs(title = "Scatterplot of CO2 Emissions vs. City Miles per Gallon",
       x = "City MPG",
       y = "CO2 Emissions") +
  theme_minimal() + 
  theme(text = element_text(size = 14))

# Create a histogram of car price.
vehicles %>%
  ggplot() +
  geom_histogram(
    mapping = aes(x = co2emissions),
    bins = 30,
    fill = "yellow",
    color = "black"
  ) +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Frequency") +
  theme_minimal() + 
  theme(text = element_text(size = 14))

# Create a stacked bar chart of drive type by year.
vehicles %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, fill = drive), color = "black") +
  labs(title = "Stacked Bar Chart of Drive Type Composition by Year", x = "Model Year", y = "Number of Cars") +
  coord_flip() +
  theme_minimal() + 
  theme(text = element_text(size = 14))

#----------------------------------------------------------------
#' #4. Clean the Data
#----------------------------------------------------------------
# First, let's recall the summary statistics on citympg, displacement and highwaympg.
vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

# Let's use the median imputation approach to resolve the missing values for citympg and highwaympg.
vehicles <- vehicles %>%
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm = TRUE), citympg)) %>%
  mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg, na.rm = TRUE), highwaympg))

# For displacement, we use the mean imputation approach.
vehicles <- vehicles %>%
  mutate(displacement = ifelse(
    is.na(displacement),
    mean(displacement, na.rm = TRUE),
    displacement
  ))

# Now, let's take a look at our summary statistics.
vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

#----------------------------------------------------------------
#' #5. Normalize the data
#----------------------------------------------------------------
## Decimal Scaling.
# Let's look at the descriptive statistics for the c02emissions feature.
vehicles %>%
  select(co2emissions) %>%
  summary()

# Based on the descriptive statistics, we set j=4 (10^4).
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_d = co2emissions / (10 ^ 4)) %>%
  summary()

## z-score Normalization.
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_z = (co2emissions - mean(co2emissions)) / sd(co2emissions)) %>%
  summary()

# Alternatively, we can also use the scale() function.
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_z = scale(co2emissions)) %>%
  summary()

## Min-max Normalization.
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n =
           ((co2emissions - min(co2emissions))
            / (max(co2emissions) - min(co2emissions))) * (1 - 0) + 0) %>% 
  summary()

## Log Transformation.
vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_b = log10(co2emissions)) %>%
  summary()

#----------------------------------------------------------------
#' #6. Dummy Coding
#----------------------------------------------------------------
# We make use of the drive feature from the vehicles dataset to illustrate dummy coding.
# What are the current values for the drive feature?
vehicles %>%
  select(drive) %>%
  summary()

# Let's create a new version of our vehicles dataset called vehicles2.
# For this new dataset, we also create a new feature called drive2.
# Using the recode() function, we can change values in the selected variable.
vehicles2 <- vehicles %>%
  mutate(drive2 = recode(drive, "2-Wheel Drive" = "Front-Wheel Drive")) %>%
  mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive")) %>%
  select(drive, drive2)

# What do we have?
head(vehicles2)
summary(vehicles2)

# Before we can dummy code, we need to convert vehicles2 to a data frame.
vehicles2 <- data.frame(vehicles2)

# The dummies package provides us with useful functions for dummy coding.
library(dummies)

# Now we can use the dummy.data.frame() function to create dummy variables.
vehicles2 <-
  dummy.data.frame(data = vehicles2, names = "drive2", sep = "_")

# What do we have now?
head(vehicles2)
summary(vehicles2)

#----------------------------------------------------------------
#' #7. Simple Random Sampling
#----------------------------------------------------------------
# Before we sample our data, let's go over some basics.
# There are several ways to do sampling in R.
# For simple random sampling, we make use of the base sample() function.

# Recall that random sampling can be done without replacement.
# Generate a random sampling of 10 unique numbers between 1 and 20.
# The use of set.seed() ensures that every time we run our code, we get the same results.
set.seed(1234)
sample(100, 20, replace = FALSE)

# Random sampling can also be done with replacement.
# This is also known as Bootstrapping.
set.seed(1234)
sample(100, 20, replace = TRUE)

# Using sampling without replacement, we can split our data set into training and test datasets.
# First, we need to create a sample set vector which will serve as a reference.
# Let's generate a random sampling of 27,734 unique numbers between 1 and 36,979.
set.seed(1234)
sample_set <- sample(36979, 27734, replace = FALSE)

# Notice that 750 is 75% of our data. So we're effectively creating a 75:25 split.
# We could also use the following approach to creating our sample set vector.
set.seed(1234)
sample_set <-
  sample(nrow(vehicles), nrow(vehicles) * 0.75, replace = FALSE)

# Our training data will include the rows in our sample set vector.
vehicles_train <- vehicles[sample_set, ]
vehicles_train

# And our test data will include the rest of the rows NOT in our sample set vector.
vehicles_test <- vehicles[-sample_set, ]
vehicles_test

#----------------------------------------------------------------
#' #7. Stratified Random Sampling
#----------------------------------------------------------------
# Let's get the distribution of values for the drive feature.
vehicles %>%
  select(drive) %>%
  table() %>%
  prop.table()

# Let's get a sample of the data using simple random sampling.
set.seed(1234)
sample_set <-
  sample(nrow(vehicles), nrow(vehicles) * 0.01, replace = FALSE)
vehicles_simple <- vehicles[sample_set, ]

# What is the distribution of values for the drive feature?
vehicles_simple %>%
  select(drive) %>%
  table() %>%
  prop.table()

# We can also do stratified random sampling in order to keep the distribution the same.
# For this, we need to install the caTools package and make use of the sample.split() function.
library(caTools)

# To create our sample set vector, we use the sample.split() function and specify our split ratio.
set.seed(1234)
sample_set <- sample.split(vehicles$drive, SplitRatio = 0.01)
vehicles_stratified <- subset(vehicles, sample_set == TRUE)

# What is the distribution of values for the drive feature?
vehicles_stratified %>%
  select(drive) %>%
  table() %>%
  prop.table()
