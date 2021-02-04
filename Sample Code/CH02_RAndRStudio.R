#' ---
#' title: "CHAPTER 2 - R And R Studio"
#' author: "Fred Nwanganga and Mike Chapple"
#' date: "March 10th, 2020"
#' ---

#----------------------------------------------------------------
#' 1. R Packages
#----------------------------------------------------------------
# Install a package.
install.packages("RWeka")

# Load a package.
library(tidyverse)

# Check the vignette of the package.
vignette(package = 'dplyr')

# Check the programming topic in the vignette of the package.
vignette(package = 'dplyr', topic = 'programming')

#----------------------------------------------------------------
#' 2. Data Types in R
#----------------------------------------------------------------
# Create vectors.
names <- c('Mike', 'Renee', 'Richard', 'Matthew', 'Christopher')
points <- c(85, 92, 95, 97, 96)

# Check individual component in the vectors.
names[1]
names[2]
scores[3]

# Perform calculations on the entire numeric vector.
mean(scores)
median(scores)
min(scores)
max(scores)
sum(scores)

# If we attempt to create a vector containing both strings 
# and numbers, R would convert all the elements to strings.
mixed <- c('Mike', 85, 'Renee', 92, 'Richard', 95, 'Matthew', 97, 'Christopher', 96)

mixed

# Combine different types of vectors in a data frame.
testResults <- data.frame(names, scores)

testResults

# Use `$` operator to access a specific column in the data frame.
mean(testResults$scores)

#----------------------------------------------------------------
#' 3. Testing Data Types
#----------------------------------------------------------------
x <- TRUE
y <- 1
z <- 'Mike Chapple'

# Check the data type of the object.
class(x)
class(y)
class(z)

# Create a character vector named 'productCategories' and check its data type.
productCategories <- c('fruit', 'vegetable', 'fruit', 'fruit', 'dry goods', 'dry goods', 'vegetable')
class(productCategories)
# Convert the vector from character to factor.
productCategories <- factor(productCategories)
# Check the data type again.
class(productCategories)

# Check the length of the objet.
length(x)
length(y)
length(z)
length(productCategories)

# Check whether the object is of a specific data type.
is.numeric(x)
is.character(x)
is.integer(x)
is.logical(x)
is.numeric(y)
is.integer(y)
is.character(z)

# Let's see the difference between numeric variable and integer variable.
y <- 1
yint <- 1L

is.integer(yint)
is.numeric(yint)

class(y)
class(yint)

# The `is` function also works for vectors.
is.character(names)
is.numeric(names)
is.character(scores)
is.numeric(scores)
is.integer(scores)

#----------------------------------------------------------------
#' 4. Converting Data Types
#----------------------------------------------------------------
# Use `as` function to convert data from one type to another.
# Note: There is no reasonable way to convert a word into an integer.
as.numeric("1.5")
as.integer("1.5")
as.character(3.14159)
as.integer("apple")
as.logical(1)
as.logical(0)
as.logical("true")
as.logical("apple")
