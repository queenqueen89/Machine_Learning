#### Describing the Data #### 
# read_csv()
#     col_types: l=logical, n=numeric, i=integer, c=characters, f=factors, D=datas, T=datetimes
#     skip = 10: means not read the first 10 rows.
# glimpse(): shows total observations(rows) & total variables (columns)
# summary() shows min-max for numeric, frequency for category (only top 6 feature values)
# select(): choose variables(columns)
# table(): shows all feature values for category
# prop.table(): show table of proportion for variables 
# filter(): choose observations(rows)
# %>%: pipe 
# summarize(): aggregate rows 

# 1. Load the data
library(tidyverse)
library(dummies)
library(caTools)

setwd("/Users/nicoleyin88/Documents/Other /2. Machine Learning/Data/")

vehicles <- read_csv(file = "vehicles.csv", 
                     col_names = TRUE,
                     col_types="nnnfnfffffnn",
                     skip = 0)  

# 2. Describe the Data
# (1) Show glimpse of data 
glimpse(vehicles)    

# (2) Show summary of data 
summary(vehicles)   

# (3) Select variables to display 
# display variable "class" only 
select(vehicles, class)  

# display variable "class" & "cylinders" 
select(vehicles, class, cylinders)    

# (4) Show summary of selected variables 
# nesting: function inside function (not easy to keep track)
summary(select(vehicles, class, cylinders))     

# (5) Show table of selected variables 
table(select(vehicles, class, cylinders))      

# (6) Show table of proportion of selected variables 
prop.table(table(select(vehicles, class)))   

# (7) Use pipe %>%
# pipe: use %>%, easy to control logical flow of code
vehicles %>%
  select(class) %>%
  table() %>%
  prop.table()

# (8) Filter observations to display 
vehicles %>%
  filter(drive == "2-Wheel Drive") %>%
  select(co2emissions) %>%
  summary()
####----------------------------------------------------------------------------------

#### Visualizing the Data #### 
# ggplot(): visualize data 
# geom_boxplot(): compare distribution (1 category + 1 numeric variable)
# geom_point(): show correlation (2 numeric variables)
# geom_histogram(): show spread & skewness
#   bins = : how many bins for histogram
# geom_bar(): show how a total value can be divided into parts/highlight the significance of each part relative to the total value
# labs(): title = "", x = "", y = ""
#   mapping=aes(): decide what is on x-axis & y-axis

# 1. Boxplot (Comparison Visualization)
# (1) Compare distribution of CO2 emissions across different vehicle classes 
vehicles %>%
  ggplot() + geom_boxplot(mapping = aes(x=class, y=co2emissions), fill="pink") + labs(title="Boxplot of CO2 Emissions by Vehicle Class", x="Class", y="CO2 Emissions")

# (2) Result
# On average, subcompact cars, compact cars, and midsize cars have lowest CO2 emissions; 
# vans, pickups, and special-purpose cars have highest.

# 2. Scatterplot (Relationship Visualization)
# (1) Show correlation between "citympg" and "co2emissions" 
vehicles %>% 
  ggplot() + geom_point(mapping = aes(x=citympg, y=co2emissions), color="blue", size=2) + labs(title="Scatterplot of CO2 Emissions vs. City Miles per Gallon", x="City MPG", y="CO2 Emissions")

# (2) Result 
# As city gas mileage increases, CO2 emissions decrease. 
# Vehicles with better fuel efficiency ratings emit less carbon dioxide. 

# 3. Histogram (Distribution Visualization)
# (1) Show the spread and skewness of data for CO2 Emissions
vehicles %>% 
  ggplot() + geom_histogram(mapping=aes(x=co2emissions), bins=30, fill="thistle2", color="black") + labs(title="Histogram of CO2 Emissions", x="CO2 Emissions", y="Frequency")

# (2) Result 
# Most CO2 emission values are clustered between 250 and 750 grams per mile. 
# We have some outliers at both low and high end (very small bars)

# 4. Stacked bar chart (Composition Visualization)
# (1)  show how a total value can be divided into parts/highlight the significance of each part relative to the total value
# fill=drive is to fill the bars with types of drive
# coord_flip() is to flip x and y axis

vehicles %>% 
  ggplot() + geom_bar(mapping=aes(x=year, fill=drive), color="black") + labs(title="Stacked Bar Chart of Drive Type Composition by Year", x="Model Year", y="Number of Cars") + coord_flip()

# (2) Result 
# i) Other than in 1997, it appears that no 4-wheel drive vehicles were tested before 2010. 
# ii) 2-wheel drive vehicles were tested only in 1984 and 1999 -- possible variance 
#     in the way vehicle drive types were classified in the impacted years. For example: 
#     It's conceivable that all 4-wheel drive vehicles were classified as all-wheel drive 
#     vehicles every year except for 1997 and from 2010-2018. 
# iii) The same logic applies to classification of 2-wheel drive vehicles as either 
#      rear-wheel drive or front-wheel drive. 
####----------------------------------------------------------------------------------

#### Cleaning the Data #### 
# mutate(): modify variable values of data 
#     ifelse(test, yes, no): ifelse(is.na(var), median(var, na.rm=TRUE), var)
#         is.na(): test is if the variable is N/A
#     median(): if yes it's N/A, replace with median of var
#         na.rm=TRUE: calculate median without N/A values 
#         var: if no it's not N/A, keep original value 

# 1. Mean/Median Imputation (deal with missing data)
# (1) Observe mean/median values 
vehicles %>% 
  select(citympg, displacement, highwaympg) %>%
  summary()

# (2) Result
# Mean/median are close for all three variables;
# We can use mean/median to replace missing values in the data 

# (3) Replace missing in "citympg" & "highwaympg" with Median
vehicles <- vehicles %>%
  mutate(citympg = ifelse(is.na(citympg), median(citympg,na.rm = TRUE),citympg)) %>%
  mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg,na.rm = TRUE),highwaympg))
  
# (4) Replace missing in "displacement" with Mean 
vehicles <- vehicles %>%
  mutate(displacement = ifelse(is.na(displacement), mean(displacement, na.rm = TRUE), displacement))

# (5) Show summary of modified data 
vehicles %>% 
  select(citympg, highwaympg, displacement) %>% 
  summary()

# (6) Result 
# i) Descriptive stat all remained unchanged -- our imputation didn't have 
#    have an appreciable impact on properties of dataset 
# ii) It's not always the outcome of imputation. It depends on number of N/As 
#     and imputation approach chosen, the descriptive stat will vary slightly 
#     after imputing missing values. The goal is to keep the changes as small 
#     as possible. 
####----------------------------------------------------------------------------------

#### Transforming the Data #### 
# scale(): perform z-score normalization 
# mutate()
#     recode(): change variable names to others 
# dummy.data.frame(): dummy coding for variables 

# 1. Decimal Scaling (rescale value between 0 and 1)
# (1) Check min-max for "co2emissions" 
vehicles %>%
  select(co2emissions) %>%
  summary()

# (2) rescale value between 0 and 1
vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_d = co2emissions / 10^4) %>% 
  summary()
  
# 2. Z-score normalization 
# Method 1
vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_z = (co2emissions - mean(co2emissions))/sd(co2emissions)) %>% 
  summary()

# Method 2
vehicles %>% 
  select(co2emissions) %>% 
  scale() %>% 
  summary()

# 3. Min-Max Normalization 
vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_n = (co2emissions - min(co2emissions))*(1-0)/(max(co2emissions)-min(co2emissions)) + 0) %>%
  summary()

# 4. Log Transformation 
vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_log = log10(co2emissions)) %>% 
  summary()

# 5. Dummy Coding
# (1) Show summary for "drive" 
vehicles %>% 
  select(drive) %>% 
  summary()

# (2) Recode 2-wheel drive to front-wheel drive, recode 4-wheel drive to all-wheel drive
vehicles2 <- vehicles %>% 
  select(drive) %>% 
  mutate(drive2 = recode(drive, "2-Wheel Drive" = "Front-Wheel Drive")) %>% 
  mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive")) %>% 
  select(drive, drive2)

head(vehicles2)

summary(vehicles2)

# (3) Dummy coding 
vehicles2 <- data.frame(vehicles2)

vehicles2 <- dummy.data.frame(data=vehicles2, names="drive2", sep = "_")

head(vehicles2)
####----------------------------------------------------------------------------------

#### Reducing the Data 
# sample(N=, n=, replace= ): N=pop. size, n=sample size, replace=T or F

# 1. Random Sampling with Replacement
set.seed(1234)
sample(100,20,replace=TRUE)        # Pop. size N=100, sample size n=20

# 2. Random Sampling without Replacement
# (1) Example 1: sample 1/5 of pop. 
set.seed(1234)
sample(100,20,replace=FALSE)

# (2) Example 2: sample 75% of pop.
set.seed(1234)
sample(36979, 27734, replace=FALSE)

# 3. Sample 75% for training set, 25% for test set
# (1) Method 1: 
set.seed(1234)
sample_set <- sample(nrow(vehicles), nrow(vehicles)*0.75, replace = FALSE)

vehicles_train <- vehicles[sample_set, ]      # 75% for training set
vehicles_train

vehicles_test <- vehicles[-sample_set, ]      # 25% for test set
vehicles_test

# 4. Stratified Random Sampling
# (1) Show table of proportions 
vehicles %>% 
  select(drive) %>% 
  table() %>% 
  prop.table()

# (2) Select 1% of data for our sample 
# Method 1: # sample prop. is not the same as population prop.
set.seed(1234)
sample_set <- sample(nrow(vehicles), nrow(vehicles)*0.01, replace=FALSE)

vehicles_sample <- vehicles[sample_set, ]

vehicles_sample %>% 
  select(drive) %>% 
  table() %>%
  prop.table()          

# Method 2
set.seed(1234)
sample_set <- sample.split(vehicles$drive, SplitRatio = 0.01)

vehicles_stratified <- subset(vehicles, sample_set == TRUE)

vehicles_stratified %>% 
  select(drive) %>% 
  table() %>% 
  prop.table()
####----------------------------------------------------------------------------------