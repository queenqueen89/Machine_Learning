#### Summary #### 
# 1. Analyze Data 
# (1) Glimpse Data ( glimpse() )
# (2) Look at all categorical variables ( keep(is.factor) )
# (3) See proportion of NA's (exclude=NULL)

# 2. Deal with Missing Data 
# (1) Replace NA's with UNK for categorical variables ( as.factor(ifelse(is.na(), 'UNK', ) )
# (2) Replace NA's for continuous variables ( (ifelse(is.na(), mean(, na.rm=TRUE), ) ) 

# 3. Deal with outliers 
# max1 = quantile(, 0.75) + (1.5 * IQR())
# filter( <= max1)
# select(-max1)

# 4. Split data 
# (1) Create training & testing set 
# (2) Check class distribution & imbalance problem

# 5. Deal with class imbalance problem 
# (1) Fix Class Imbalance Problem ( SMOTE(dep var ~ ., data.frame(), perc.over = 100, perc.under = 200) )
# (2) Transform  class from T/F to 1/0

# 6. Train & Evaluate a Model 
# (1) Model 1 (with all regressors)
# (2) Interprete Coefficeints ( exp(coef())[" "] )
# (3) predict with testing set 
# (4) convert results to 1/0 or T/F
# (5) confusion matrix
# (6) prediction accuracy 

# 7. Deal with multicollinearity 
# (1) correlation plot ( corrplot() )
# (2) VIF for Model 1 ( vif() )

# 8. Train & evaluate new model 
# (1) Model 2 (after removing collinear regressors)
# (2) VIF for Model 2
# (3) Prediction with testing set 

# 9. Choose an Ideal cutoff value 
# (1) Find ideal cutoff 
# (2) Predict with ideal cutoff 
# (3) Confusion Matrix
# (4) Prediction Accuracy
####----------------------------------------------------------------------------------

#### Binomial Logistic Regression  #### 
# 1. Analyze Data 
library(tidyverse)
library(DMwR)
library(corrplot)
library(car)
library(InformationValue)

setwd("/Users/nicoleyin88/Documents/Other /2. Machine Learning/Data/")
donors <- read_csv("donors.csv", col_types = "nnffnnnnnnnnffffffffff")

# (1) Glimpse data 
glimpse(donors)

# (2) Look at only categorical variables (use keep())
donors %>%
  keep(is.factor) %>%
  summary()

# (3) See proportion of NA's 
donors %>% 
  select(incomeRating) %>% 
  table(exclude=NULL) %>%        # to include NA values 
  prop.table()                   # 22.31% are NA's

# 2. Deal with Missing Data 
# (1) Replace NA's with UNK for "incomeRating" 
# replace 
donors <- donors %>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), 'UNK', incomeRating)))

# take a look
donors %>% 
  select(incomeRating) %>%
  table() %>% 
  prop.table()

# (2) Replace NA's with UNK for other variables 
# Replace 
donors <- donors %>%
  mutate(wealthRating = as.character(wealthRating)) %>%
  mutate(wealthRating = as.factor(ifelse(is.na(wealthRating), 'UNK', wealthRating))) %>%
  mutate(urbanicity = as.character(urbanicity)) %>%
  mutate(urbanicity = as.factor(ifelse(is.na(urbanicity), 'UNK', urbanicity))) %>%
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>%
  mutate(socioEconomicStatus = as.factor(ifelse(is.na(socioEconomicStatus), 'UNK', socioEconomicStatus))) %>%
  mutate(isHomeowner = as.character(isHomeowner)) %>%
  mutate(isHomeowner = as.factor(ifelse(is.na(isHomeowner), 'UNK', isHomeowner))) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = as.factor(ifelse(is.na(gender), 'UNK', gender)))

# Take a look 
donors %>% 
  keep(is.factor) %>%
  summary()

# (3) Look at only continuous variables (use keep())
donors %>% 
  keep(is.numeric) %>%
  summary()

# (4) Replace NA's for "age" 
# replace 
donors <- donors %>%
  group_by(gender) %>%      # use mean of age grouped by gender 
  mutate(age = ifelse(is.na(age), mean(age, na.rm=TRUE), age)) %>%
  ungroup()

# take a look 
donors %>% 
  select(age) %>% 
  summary() 
  
# (5) Replace NA's for "gender" 
# replace 
donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), 
                                 median(numberChildren, na.rm=TRUE), 
                                 numberChildren)) 
# take a look 
donors %>% 
  select(numberChildren) %>% 
  summary() 

# 3. Deal with Outliers 
# rule of thumb: outlier = value that is larger than 1.5 times the interquantile range Q3-Q1
donors <- donors %>% 
  mutate(max1 = quantile(mailOrderPurchases, 0.75) + (1.5 * IQR(mailOrderPurchases))) %>%      # get outliers
  mutate(max2 = quantile(totalGivingAmount, 0.75) + (1.5 * IQR(totalGivingAmount))) %>%
  mutate(max3 = quantile(numberGifts, 0.75) + (1.5 * IQR(numberGifts))) %>%
  mutate(max4 = quantile(smallestGiftAmount, 0.75) + (1.5 * IQR(smallestGiftAmount))) %>%
  mutate(max5 = quantile(largestGiftAmount, 0.75) + (1.5 * IQR(largestGiftAmount))) %>%
  mutate(max6 = quantile(averageGiftAmount, 0.75) + (1.5 * IQR(averageGiftAmount))) %>%
  filter(mailOrderPurchases <= max1) %>%            # remove outliers
  filter(totalGivingAmount <= max2) %>%
  filter(numberGifts <= max3) %>%
  filter(smallestGiftAmount <= max4) %>%
  filter(largestGiftAmount <= max5) %>%
  filter(averageGiftAmount <= max6) %>%
  select(-max1, -max2, -max3, -max4, -max5, -max6)     # select without outliers 

# Take a look 
donors %>% 
  keep(is.numeric) %>%
  summary()

# 4. Split the Data 
# (1) Create training & testing sets 
set.seed(1234)  
sample_set <- sample(nrow(donors), round(nrow(donors)*0.75), replace=FALSE)
donors_train <- donors[sample_set,]   
donors_test <- donors[-sample_set,]

# (2) Check class distribution & imbalance 
round(prop.table(table(select(donors,respondedMailing), exclude=NULL)),4) *100

round(prop.table(table(select(donors_train,respondedMailing), exclude=NULL)),4) *100

round(prop.table(table(select(donors_test,respondedMailing), exclude=NULL)),4) *100     # have similar class distributions, but have imbalance problem

# 5. Deal with Class Imbalance
# (1) Fix Class Imbalance Problem
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ .,
                      data.frame(donors_train), 
                      perc.over = 100,         
                      perc.under = 200)

# (2) Table a look 
round(prop.table(table(select(donors, respondedMailing), exclude=NULL)), 4) * 100

round(prop.table(table(select(donors_train, respondedMailing), exclude=NULL)), 4) * 100     # training set is 50-50 ratio

round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

# (3) Transform class from T/F to 1/0
donors <- donors %>% 
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))

donors_train <- donors_train %>% 
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))

donors_test <- donors_test %>% 
  mutate(respondedMailing = as.factor(ifelse(respondedMailing==TRUE, 1, 0)))
  
# 6. Train & Evaluate a Model 
# (1) Model 1 (with all regressors) 
donors_mod1 <- glm(formula = respondedMailing ~ ., family = binomial, data = donors_train)

summary(donors_mod1)

# (2) Interpret Coefficients
exp(coef(donors_mod1))["averageGiftAmount"]      # less than 1

exp(coef(donors_mod1))["monthsSinceLastDonation"]

exp(coef(donors_mod1))["incomeRating2"]      # greater than 1: with 1 unit increase in incomeRating2, odds of donor respond increase by 1.15

# (3) Predict with Testing set 
donors_pred1 <- predict(donors_mod1, donors_test, type = "response")   # testing set contains states "RI" and "NH" that training set doesn't have

# (4) Filter "RI" and "NH" states from Testing set 
filter(donors_test, state=="RI" | state=="NH")  # only 3 observations 

# (5) Remove "RI" and "NH" states from Testing set 
donors_test <- donors_test %>% 
  filter(state!="RI" & state!="NH")

# (6) Predict with new Testing set 
donors_pred1 <- predict(donors_mod1, donors_test, type = "response")   # testing set contains states "RI" and "NH" that training set doesn't have
head(donors_pred1)

# (7) Convert results to 1/0 or T/F
# Method 1: Convert to 1/0
donors_pred1 <- ifelse(donors_pred1 >= 0.5, 1, 0)     # set cutoff = 0.5

head((donors_pred1))

# Method 2: Convert to T/F
donors_pred1 <- ifelse(donors_pred1 >= 0.5, TRUE, FALSE)

head((donors_pred1))

# (8) Confusion Matrix 
donors_pred1_table <- table(donors_test$respondedMailing, donors_pred1)
donors_pred1_table      # 12481 predicted correct for 0, 296 predicted correct for 1 (diagonal terms)

# (9) See prediction accuracy 
sum(diag(donors_pred1_table)) / nrow(donors_test)   # 73% prediction accuracy 

# 7. Deal with Multicollinearity
# (1) Correlation Plot 
donors %>% 
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

# (2) VIF factors for Model 1
vif(donors_mod1)   # remove "totalGivingAmount, smallestGiftAmount, LargestGiftAmount"

# 8. Train & Evaluate New Model 
# (1) Model 2 (after removing collinear regressors) 
donors_mod2 <- glm(respondedMailing ~ incomeRating + wealthRating + 
        mailOrderPurchases + numberGifts + yearsSinceFirstDonation + 
        monthsSinceLastDonation + sweepstakesDonor + state + 
        urbanicity + socioEconomicStatus + isHomeowner + gender, 
        data = donors_train,
        family = binomial)

summary(donors_mod2)

# (2) VIF factors for Model 2
vif(donors_mod2)    # no VIF is greater than 5 

# (3) Prediction with testing set 
donors_pred2 <- predict(donors_mod2, donors_test, type = "response")

head(donors_pred2)

# 9. Choose an Ideal cutoff value 
# (1) Find ideal cutoff 
ideal_cutoff <- optimalCutoff(actuals = donors_test$respondedMailing,
                              predictedScores = donors_pred2,
                              optimiseFor = "Both") 
ideal_cutoff      # ideal cutoff is 0.4722993 not 0.5 

# (2) Predict with ideal cutoff 
donors_pred2 <- ifelse(donors_pred2 >= ideal_cutoff, 1, 0)

head(donors_pred2) 

# (3) Confusion Matrix
donors_pred2_table <- table(donors_test$respondedMailing, donors_pred2)
donors_pred2_table

# (4) Prediction Accuracy 
sum(diag(donors_pred2_table)) / nrow(donors_test)    # 59% accurate 
####----------------------------------------------------------------------------------

####  Case Study: Income Prediction #### 

# 1. Analyze Data 
income <- read_csv("income.csv", col_types = "nffnfffffnff")

# (1) Glimpse data 
glimpse(income)

# (2) Look at Categorical variables 
# Method 1: summary()
income %>% 
  keep(is.factor) %>%
  summary()      # could only see top six values if results show "(other)"

# Method 2: table()
table(select(income, workClassification))
table(select(income, educationLevel))
table(select(income, occupation))
table(select(income, nativeCountry))

# 2. Deal with Missing Data 
# (1) Replace "?" with UNK for categorical variables 
income <- income %>%
  mutate(workClassification = dplyr::recode(workClassification, "?"="UNK")) %>%     # recode() for factors
  mutate(nativeCountry = dplyr::recode(nativeCountry, "?"="UNK")) %>%
  mutate(occupation = dplyr::recode(occupation, "?"="UNK")) 

summary(income[,c("workClassification", "nativeCountry","occupation")])

# (2) Replace <=50k or >50k with values 0 or 1
income <- income %>%
  mutate(income = dplyr::recode(income, "<=50K" = "0")) %>%
  mutate(income = dplyr::recode(income, ">50K" = "1"))

summary(income[,"income"])

# 3. Split data 
# (1) Create training & testing set 
set.seed(1234)
sample_set <- sample(nrow(income), round(nrow(income)*0.75), replace=FALSE)
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

# (2) Check class distribution & imbalance problem
round(prop.table(table(select(income,income),exclude=NULL)), 4) * 100

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

round(prop.table(table(select(income_test, income), exclude = NULL)), 4) * 100

# 4. Deal with class imbalance problem 
set.seed(1234) 
income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200) 

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

# 5. Train & Evaluate a Model 
# (1) Model 1 (with all regressors)
income_mod1 <- income_train %>% 
  keep(is.factor) %>%         # only use categorical variables 
  glm(formula = income ~ ., family = binomial)

summary(income_mod1)

# (2) predict with testing set 
income_pred1 <- predict(income_mod1, data=income_test, type="response")

head(income_pred1)

# 6. Choose an Ideal cutoff value 
# (1) Find ideal cutoff 
ideal_cutoff <- optimalCutoff(actuals = income_test$income, 
                              predictedScores = income_pred1,
                              optimiseFor = "Both")
ideal_cutoff

# (2) Predict with ideal cutoff 
income_pred1 <-- ifelse(income_pred1 >= ideal_cutoff, 1, 0)

head(income_pred1)

# (3) Confusion Matrix
income_pred1_table <- table(income_test$income, income_pred1)

# (4) Prediction Accuracy
sum(diag(income_pred1_table)) / nrow(income_test)
####----------------------------------------------------------------------------------