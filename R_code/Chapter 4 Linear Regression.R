#### Correlation #### 
# cor(): to find correlation: (1) between 2 variables, specify them; (2) among all variables, specify data set 
# corrplot(): make correlation plot in a square, darker bigger the circle, the stronger correlation 
#     type="upper": only show upper triangle
# corrplot.mixed(): make correlation plot in a square with pearson r

# 1. Correlation "humidity" & "rentals" 
library(tidyverse)
library(corrplot)
library(olsrr)
library(car)
library(plyr)
library(dplyr)

setwd("/Users/nicoleyin88/Documents/Other /2. Machine Learning/Data/")
bikes <- read_csv("bikes.csv")

# Method 1
cov(bikes$humidity, bikes$rentals)
sd(bikes$humidity)
sd(bikes$rentals)

pearson <- cov(bikes$humidity, bikes$rentals) / (sd(bikes$humidity) * sd(bikes$rentals))
pearson

# Method 2 
cor(bikes$humidity, bikes$rentals)

# Other Correlations
cor(bikes$windspeed, bikes$rentals)

cor(bikes$temperature, bikes$rentals)

# 2. Visualizing Correlations 
# (1) Remove non-numeric data "data"  
bikenumeric <- bikes %>% 
  select(-date)

# (2) Compute table of correlation coefficients 
bike_correlations <- cor(bikenumeric)

# (3) Correlation plot in a square 
corrplot(bike_correlations)

# (4) Correlation plot in an upper triangle 
corrplot(bike_correlations, type="upper")

# (5) Correlation plot mixed: with circles and numbers 
corrplot.mixed(bike_correlations)
####----------------------------------------------------------------------------------

####  Regression ####
# ols_plot_resid_hist(): plot histogram of residuals to see if normal distribution 
# ols_plot_resid_fit(): plot residual vs fitted values to see if residuals are homoscedastic 
# durbinWatsonTest(): if between (0,2) positive correlation; if 2, no autocorrelation; if between (2,4), negative autocorrelation
# ols_plot_cooksd_chart(): plot all influential points 

# 1. Beta coefficients 
B1 <- cov(bikes$temperature, bikes$rentals) / var(bikes$temperature)
B1

B0 <- mean(bikes$rentals) - B1 * mean(bikes$temperature)
B0

# 2. Linear Regression 
# (1) Simple Regression 
bikes_mod1 <- lm(rentals ~ temperature, data=bikes)

bikes_mod1
summary(bikes_mod1)

# (2) Multiple Regression
bikes_mod2 <- lm(rentals ~ humidity + windspeed + temperature, data=bikes)

summary(bikes_mod2)

# 3. Residual Diagnostics 
# (1) Zero Mean of Residuals 
mean(bikes_mod2$residuals)         # zero mean 

# (2) Normality of Residuals 
ols_plot_resid_hist(bikes_mod2)     # normal distribution 

# (3) Homoscedasticity 
ols_plot_resid_fit(bikes_mod2)      # homoscedastic 

# (4) Residual Autocorrelation 
durbinWatsonTest(bikes_mod2)        # positive autocorrelation 

# 4. Influential Point Analysis 
# (1) Cook's distance test: plot all influential points  
ols_plot_cooksd_chart(bikes_mod2)

# (2) List all outliers 
cooks_outliers <- ols_plot_cooksd_chart(bikes_mod2)$outliers 
arrange(cooks_outliers, desc(cooks_distance))

# (3) List observation (outlier) "69"
bikes[69, c("rentals", "humidity", "windspeed","temperature")]

# (4) Compare summary for observation "69" and all other observations
summary(bikes[69, c("rentals", "humidity", "windspeed","temperature")])

summary(bikes[-69, c("rentals", "humidity", "windspeed","temperature")])

# (5) Compare summary for 25 outliers, data without 25 outliers, and all data 
outlier_index <- as.numeric(unlist(cooks_outliers[,"observation"])) 

summary(bikes[outlier_index,c("rentals", "humidity", "windspeed","temperature")])    # 25 outliers  

summary(bikes[-outlier_index,c("rentals", "humidity", "windspeed","temperature")])   # data without 25 outliers

summary(bikes[,c("rentals", "humidity", "windspeed","temperature")])      # all data 

# 5. Multicollinearity 
ols_vif_tol(bikes_mod2)    # all VIF<5 (all tolerance >0.2), no multicollinearity

####----------------------------------------------------------------------------------

#### Improving the Model 
# 1. Adding Squared terms (Nonlinear Relationship: polynomial regression) 
# (1) Create squared term
bikes <- bikes %>%                # should use bikes2, but it doesn't work
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>% 
  mutate(temperature2 = temperature^2)

# (2) Multiple linear regression 
bikes_mod3 <- lm(rentals ~ humidity + windspeed + temperature + humidity2 + windspeed2 + temperature2,
                 data=bikes)

summary(bikes_mod3)

# (3) Result 
# windspeed2 not significant, remove it

# (4) New Multiple linear regression 
bikes_mod3 <- lm(rentals ~ humidity + windspeed + temperature + humidity2 + temperature2,
                 data=bikes)

summary(bikes_mod3)

# 2. Adding Categorical variables 
# (1) Look at the categorical variables - now numeric
summary(bikes[,c("season","holiday","weekday","weather")])

# (2) Transform: change levels (names) of categorical factors from numbers to text equivalent 
bikes <- bikes %>% 
  mutate(season = revalue(as.factor(season), c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Fall"))) %>%
  mutate(holiday = revalue(as.factor(holiday), c("0" = "No", "1" = "Yes"))) %>% 
  mutate(weekday = revalue(as.factor(weekday), c("0" = "Sunday", "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday"))) %>%
  mutate(weather = revalue(as.factor(weather), c("1" = "Clear", "2" = "Light precipitation", "3" = "Heavy precipidation")))

# (3) Multiple regression with categorical terms 
bikes_mod4 <- lm(rentals ~ humidity + windspeed + temperature + humidity2 + temperature2 + season,
                 data = bikes)

summary(bikes_mod4)

# 3. Adding interaction terms 
bikes_mod5 <- lm(rentals ~ humidity + temperature + humidity2 + temperature2 + season + windspeed*weather,
                 data = bikes)

summary(bikes_mod5)

# 4. Variable Selection
model <- lm(rentals ~ humidity + weekday + holiday + temperature + humidity2 + temperature2 + season 
           + windspeed*weather + realfeel + date, data = bikes)
ols_step_both_p(model, pent=0.2, prem = 0.01, details =  FALSE) 
####----------------------------------------------------------------------------------

#### Case Study: Predicting Blood Pressure ####
# 1. Import data 
health <- read_csv("health.csv")

glimpse(health)    # notice "diabetes" & "smoker" should be factors 

# 2. Transform "numeric" to factor 
health %>% 
  mutate(diabetes = as.factor(diabetes)) %>% 
  mutate(smoker = as.factor(smoker))

# 3. Explore distribution of each variable 
# (1) Histogram for each variable  
summary(health) 

health %>% 
  keep(is.numeric) %>%
  gather() %>% 
  ggplot() + geom_histogram(mapping = aes(x=value, fll=key), fill="thistle2", color="black") + facet_wrap(~ key, scales="free")  + theme_minimal()

# (2) Result 
# i) Dependent variable "systolic" about normally distributed 
# ii) "age" uniformly distributed 
# iii) "bmi, height, waist, weight" are about normally distributed 
# iv) "fastfood" is right-skewed. 

# 4. Explore Correlation 
# (1) Correlation 
cor(health[,c("systolic", "weight","height","bmi","waist","age","fastfood")])

# (2) Result 
# Correlation with "systolic" from strong to week: "age, waist, weight."
# "fastfood" is negatively correlated with "systolic," since it's small, we can ignore it.

# 5. Simple Linear Regression 
# (1) Regression 
health_mod1 <- lm(systolic ~ age, data = health)

summary(health_mod1)

#  (2) Result 
# Coefficients are significant 
# Residual SE is low
# F-stat is significant
# Adjusted R-squared is only 16%. 

# 6. Multiple Linear Regression 
# (1) Regression 
health_mod2 <- lm(systolic ~., data = health)    # use . to represent all variables 

summary(health_mod2)

# (2) Result 
# Coefficients for "weight, height, bmi,age, diabetes" are significant
# a slight reduction in residual SE
# a slight increase in adjusted R-squared
# significant F-stat

# 7. Residual Diagnostics 
# (1) Zero mean of residuals 
mean(health_mod2$residuals)    # close to zero 

# (2) Residual normal distribution 
ols_plot_resid_hist(health_mod2)  # normally distributed with a slight right skew 

# (3) Homoscedasticity 
ols_plot_resid_fit(health_mod2)   # homoscedastic

# (4) No autocorrelation 
durbinWatsonTest(health_mod2)  # Durbin-Watson stat=2.04, p-value>0.05, can't reject "no first order autocorrelation exists." So no autocorrelation

# 8. Check influential points 
# (1) Cook's distance chart
ols_plot_cooksd_chart(health_mod2)

# (2) Compare biggest outlier 1358 with all data 
health[1358,]      # larger than median/mean for all data 

summary(health)

# (3) Extract all outliers 
outlier_index <- as.numeric(unlist(ols_plot_cooksd_chart(health_mod2)$outliers[,"observation"]))
outlier_index

summary(health[outlier_index])    # summary for outliers

summary(health[-outlier_index])   # summary for non-outliers 

# (4) Create New data without outliers 
health2 <- health[-outlier_index]
health2

# 9. Check multicollinearity 
# (1) VIF values 
ols_vif_tol(health_mod2)

# (2) Result 
# "weight, height, bmi, waist" have VIF >5, there's multicollinearity
# can combine/drop impacted variables.  
# Since "weight" has lowest tolerance(largest VIF), we can drop that and keep the others. 

# 10. Multiple regression with new data 
# (1) Multiple Regression 
health_mod3 <- lm(systolic ~ weight + age + diabetes, data = health)   # data health2 isn't generated, I use health for now

summary(health_mod3)

# (2) Result (if with dath health2)
# all predictors significant 
# now the model explains 21% 

# 11. Multiple regression with interaction terms 
# (1) Create age2, log(age) variable 
health <- health %>%
  mutate(age2=age^2,
         lage=log(age))

# (2) Perform variable selection 
model <- lm(systolic ~ weight*diabetes + age*diabetes + age2*diabetes + lage*diabetes, data = health) 

ols_step_both_p(model, pent=0.2, prem=0.001, details = FALSE)   # This doesn't match book 

# (3) Result 
# slight improvement over previous model 
# to get model that better explains variability in our response, need more predictors that correlate with response 
# could include info about gender, family medical history, exercise habits 
####----------------------------------------------------------------------------------