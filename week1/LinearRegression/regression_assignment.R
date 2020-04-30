# Multi Linear Regression Assumptions
# 1. Linearity : The relationship between the independent and dependent variables should be linear.
# 2. Independence : Independence among independent variables. No multicollinearity in the data. Multicollinearity occurs when the independent variables are too highly correlated with each other.
# 3. Normality : The errors between observed and predicted values should be normally distributed.(Multivariate Normality)
# 4. Homoscedasticity(Equality of variance of residual) : Even if the i-th of any independent variable is selected, its error-term has the same distributionA situation in which the error term is the same across all values of the independent variables. A scatterplot of residuals versus predicted values is good way to check for it. If the data are heteroscedastic, a non-linear data transformation or addition of a quadratic fterm might fix the problem


# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

# Data load
# install.packages("car")
library(car)
library(caret)
library(MASS)
library(ggplot2)
data(Boston)

##### Linear Regression - Predict medv(median value of owner-occupied homes in \$1000s) #####

# Data Preprocessing
## Check NaN
sum(is.na(Boston)) # 0, if NA, remove NA by row by using data <- na.omit(data)

## EDA
### Basic EDA
str(Boston) # 506 obs. of  14 variables
summary(Boston) # Min, 1st Qu, Median, Mean, 3rd Qu, Max

### Scatter Plot for Indepedent EDA
for(i in 1:(length(Boston)-1)){
  if(i%%8==0|i==1){ # 2 windows(7, 6) for 13 scatter plots
    x11() # new window
    par(mfrow=c(3,3)) # divide palet into 3x3
  }
  plot(Boston[,i], Boston[,14], main=colnames(Boston)[i])
}

### Histogram for Independent Variable EDA
for(i in 1:(length(Boston)-1)){
  if(i%%8==0|i==1){ # 2 windows(7, 6) for 13 histograms
    x11()
    par(mfrow=c(3,3))
  }
  hist(Boston[,i], main=colnames(Boston)[i])
}

### Target Variable EDA
hist(Boston$medv);summary(Boston$medv)

##### Variable Description ########################################
# crim(num) : per capita crime rate by town. -> Negative Correlation
# zn(num) :proportion of residential land zoned for lots over 25,000 sq.ft. -> Positive Correlation
# indus(num) :proportion of non-retail business acres per town. -> Negative Correlation
# chas(cat) : Charles River dummy variable (= 1 if tract bounds river; 0 otherwise). -> ?
# nox(num) : nitrogen oxides concentration (parts per 10 million). -> Negative Correlation
# rm(num) : average number of rooms per dwelling. -> Positive Correlation
# age(num) : proportion of owner-occupied units built prior to 1940. -> ?
# dis(num) : weighted mean of distances to five Boston employment centres. -> Positive Correlation
# rad(cat) : index of accessibility to radial highways. -> Negative Correlation
# tax(num) : full-value property-tax rate per \$10,000. -> Negative Correlation
# ptratio(num) : pupil-teacher ratio by town. -> Negative Correlation
# black(num) : 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town. -> ?
# lstat(num) : lower status of the population (percent). -> Negative Correlation
# medv(num) : median value of owner-occupied homes in \$1000s(Target Variable)
####################################################################

## Fit Linear Regression Model
fit.full<-lm(medv~.,Boston) # fit Y with all possible variables
summary(fit.full)
#### R-squared: 0.7406,	Adjusted R-squared :  0.7338
#### significant variable : crim, zn, chas, nox, rm, dis, rad, tax, ptratio, black, lastat

## Variable selection based on AIC(The lower AIC, the better variable)
### A model without Explanatory variable(Independent variable)
fit.con <- lm(medv ~ 1, Boston)

#### Forward selection
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")

#### Backward selection
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")

#### Stepwise selection
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")

summary(fit.forward)
summary(fit.backward)
summary(fit.both)
#### R-squared : 0.7406, Adjusted R-squared :  0.7348
#### Drop indus, age

## Model Diagnostics
### Plot
par(mfrow=c(2,2))
plot(fit.both)
#### Residuals vs Fitted - Determine Homogeneity of Variance, Linearity and Independence -> Ideally, Straight line with zero slope
#### normal Q-Q - Determine Normality of Residuals -> Ideally, straight line
#### Scale-Location - Show Standardized Residual -> Ideally, straight line with zero slope
#### Residuals vs Leverage - Show Residuals vs Leverage based on Cook's distance. Leverage is how extreme the explanatory variable is. The more the dots are concentrated to one side, the better the model fits.
#### 365, 369, 373

### Normality Test - shapiro.test
e <-resid(fit.both) # residual
shapiro.test(e) # p-value < 0.05 -> Normality not satisfied, but keep going

### Independence
plot(predict(fit.both), e) # no exact patterns
plot(predict(fit.both), e, type = 'o')

### Multicollinearity
vif(fit.both)
#### Variance Inflation Factor : VIF(i) = 1/(1-R(i)^2)
#### If VIF(i) > 10 -> multicollinearity(up to significance level)

## Data Diagnostics
#### Data points with large residuals (outliers) and/or high leverage may distort the outcome and accuracy of a regression.
#### If leverage low, rstudent high -> outlier
#### If leverage high, rstudent high -> influential observation
#### The higher the leverage and residuals, the higher the cook's distance.

### Outlier
outlierTest(fit.both) # 365, 369, 373th values -> outliers
#### Bonferonni p < 0.05 -> determine that it is an outlier

### Influential Observation
#### leverage : Leverage is a measure of how far away the independent variable values of an observation are from those of the other observations.
#### diffit : Difference in Fits Cook's Distances. A scaled measure of the change in the predicted value for the ith observation and is calculated by deleting the ith observation
#### cov.r : The change in the determinant of the covariance matrix of the estimates by deleting the ith observation
#### cook.d : The sum of all the changes in the regression model when observation (i) is removed from it. Cook's distance measures the effect of deleting a given observation. Technically, Cook¡¯s D is calculated by removing the ith data point from the model and recalculating the regression. It summarizes how much all the values in the regression model change when the ith observation is removed.
#### hat matrix : Influence matrix. y^ = H*y. H(ii) -> leverage
#### inf : * or not
influencePlot(fit.both, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook¡¯s distance") # 365, 369, 373th values -> potentially both outliers & influential observations
influenceIndexPlot(fit.both, id.n=3) # Cook's distance measures how much an observation influences the overall model or predicted values
influencePlot(fit.both, id.n=6) # Creates a bubble-plot combining the display of Studentizedresiduals, hat-values, and Cook's distance (represented in the circles)

### Elimination of Outliers & Influential Observations
inf_res<-influence.measures(fit.both) # * marked : suspected influential observation
table(inf_res$is.inf) # 75 potentially influential observations
inf_res_summary<-summary(inf_res)
inf_idx<-rownames(inf_res_summary)
Boston2<-Boston[-as.numeric(inf_idx)]

# Rebuild the Model
fit2.full<-lm(medv~.,Boston2)
fit2.con <- lm(medv ~ 1, Boston2)
fit2.both <- step(fit2.con, list(lower=fit2.con, upper=fit2.full), direction = "both")
summary(fit2.both)

par(mfrow=c(2,2))
plot(fit2.both)
summary(fit2.both)

influencePlot(fit2.full, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook¡¯s distance")

# Train & Test
## Data Split
set.seed(1)
idx = createDataPartition(Boston2$medv, p=0.7, list=F)
train = Boston2[idx,]
test = Boston2[-idx,]

## Train
fit_final.con<-lm(medv~1, data=train)
fit_final.full<-lm(medv~., data=train)
fit_final.both<-step(fit_final.con, list(lower=fit_final.con, upper=fit_final.full), direction='both', data=train)
summary(fit_final.both)

## Prediction
pred<-predict(fit_final.both, test)

## rmse
rmse<-sqrt(mean((test$medv-pred)^2))
rmse # rmse : 5.14
