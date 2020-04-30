# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

# Simple Linear Regression
fundata <- read.csv("fundata.csv")
str(fundata)

fundata <- fundata[,-1] # remove index
str(fundata)
fit1 <- lm(y1 ~ x1, fundata)
summary(fit1)
fit2 <- lm(y2 ~ x2, fundata)
fit3 <- lm(y3 ~ x3, fundata)
fit4 <- lm(y4 ~ x4, fundata)
summary(fit2);summary(fit3);summary(fit4)

par(mfrow=c(2,2)) # 2x2 plots

plot(fundata$x1, fundata$y1) # scatter plot
abline(fit1) # add a line of fitted simple linear regression model for comparison
plot(fundata$x2, fundata$y2)
abline(fit2)
plot(fundata$x3, fundata$y3)
abline(fit3)
plot(fundata$x4, fundata$y4)
abline(fit4)

# Multi Linear Regression
data <- read.csv("data.csv")
str(data)
head(data)
names(data)<-c("Y","X1","X2","X3","X4")
head(data)
####################################################
# Y : Gasoline fuel efficiency
# X1 : Displacement of engine
# X2 : Horsepower
# X3 : Compression ratio of engine
# X4 : Weight
####################################################

## EDA & Data Preprocessing
### Check NaN
sum(is.na(data)) # 0, if NA, remove NA by row by using data <- na.omit(data)

### EDA
par(mfrow=c(2,3))
hist(data$Y);summary(data$Y)
hist(data$X1);summary(data$X1)
hist(data$X2);summary(data$X2)
hist(data$X3);summary(data$X3)
hist(data$X4);summary(data$X4)
plot(data)

### Fit Linear Regression Model
fit.full <- lm(Y~., data) # fit Y with all possible variables
summary(fit.full) # R-squared: 0.7665,	Adjusted R-squared:  0.7292, significant variable : X1


### Remove some variables based on p-value
fit1 <- lm(Y~ . -X4, data)
summary(fit1) # R-squared:  0.7659,	Adjusted R-squared:  0.7389 
fit2 <- lm(Y~ . -X4 -X3, data)
summary(fit2) # R-squared:  0.7649,	Adjusted R-squared:  0.7475
fit3 <- lm(Y~ . -X4 -X3 -X2, data)
summary(fit3) # R-squared:  0.7601,	Adjusted R-squared:  0.7515 


### Variable selection based on AIC(The lower AIC, the better variable)
#### A model without Explanatory variable(Independent variable)
fit.con <- lm(Y ~ 1, data)
##### Forward selection
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
##### Backward selection
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
##### Stepwise selection
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")

summary(fit.forward) # R-squared:  0.7601,	Adjusted R-squared:  0.7515, significant variable : X1
summary(fit.backward) # R-squared:  0.7601,	Adjusted R-squared:  0.7515, significant variable : X1
summary(fit.both) # R-squared:  0.7601,	Adjusted R-squared:  0.7515, significant variable : X1

## Model Comparison
#anova(fit3, fit.both) # For now no need to use it as they are same models.(In general, p-value > 0.05 -> significant difference x)

## Model Diagnostics & Data Diagnostics
####################################################
# Residuals vs Fitted : To determine Linearity, Homogeneity of Variance of Error and Independence.
# If there is some correlation between residual and predicted values, you can use it for additional variable.
# Normal Q-Q : Plot for Normality Test. Normality -> All points should be on y=x.
# Scale-Location : Plot for determining Homogeneity of Variance. The degree of scattering should be constant.
#                  If not, you can conclude there is something wrong with Homogeniety of Variance.
# Residual vs Leverage : 
# Provides information about individual observation. Able to check Outlier/High Leverage Point/Influentail Observation
# Outlier : An observation that lies outside the overall pattern of a distribution. An observation not well predicted by regression(Very large scale of residual).
# High Leverage Point : Outlier of predictor
# Good Leverage Points : High Leverage, Low Residual
# Bad Leverage Points : High Leverage , High Residual(Outlier)
# Influential Observation : An observation for a statistical calculation whose deletion from the dataset would noticeably change the result of the calculation.
# ->In particular, in regression analysis an influential point is one whose deletion has a large effect on the parameter estimates.
#######################################################

## Model Diagnostics
### PLOT

par(mfrow=c(2,2))
plot(fit.both)
# Residuals vs Fitted - Determine Homogeneity of Variance, Linearity and Independence
# normal Q-Q - Determine Normality

### Normality Test - shapiro.test
e <-resid(fit.both) # residuals
shapiro.test(e) # p-value = 0.8677 -> Normality satisfied

### Independence
plot(predict(fit.both), e) # no exact patterns
plot(predict(fit.both), e, type = 'o')

### Car Packages for vif, outlierTest
install.packages("car")
library(car)

### Multicollinearity
vif(fit.both) # error
vif(fit.full)

## Data Diagnostics
### Outlier 
outlierTest(fit.both)
#### Bonferonni p?? < 0.05 -> determine that it is an outlier
#### So, no outlier

### Influential Observation
influence.measures(fit.both) # * marked : suspected influential observation 
influencePlot(fit.both, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook??s distance") # large circle -> suspected influential observation
influenceIndexPlot(fit.both, id.n=3)
#### Cook's distance measures how much an observation influences the overall model or predicted values
influencePlot(fit.both, id.n=6)
#### Creates a bubble-plot combining the display of Studentizedresiduals, hat-values, and Cook's distance (represented in the circles).

## Conclusion
summary(fit.both) # y=33.487803-0.47056x1
# R-squared:  0.7601,	Adjusted R-squared:  0.7515

## Predicted Value with Linear Regression
predict(fit.both)


###############################################################################################
# Dummy Variables Processing
## Data Load
data2 <- read.csv("education1960_70.csv",header=T,stringsAsFactors=FALSE)
str(data2)
head(data2)
summary(data2)
names(data2)

######################################################
# STATE : State
# Y : Education costs per person
# X1 : Income
# X2 : Resident poplution under 18 (k)
# X3 : Urban resident population(k)
# Region : Northeast(1), North Central(2), South(3), West(4)
# Year : 1960 or 1970
#######################################################

## EDA & Data Preprocessing
### Check NaN
sum(is.na(data2)) # 0, if NA, remove NA by row by using data <- na.omit(data2)

### EDA
par(mfrow=c(2,3))
hist(data2$Y);summary(data2$Y)
hist(data2$X1);summary(data2$X1)
hist(data2$X2);summary(data2$X2)
hist(data2$X3);summary(data2$x3)
hist(data2$Region);summary(data2$Region)
hist(data2$Year);summary(data2$Year)

### Region Dummy Variables Processing(4 Categories -> 3 Dummy Variables)
data2$Z1<-ifelse(data2$Region=="1",1,0)
data2$Z2<-ifelse(data2$Region=="2",1,0) 
data2$Z3<-ifelse(data2$Region=="3",1,0)
names(data2)
unique(data2$STATE)
data2 <- data2[,c(-1,-6)]
names(data2)
#####################################################
# Y : Education costs per person
# X1 : Income
# X2 : Resident poplution under 18 (k)
# X3 : Urban resident population(k)
# Year: 1960 or 1970
# Z1=0 Z2=0 Z3=0 : Region = West
# Z1=1 Z2=0 Z3=0 : Region = Northeast
# Z1=0 Z2=1 Z3=0 : Region = North Central
# Z1=0 Z2=0 Z3=1 : Region = South
######################################################