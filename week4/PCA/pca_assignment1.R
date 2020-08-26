# PCA -> PCR
# Principal Components -> fit Y using linear regression
# PCR model through trainset
# performance comparison through testset

# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)
setwd("/Users/yunho/projects/Tobigs_2018")
if(!require(hydroGOF)) install.packages("hydroGOF"); library(hydroGOF) # for MSE
if(!require(rgl)) install.packages("rgl"); library(rgl)
if(!require(caret)) install.packages("rgl"); library(caret)
if(!require(corrplot)) install.packages("rgl"); library(corrplot)

# Data Load
facebook <- read.csv("Features_Variant_5.csv", header = F)
facebook.test <- read.csv("Features_TestSet.csv", header = F)

# EDA & Data Pre-processing
## Check NaN
colSums(is.na(facebook))
colSums(is.na(facebook.test))

## Data
dim(facebook)
dim(facebook.test)
str(facebook)
str(facebook.test)
summary(facebook)
summary(facebook.test)

## Box plot for the target variable distribution
boxplot(facebook$V54)
boxplot(facebook.test$V54)

## Correlation coefficient check -> Linearity check
cor(facebook)
table(facebook$V38) # V38 : a column with 0 values -> need to drop it

## Drop V38
facebook <- facebook[,!names(facebook) %in% 'V38']
facebook.test <- facebook.test[,!names(facebook.test) %in% 'V38']

## Correlation coefficient check
corrplot(cor(facebook))

# Train/Test Partition - createDataPartition in caret package
facebook <- as.data.frame(facebook)
idx = createDataPartition(facebook$V54, p=0.7, list=F)
train_set = facebook[idx, -53]
test_set = facebook[-idx, -53]

train_set_y = facebook[idx, 53]
test_set_y = facebook[-idx, 53]
dim(train_set)
dim(test_set)

# PCA
train_set.PCA <- prcomp(scale(train_set))
plot(train_set.PCA$x[,1:2], col = train_set_y)
summary(train_set.PCA)
plot(train_set.PCA, type="l")

## Check principal components that explain variance about 80 percent
min(which(summary(train_set.PCA)[[6]][3,] >= 0.8)) # 15
summary(train_set.PCA)[[6]][,15] # 80%


## Extract 15 PCs
train_setPRC <- as.matrix(train_set) %*% train_set.PCA$rotation[,1:15]
test_setPRC <- as.matrix(test_set) %*% train_set.PCA$rotation[,1:15]

final_train = as.data.frame(cbind(train_setPRC, label=train_set_y))
final_test = as.data.frame(cbind(test_setPRC, label=test_set_y))

dim(final_train)
dim(final_test)

head(final_train)
head(final_test)

# Linear Regression
fit <- lm(label~., data=final_train)
summary(fit)

# Prediction
fit_pred <- predict(fit, newdata=final_test[-16])
head(fit_pred)

# RMSE
rmse <- function(real, pred){sqrt(mean((real-pred)^2))}
rmse(final_test$label, fit_pred) # 27.76235

# Model Validation
head(facebook.test)

table(facebook.test$V38)
facebook.test <- facebook.test[,!names(facebook.test) %in% "V38"]

test_x <- facebook.test[,-53]
test_y <- facebook.test[,53]

test_PRC <- as.matrix(test_x) %*% train_set.PCA$rotation[,1:15]
real_test <- as.data.frame(cbind(test_PRC, label=test_y))
dim(real_test)

## Prediction
real_fit_pred <- predict(fit, newdata=real_test[-16])

## RMSE
rmse(real_test$label, real_fit_pred) # 106.029