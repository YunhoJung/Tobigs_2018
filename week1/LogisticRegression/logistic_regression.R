# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

library(boot)
library(ROCR)
library(caret)


# 1. Nodal : Nodal Involvement in Prostate Cancer
## Data Load
data(nodal)

## EDA
str(nodal) # 53 obs. of  7 variables
summary(nodal) # Min, 1st Qu, Median, Mean, 3rd Qu, Max
?nodal

table(nodal$m) # Needed to drop column m
table(nodal$r) # Target variable

## Check NaN
sum(is.na(nodal)) # 0, if NA, remove NA by row by using data <- na.omit(data)

## Data Preprocessing
nd = nodal[,-1]
table(nd$r) # 0 : 33, 1 : 20

## Logistic Regression
model = glm(r~., data = nd, family = binomial)
summary(model)

predict(model) # output -> logit

sigmoid = function(x) {
  return(exp(x)/(1+exp(x))) 
}

sigmoid(predict(model)) # Needed to use sigmoid function to get the desired probability value
predict(model, type = "response") # The output -> the values that went through the sigmoid function(type='response')


# 2. Bank : Data for prediction of whether customers sign up for a bank deposit
## Data Load
bank = read.csv("bank-additional.csv", sep = ";")

## EDA
str(bank) # 4119 obs. of  21 variables
summary(bank) # Target valuable(no : 3668 yes : 451) -> Upsampling Needed

## Check NaN
sum(is.na(bank)) # 0, if NA, remove NA by row by using data <- na.omit(data)

## Data Preprocessing - Feature Selection by Hand
select = colnames(bank)[c(1,2,3,6,7,8:10,12,15,17:19,21)]

select_form = colnames(bank)[c(1,2,3,6,7,8:10,12,15,17:19)]
formula1 = formula(paste("y~",paste(select_form, collapse=" + ")))

bank = bank[select]
bank$y = as.factor(ifelse(bank$y == "no",0,1)) # Target variable -> categorical variable
str(bank)

## Train/Test Partition
idx = createDataPartition(bank$y, p = 0.7, list = F)
banktrain = bank[idx,]
banktest = bank[-idx,]

## Model1 : High Accuracy, but Low Specificity
model.glm1 = glm(formula1, banktrain, family = binomial)
pred.glm1 = as.numeric(predict(model.glm1, banktest, type = "response") > 0.5) # cutoff value : .50
confusionMatrix(as.factor(pred.glm1),as.factor(banktest$y)) # numeric to factor : `data` and `reference` should be factors with the same levels
table(pred.glm1)

## Model2 : Specificity risen -> predicted customers better who would actually sign up for a bank deposit
model.glm2 = glm(formula1, banktrain, family = binomial)
pred.glm2 = as.numeric(predict(model.glm2, banktest, type = "response") > 0.3) # cutoff value : 0.30
confusionMatrix(as.factor(pred.glm2),as.factor(banktest$y))
table(pred.glm2)

## Upsample
table(banktrain$y)
banktrain_up = upSample(subset(banktrain, select=-y), banktrain$y)
table(banktrain_up$Class) # upsample train set
formula2 = formula(paste("Class~",paste(select_form, collapse=" + ")))

## Model3 : The Best Model in terms of both Sensitivity and Specificity
model.glm3 = glm(formula2, banktrain_up, family = binomial)
pred.glm3 = as.numeric(predict(model.glm3, banktest, type = "response") > 0.5) # cutoff value : 0.50
confusionMatrix(as.factor(pred.glm3),banktest$y)
table(pred.glm3)

## ROC
pred_glm <- prediction(as.numeric(pred.glm3),as.numeric(banktest$y))
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr")
plot(perf_glm, main = "ROC curve for GLM", col = "blue", lwd = 2)

## AUC
auc_glm = performance(pred_glm, measure = "auc")
auc_glm@y.values[[1]] # AUC : 0.7469024
