# Assignment 1
# After splitting BreastCancer data of mlbench package into a train set and test set in a 7 to 3 ratio,
# fit logistic regression and predict the target variable.
# For accurate results, calculate the average of accuracy with random data partition 50 times

# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

library(mice)
library(mlbench)
library(caret) # for creatDataPartition

# Data Load : BreastCancer
data(BreastCancer)

# EDA
?BreastCancer
str(BreastCancer)
summary(BreastCancer) # 16 NaN in Bare.nuclei
BreastCancer = BreastCancer[,-1] # drop ID column

# Check NaN
sum(is.na(BreastCancer)) # 16 NaN in Bare.nuclei
mean(is.na(BreastCancer)) # 2% -> Despite low rate, decided to impute NaN data

# Data Imputation
dataset_impute <- mice(BreastCancer[,1:10],  print = FALSE)
BreastCancer <- complete(dataset_impute)
summary(BreastCancer) # Target valuable(0 : 241, 1 : 458) -> Upsampling Needed
str(BreastCancer)

# Encoding Categorical Variable - Target variable
BreastCancer$Class = as.factor(ifelse(BreastCancer$Class == "malignant",0,1)) # Target variable -> categorical variable

# Categorical Indepedent Variables EDA - Check the distribution of categorical independent variables
plot(BreastCancer$Bare.nuclei)
plot(BreastCancer$Bl.cromatin)
plot(BreastCancer$Normal.nucleoli)
plot(BreastCancer$Mitoses)

# Convert Factor to Numeric for Categorical Indepedent Variables - Too many levels in those variables
BreastCancer[, c(6:9)] <- sapply(BreastCancer[, c(6:9)], as.numeric)
summary(BreastCancer)
str(BreastCancer)

# Formula for glm
select1 = colnames(BreastCancer)[-c(10)] # for formula
str(select1)
summary(select1)
formula1 = formula(paste("Class~", paste(select1,collapse = " + "))) # for glm

# Train/Test Partition
idx = createDataPartition(BreastCancer$Class, p=0.7, list=F)
BCtrain = BreastCancer[idx,]
BCtest = BreastCancer[-idx,]

# Model1
model.glm1 = glm(formula1,  BCtrain, family=binomial)
summary(model.glm1)
pred.glm1 = as.numeric(predict(model.glm1, BCtest, type="response") > 0.5)
confusionMatrix(as.factor(pred.glm1), as.factor(BCtest$Class))
table(pred.glm1)
table(BCtrain$Class)

# Upsampling
BCtrain_up = upSample(subset(BCtrain, select=-Class), BCtrain$Class)
table(BCtrain_up$Class)
formula2 = formula(paste("Class~",paste(select1, collapse = " + ")))

# Model2
model.glm2 = glm(formula2, BCtrain_up, family=binomial)
pred.glm2 = as.numeric(predict(model.glm2, BCtest, type="response") > 0.5)
confusionMatrix(as.factor(pred.glm2), BCtest$Class)
## Accuracy : 0.9139
## Sensitivity : 0.8750          
## Specificity : 0.9343

# Train & Test with Random Data Partition 50 times
acc = 0
n = 0
for(i in 1:50)
{
  idx = createDataPartition(BreastCancer$Class, p=0.7, list=F)
  BCtrain = BreastCancer[idx,]
  BCtest = BreastCancer[-idx,]
  BCtrain_up = upSample(subset(BCtrain, select=-Class), BCtrain$Class)
  model.glm2 = glm(formula2, BCtrain_up, family=binomial) # ??????ƽ ȸ?? ?????? ????
  pred.glm2 = as.numeric(predict(model.glm2, BCtest, type="response") > 0.5) # ????
  result = confusionMatrix(as.factor(pred.glm2), BCtest$Class)
  accuracy = as.numeric(result$overall['Accuracy'])
  acc = acc + accuracy
  n = n + 1
}

# Accuracy Average
avg = acc / n
print(avg) # Accuracy : 0.9285714
