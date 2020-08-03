# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

## packages
if(!require(class)) install.packages("class"); library(class) # for knn
if(!require(kknn)) install.packages("kknn"); library(kknn) # for wknn
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)


# Data Load : BreastCancer(Wisconsin Breast Cancer Database)
wdbc <- read.csv('wisc_bc_data.csv', stringsAsFactors = F)
## stringAsFactors = F: Originally, read.csv() function converts all chacters to factors when reading a file,
## but by using stringAsFactors=F, able to read characters in the same format without changing them automatically
## However, in general, it would be better to use it as stringsAsFactors = T(Default)
## Factor -> easy to handle categorical variables

# EDA
str(wdbc)
summary(wdbc)
dim(wdbc) # 569 x 32
# radius : 반지름 / texture : 텍스처 / perimeter : 둘레 / area : 면적 /
# smmothness : 평활도 compactness : 다짐도 / concavity : 요면 /
# concave points : 요면점 / symmetry : 대칭 / fractal dimension : 프렉탈 차원

## Target Variable EDA
table(wdbc$diagnosis)
prop.table(table(wdbc$diagnosis)) # 0.62, 0.38
### B: Benign tumor / M : Malignant tumor
### M detection accuracy - IMPORTANT

# Data Preprocessing
wdbc <- wdbc[-1] # drop ID column - unnecessary variable

## Convert Chacater to Factor for Categorical Indepedent(Target) Variable
wdbc$diagnosis <- factor(wdbc$diagnosis, level=c("B","M"))

# Train/Test Partition - createDataPartition in caret package
set.seed(1) # sets the starting number used to generate a sequence of random numbers
idx <- createDataPartition(y = wdbc$diagnosis, p = 0.7, list =FALSE)
wdbc_train <- wdbc[idx,]
wdbc_test <- wdbc[-idx,]

# Model1 - Basic KNN (majority voting)
wdbc_train %>% str # str(wdbc_train)
model.knn <- knn(wdbc_train[-1], wdbc_test[-1], wdbc_train$diagnosis, k=5) # for random k, wdbc[-1] : target variable
confusionMatrix(model.knn, wdbc_test$diagnosis) # Specificity : 0.9048
### Specificity : measures the proportion of actual malignant that are correctly identified as such
### M detection accuracy - IMPORTANT when it comes to Breast Cancer data

# Model2 - Weighted KNN
## distance, kernel(weight)
model.wknn <- kknn(diagnosis ~., wdbc_train, wdbc_test, k=5, scale = F) # formula, scale=T for normalization(Default)
model.wknn$prob # probability of belonging
model.wknn$D # distance to nearest k
wknn_pred <- model.wknn$fitted.values # values predicted by wknn
confusionMatrix(wknn_pred, wdbc_test$diagnosis) # Specificity : 0.9524

# Feature scaling
summary(wdbc)
## The range of each data of wdbc are all different
## To see the effect of variables more accurately, normalization needed

## Min-Max scaling
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

colnames(wdbc)
wdbc_normal <- as.data.frame(lapply(wdbc[-1], normalize))
summary(wdbc_normal) #  All variables range 0 ~ 1

wdbc_normal$diagnosis <- wdbc$diagnosis # No need to normalize the target variable in this case

wdbc_train_n <- wdbc_normal[idx,]
wdbc_test_n <- wdbc_normal[-idx,]


# Model1 - Basic KNN (majority voting)
wdbc_pred_n <- knn(wdbc_train_n[-31],wdbc_test_n[-31], wdbc_train_n$diagnosis, k=5) # for random k
confusionMatrix(wdbc_pred_n, wdbc_test$diagnosis) # Specificity : 0.9524

# Model2 - Weighted KNN
## distance, kernel(weight)
wknn.model <- kknn(diagnosis ~., wdbc_train_n, wdbc_test_n, k=5, scale = F)
wdbc_pred_n2 <- wknn.model$fitted.values
confusionMatrix(wdbc_pred_n2, wdbc_test$diagnosis) # Specificity : 0.9524

# Cross-Validation - Find out an optimal K(Best K)
# ks = 실험할 k, 일부러 홀수로 지정하였다.
wdbc.cv <- train.kknn(diagnosis ~., wdbc_train_n, 
                      ks = seq(1, 50, by=2), scale = T);wdbc.cv # ks -> try all k range in 1,3,5,...,49
best_k <- wdbc.cv$best.parameters$k;best_k # k = 27

# Prediction
wdbc_pred_cv <- kknn(diagnosis ~., train = wdbc_train_n, test = wdbc_test_n, k = best_k, scale = F)
wdbc_pred_cv <- wdbc_pred_cv$fitted.values

confusionMatrix(wdbc_pred_cv, wdbc_test$diagnosis) # Specificity : 0.9524



#########################################################
#### caret package
#### 
#### https://topepo.github.io/caret/available-models.html

# Cross-Validation
cv <- trainControl(method = "cv", number = 5, verbose = T) # verbose : whether to print the progress on the screen
repCv <- trainControl(method = "repeatedcv", number = 5,repeats = 3, verbose = T) # repeated cross-validation
 
# KNN
## Hyperparameter tuning
knn.grid = expand.grid(
  .k = c(1,3,5,7,9)
)

## Train
train.knn <- train(diagnosis~.,wdbc_train_n, method = "knn",trControl = cv,
                   tuneGrid = knn.grid) # caret package
train.knn$results
train.knn$bestTune # Which one is the best K? K = 5, 9

## Prediction
predict.knn <- predict(train.knn,wdbc_test_n)
confusionMatrix(predict.knn, wdbc_test$diagnosis)

# Weighted KNN
## Hyperparameter tuning
wknn.grid = expand.grid(
  .kmax = c(1,3,5), # 
  .distance = c(1,2), # 1 for manhattan distance, 2 for euclidean distance
  .kernel = "optimal"
)

## Train
train.wknn <- train(diagnosis~.,wdbc_train_n, method = "kknn", trControl = cv,
                    tuneGrid = wknn.grid)

## Prediction
predict.wknn <- predict(train.wknn,wdbc_test_n)
confusionMatrix(predict.wknn, wdbc_test$diagnosis)

# LDA
## Train
train.lda <- train(diagnosis~.,wdbc_train_n, method = "lda", trControl = repCv)

## Prediction
predict.lda <- predict(train.lda,wdbc_test_n)
confusionMatrix(predict.lda, wdbc_test$diagnosis)

# Logistic Regression
## Train
train.glm <- train(diagnosis~.,wdbc_train_n, method = "glm", trControl =cv)

## Prediction
predict.glm <- predict(train.glm,wdbc_test_n)
confusionMatrix(predict.glm, wdbc_test$diagnosis)