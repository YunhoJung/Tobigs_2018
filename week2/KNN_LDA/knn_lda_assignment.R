# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")

## packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(readr)) install.packages("readr"); library(readr)

# Data Load
## The function for MAC OS to process Korean when reading csv files
read.any <- function(text, sep = "", ...){
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  
  if (sep != ""|!(setting %in% c("csv", "txt"))) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  
  return(result)
}

pro.train <- read.any('profiles_train.csv', header=TRUE)
pro.train <- as.data.frame(pro.train)
pro.test <- read.any('profiles_test.csv', header=TRUE)
pro.test <- as.data.frame(pro.test)

click.train <- read.any('click_train.csv', header=TRUE)
click.train <- as.data.frame(click.train)
click.test <- read.any('click_test.csv', header=TRUE)
click.test <- as.data.frame(click.test)

pro.train %>% head # a function of dplyr package

# EDA
str(pro.train)
summary(pro.train)
dim(pro.train)

str(pro.test)
summary(pro.test)
dim(pro.test)

str(click.train)
summary(click.train)
dim(click.train)

str(click.test)
summary(click.test)
dim(click.test)



# Data Preprocessing
## 1. DT - Feature engineering
## The sum of st_t = Total session time
a <-click.train %>% group_by(id) %>% summarise(DT = sum(st_t)) # or aggregate
pro.train <- inner_join(pro.train, a) # or merge
pro.train %>% head
str(pro.train)

## 2. PV - Feature engineering
## The sum of st_c = Total page views
a <-click.train %>% group_by(id) %>% summarise(PV = sum(st_c))
pro.train <- inner_join(pro.train,a)
pro.train %>% head

## 3. COV - Feature engineering
## Find out how many different categories they have accessed
## length(unique(cate))/ 22
a <-click.train %>% group_by(id) %>% summarise(COV = round(length(unique(cate))/22, 2))
pro.train <- inner_join(pro.train,a)
pro.train %>% head
str(pro.train)

## 4. Day - Feature engineering
## Total Active Days
click.train$time <- substr(click.train$time,1,8) # up to 8th idx -> yyyymmdd
a <-click.train %>% group_by(id) %>% summarise(Day = length(unique(time)))
pro.train <- inner_join(pro.train,a)
pro.train %>% head


# Data Partition
## 1. DT - Feature engineering
a <-click.test %>% group_by(id) %>% summarise(DT = sum(st_t)) # or aggregate
pro.test <- inner_join(pro.test,a) # or merge
pro.test %>% head

## 2. PV - Feature engineering
a <-click.test %>% group_by(id) %>% summarise(PV = sum(st_c))
pro.test <- inner_join(pro.test,a)
pro.test %>% head

## 3. COV - Feature engineering
a <-click.test %>% group_by(id) %>% summarise(COV = round(length(unique(cate))/22, 2))
pro.test <- inner_join(pro.test,a)
pro.test %>% head

str(pro.test)
str(pro.train)
pro.train$COV
## 4. Day - Feature engineering
click.test$time <- substr(click.test$time,1,8)
a <-click.test %>% group_by(id) %>% summarise(Day = length(unique(time)))
pro.test <- inner_join(pro.test,a)
pro.test %>% head

set.seed(1) # sets the starting number used to generate a sequence of random numbers

# Data Preprocessing - drop unnecessary variables(columns)
pro.train <- pro.train[-1] # drop id
pro.train <- pro.train[-2] # drop job
pro.train <- pro.train[-2] # drop resid
pro.test <- pro.test[-1] # drop id
pro.test <- pro.test[-1] # drop job
pro.test <- pro.test[-1] # drop resid

# Feature scaling
## The range of each data of wdbc are all different
## To see the effect of variables more accurately, normalization needed
summary(pro.train)
summary(pro.test)

## Min-Max scaling - Normalization
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)) )
}

pro.train_n <- as.data.frame(lapply(pro.train[-1], normalize))
pro.train_n$gen <- pro.train$gen
summary(pro.train_n)

pro.test_n <- as.data.frame(lapply(pro.test, normalize))
summary(pro.test_n)

# Vote Classifier
###  num : The number of models to ensemble
###  b,c,d ... : method
voteClassifier <- function(num, knn, lda, glm) {
  ## Train/Test
  ### Cross validation
  cv <- trainControl(method = "cv", number = 5, verbose = T) 
  repcv <- trainControl(method = "repeatedcv", number = 5, repeats = 3, verbose = T)
  
  ### KNN
  knn.grid = expand.grid(
    .k = seq(1,50,by=2)
  ) # tune parameter - k for knn
  train.knn <- train(gen~., pro.train_n, method = "knn", trControl = cv, tuneGrid = knn.grid)
  train.knn$results
  train.knn$bestTune
  predict.knn <- predict(train.knn, pro.test_n)
  
  ### LDA
  train.lda <- train(gen~., pro.train_n, method = "lda", trControl = repcv)
  predict.lda <- predict(train.lda, pro.test_n)
  
  ### GLM(Logistic Regression)
  train.glm <- train(gen~., pro.train_n, method = "glm", trControl = cv)
  predict.glm <- predict(train.glm, pro.test_n)

  
  ## Voting
  testset.pred_majority<-as.factor(ifelse(predict.knn=='남자' & predict.lda=='남자','남자',ifelse(predict.lda=='남자' & predict.glm=='남자','남자',ifelse(predict.glm=='남자' & predict.knn=='남자','남자','여자'))))
  
  ## Return

  return(testset.pred_majority)
}

predict.ensemble <- voteClassifier(3, "knn", "lda", "glm")
predict.ensemble

write.csv(predict.ensemble,'yunho.csv',row.names =F)