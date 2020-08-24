# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

## packages
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
if(!require(lime)) install.packages("lime"); library(lime)
if(!require(mice)) install.packages("mice"); library(mice)
if(!require(mlbench)) install.packages("mlbench"); library(mlbench)
if(!require(caretEnsemble)) install.packages("caretEnsemble"); library(caretEnsemble)
if(!require(gbm)) install.packages("gbm"); library(gbm)
if(!require(hydroGOF)) install.packages("hydroGOF"); library(hydroGOF) # for MSE

# Data Load
apt_train <- read.csv('apt_train.csv')
head(apt_train)
str(apt_train)
apt_train<-apt_train[-1] # drop X column
apt_test <- read.csv('apt_test.csv')
head(apt_test)
str(apt_test)

# Data Preprocessing
## Check Nan/NaN processing
## NaN value ratio greater than or equal to 20% - remove the columns/NaN value ratio less than 20% - replace NaN values using mice function
28663*0.2 # apt_train - 5733(5732.6)
2973*0.2 # apt_test - 595(594.6)
colSums(is.na(apt_train))
colSums(is.na(apt_test))
# building_coverage_ratio, commute_dmc, commute_seongsu, commute_youngsan, commute_chungmuro
# floor_area_ratio, floor_min, parking_inside, parking_outside, permission_date, slope

### Remove the columns with NaN value ratio greater than or equal to 20%
apt_train2<-subset(apt_train,select=-c(building_coverage_ratio,commute_dmc,commute_seongsu,commute_yongsan,commute_chungmuro,floor_area_ratio,floor_max,floor_min, parking_inside,parking_outside,parking_rate,permission_date,slope))
colSums(is.na(apt_train2))

### Replace NaN values of the columns with NaN value ratio less than 20% using mice function
mc<-mice(apt_train2[,!names(apt_train2) %in% 'price'], method='rf')
miceOutput<-complete(mc)
apt_train3<-cbind(miceOutput,price=apt_train2$price)

### Remove the columns with NaN value ratio greater than or equal to 20%
apt_test2<-subset(apt_test,select=-c(building_coverage_ratio,commute_dmc,commute_seongsu,commute_yongsan,commute_chungmuro,floor_area_ratio,floor_max,floor_min, parking_inside,parking_outside,parking_rate,permission_date,slope))
colSums(is.na(apt_test2))

### Replace NaN values of the columns with NaN value ratio less than 20% using mice function
mc<-mice(apt_test2, method='rf')
apt_test3<-complete(mc)
#find_na(apt_train, rate = TRUE)

#write.csv(apt_train,'apt_train_mice.csv',row.names = F)
#write.csv(apt_test,'apt_test_mice.csv',row.names = F)
#apt_train3 <- read.csv('apt_train_mice.csv')
#apt_test3 <- read.csv('apt_test_mice.csv')

## Dummy coding for categorical variables
## asile_type, earthquake, heat_source, heat_type -> Dummy coding needed
## Since the level of heat_source in apt_train3 and apt_test3 don't match, process them by adding the OIL level to apt_test3$heat_source
summary(apt_train3$heat_source)
summary(apt_test3$heat_source)
apt_test3$heat_source <- factor(apt_test3$heat_source, levels=c(levels(apt_test3$heat_source), "OIL"))
summary(apt_test3$heat_source)

asile_type_dm = dummyVars('~ asile_type', apt_train3)
asile_type_dm = data.frame(predict(asile_type_dm, apt_train3))

earthquake_dm = dummyVars('~ earthquake', apt_train3)
earthquake_dm = data.frame(predict(earthquake_dm, apt_train3))

heat_source_dm = dummyVars('~ heat_source', apt_train3)
heat_source_dm = data.frame(predict(heat_source_dm, apt_train3))

heat_type_dm = dummyVars('~ heat_type', apt_train3)
heat_type_dm = data.frame(predict(heat_type_dm, apt_train3))

apt_train4 <- apt_train3[!colnames(apt_train3) %in% c('asile_type_dm','earthquake_dm','heat_source','heat_type')]
apt_train4 <- cbind(apt_train3,asile_type_dm, earthquake_dm,heat_source_dm,heat_type_dm)

## Dummy coding for test
asile_type_dm = dummyVars('~ asile_type', apt_test3)
asile_type_dm = data.frame(predict(asile_type_dm, apt_test3))

earthquake_dm = dummyVars('~ earthquake', apt_test3)
earthquake_dm = data.frame(predict(earthquake_dm, apt_test3))

heat_source_dm = dummyVars('~ heat_source', apt_test3)
heat_source_dm = data.frame(predict(heat_source_dm, apt_test3))

heat_type_dm = dummyVars('~ heat_type', apt_test3)
heat_type_dm = data.frame(predict(heat_type_dm, apt_test3))

apt_test4 <- apt_test3[!colnames(apt_test3) %in% c('asile_type_dm','earthquake_dm','heat_source','heat_type')]
apt_test4 <- cbind(apt_test3,asile_type_dm, earthquake_dm,heat_source_dm,heat_type_dm)
str(apt_train4)
str(apt_test4)

# STACKING function
# Finding the best model with train data
# By making a new target variable with the average of the results (predicted values) of the three models into a new target variable,
# training the model(stacking) using the best model in terms of the performance(mse)
STACKING = function(data_train,data_test){
  ### grid search
  rf.grid = expand.grid(
    .mtry = c(1,3,5)
  )
  xgb.grid = expand.grid(
    nrounds = c(300,500),
    eta = c(0.03,0.05),
    gamma = c(3,5),
    max_depth = c(4,6),
    min_child_weight = c(6,8),
    colsample_bytree = c(0.3,0.5),
    subsample = c(0.2,0.6)
  )
  gbm.grid = expand.grid(
    shrinkage = c(0.1,0.3),
    interaction.depth = c(3,6,9),
    n.minobsinnode = c(5,10,15),
    n.trees = c(500,100,1500)
  )
  
  ### comparison of the performances by partitioning train data to find the best model based on the accuracy of the prediction
  idx = createDataPartition(data_train$price, p = 0.7, list=F)
  train_model = data_train[idx,]
  test_model = data_train[-idx,]
  
  control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
  rf.model <- train(
    price ~ .,
    data = train_model,
    tuneGrid = rf.grid,
    trControl = control,
    method = 'rf'
  )
  xgb.model <- train(
    price ~ .,
    data = train_model,
    tuneGrid = xgb.grid,
    trControl = control,
    method = 'xgbTree'
  )
  gbm.model <- train(
    price ~ .,
    data = train_model,
    tuneGrid = gbm.grid,
    trControl = control,
    method = 'gbm'
  )
  pred.rf <- predict(rf.model,subset(test_model,select=-c(price))) # the predicted values of rf
  pred.xgb <- predict(xgb.model,subset(test_model,select=-c(price))) # the predicted values of xgb
  pred.gbm <- predict(gbm.model,subset(test_model,select=-c(price))) # the predicted values of gbm
  #mser.rf <- mse(pred.rf,subset(test_model,select=c(price))) # mse comparison using mse function(confusionMatrix for classification(factor))
  #mser.xgb <- mse(pred.xgb,subset(test_model,select=c(price)))
  mser.xgb<-mse(pred.xgb,test_model$price)
  #mser.gbm <- mse(pred.gbm,subset(test_model,select=c(price)))
  mser.gbm<-mse(pred.gbm,test_model$price)
  
  ### stacking
  rf.model2 <- train(
    price ~ ., 
    data = data_train,
    tuneGrid = rf.grid,
    trControl = control,
    method = 'rf'
  )
  xgb.model2 <- train(
    price ~ .,
    data = data_train,
    tuneGrid = xgb.grid,
    trControl = control,
    method = 'xgbTree'
  )
  gbm.model2 <- train(
    price ~ .,
    data = data_train,
    tuneGrid = gbm.grid,
    trControl = control,
    method = 'gbm'
  )
  pred.rf2 <- predict(rf.model2,data_test)
  pred.xgb2 <- predict(xgb.model2,data_test)
  pred.gbm2 <- predict(gbm.model2,data_test)
  pred.targ=cbind(pred.rf2,pred.xgb2,pred.gbm2)
  avg.targ=rowMeans(pred.targ)
  data_test2=cbind(data_test,avg.targ)
  
  if(min(mser.rf,mser.xgb,mser.gbm)==mser.rf){# if rf has the smallest mse value (best performing)
    rf.model3 <- train(
      avg.targ ~ .,
      data = data_test2,
      tuneGrid = rf.grid,
      trControl = control,
      method = 'rf'
    )
    pred = predict(rf.model3,subset(data_test2,select=-c(avg.targ)))
  }
  else if(min(mser.rf,mser.xgb,mser.gbm)==mser.xgb){# if xgb has the smallest mse value (best performing)
    xgb.model3 <- train(
      avg.targ ~ .,
      data = data_test2,
      tuneGrid = xgb.grid,
      trControl = control,
      method = 'xgbTree'
    )
    pred = predict(xgb.model3,subset(data_test2,select=-c(avg.targ)))
  }
  else if(min(mser.rf,mser.xgb,mser.gbm)==mser.gbm){# if gbm has the smallest mse value (best performing)
    gbm.model3 <- train(
      avg.targ ~ .,
      data = data_test2,
      tuneGrid = gbm.grid,
      trControl = control,
      method = 'gbm'
    )
    pred = predict(gbm.model3,subset(data_test2,select=-c(avg.targ)))
  }
  return(pred)
}

# Prediction
pred = STACKING(data_train=apt_train4,data_test=apt_test4)
apt_test$price = pred
str(apt_test)
write.csv(apt_test,'apt_pred.csv',row.names = F)