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

# Data Load
dat = read.csv("dat.csv")
head(dat)
str(dat)
dat$is_cancer = as.factor(dat$is_cancer) # convert cancer variable(int) to factor
dat <- dat[-1] # drop id column - unnecessary variable

# Train/Test Partition 7:3
set.seed(1)
table(dat$is_cancer)
idx = createDataPartition(dat$is_cancer, p = 0.7, list=F)
train = dat[idx,]
test = dat[-idx,]
table(train$is_cancer)
table(test$is_cancer)
str(train)

# trainControl() : It helps to evaluate by uniformly applying a consistent comparison method to each candidate through the function

# Random Forest
## Random Search
control = trainControl(method='cv', search='random', number=5,verbose = TRUE)
rf.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 10,
  trControl = control,
  method="rf")

## Grid Search
rf.grid = expand.grid(
  .mtry = c(1,3,5)
)
control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
rf.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = rf.grid,
  trControl = control,
  method = 'rf'
)
rf.model

pred.rf <- predict(rf.model,test[-31])
confusionMatrix(pred.rf, test[,31])


# XGBoost
## Random Search
control = trainControl(method='cv', search='random', number=5,verbose = TRUE)
xgb.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 10,
  trControl = control,
  method="xgbTree")

xgb.model

pred.xgb <- predict(xgb.model,test[-31])
confusionMatrix(pred.xgb, test[,31])

## Grid Search
xgb.grid = expand.grid(
  nrounds = c(300,500),
  eta = c(0.03,0.05),
  gamma = c(3,5),
  max_depth = c(4,6),
  min_child_weight = c(6,8),
  colsample_bytree = c(0.3,0.5),
  subsample = c(0.2,0.6)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
xgb.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)


# GBM(Gradient Boosting)
## Random Search
control = trainControl(method='cv', search='random', number=2,verbose = TRUE)
gbm.model <- train(
  is_cancer ~ .,
  data = train,
  tuneLength = 3,
  trControl = control,
  method="gbm")

gbm.model$bestTune

pred.gbm <- predict(gbm.model,test[-31]) # 31th column - target variable
confusionMatrix(pred.gbm, test[,31])

## Grid Search
gbm.grid = expand.grid(
  shrinkage = c(0.1,0.3),
  interaction.depth = c(3,6,9),
  n.minobsinnode = c(5,10,15),
  n.trees = c(500,100,1500)
)

control = trainControl(method='cv', search='grid', number=5,verbose = TRUE)
gbm.model <- train(
  is_cancer ~ .,
  data = test,
  tuneGrid = gbm.grid,
  trControl = control,
  method = 'gbm'
)

pred.gbm <- predict(gbm.model,test[-31])
confusionMatrix(pred.gbm, test[,31])

dim(train)

# Lime(Local Interpretable Model-agnostic Explanations) - The algorithm is used to describe individual predictions of black box machine learning models
explainer <- lime(train[,-31], xgb.model)
explanation <- explain(test[1:5,-31],            # test data(no too much data)
                       explainer,                # apply lime
                       labels = NULL,            # specify it if the model is a classifier
                       n_labels = 1,             # specify it if the model is a classifier
                       n_features = 3,           # The number of features to use for each explanation
                       #n_permutations = 2,      # The number of permutations to use for each explanation
                       feature_select = 'auto',  # auto, highest_weighs, none,
                       # forward_selection, lasso_path, tree
                       dist_fun = 'gower',       # distance function
                       kernel_width = NULL       # if dist_fun == gower -> NULL
)
explanation[,1:9]
plot_features(explanation, ncol = 2)
?explain

# # hint1. The columns with many NaN values should be removed. If less than 20%, the NaN values are replaced with appropriate values using mice function. mice function - used to fill in NaN values. Each method fills in NaN values accordingly.
# # mc <- mice(apt[,!names(apt) %in% 'price'], method='rf')
# # miceOutput <- complete(mc)
# # apt2 <- cbind(miceOutput,price=apt$price)
# VIM package
# https://rstudio-pubs-static.s3.amazonaws.com/192402_012091b9adac42dbbd22c4d07cb00d36.html
# # hint2. Dummy coding for categorical variables(library(caret))
# # heat_source_dm = dummyVars('~ heat_source', apt2)
# # heat_source_dm = data.frame(predict(heat_source_dm, apt2))
# #
# # heat_type_dm = dummyVars('~ heat_type', apt2)
# # heat_type_dm = data.frame(predict(heat_type_dm, apt2))
# #
# # apt4 <- apt2[!colnames(apt2) %in% c('heat_source','heat_type')]
# # apt4 <- cbind(apt4,heat_source_dm,heat_type_dm)

# Data preprocessing
## 1. NaN values processing
## 2. dummy coding for categorical variables
# Train/prediction
# Ensemble function

