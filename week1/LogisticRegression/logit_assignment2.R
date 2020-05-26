# Assignment 2
# Load the psub.Rdata data and divide data into a train set and test set in a 7 to 3 ratio.
# Create a confusion matrix by fitting and predicting a logistic regression model for the remaining variables except SCHL,
# by creating bachdeg variable with two categories for those with a bachelor's degree or higher, and those who do not,
# using only AGEP, SEX, COW, PINCP, and SCHL variables.

# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)

library(caret) # for creatDataPartition

# Data Load : psub.Rdata
load("psub.RData")
str(psub) # 1224 obs. of  289 variables

# Feature Selection - using only AGEP, SEX, COW, PINCP and SCHL variables
selected <- c("AGEP", "SEX", "COW", "PINCP", "SCHL")
psub <- psub[,selected]
names(psub)

# EDA
sum(is.na(psub)) # 0

str(psub)
summary(psub)
######################################################
# AGEP : age
# SEX : gender
# COW: class of worker
# PINCP: personal income
# SCHL: level of education
#######################################################

levels(psub$COW)
######################################################
# [1] "Employee of a private for-profit"
# [2] "Federal government employee"     
# [3] "Local government employee"       
# [4] "Private not-for-profit employee" 
# [5] "Self-employed incorporated"      
# [6] "Self-employed not incorporated"  
# [7] "State government employee"
#######################################################

levels(psub$SCHL)
# ---------------------------------# who have at least a bachelor's degree
# "Bachelor's degree"
# "Doctorate degree"               
# "Master's degree"               
# "Professional degree"
# ---------------------------------# who don't
# "no high school diploma"
# "Regular high school diploma"        
# "some college credit, no degree" 
# "GED or alternative credential"
# "Associate's degree" 
# ---------------------------------#
#3,4,6,7 // 1,2,5,8,9 -> bachdeg variable

# Data Preprocessing - bachdeg variable
psub$bachdeg=as.numeric(psub$SCHL)
n=length(psub$bachdeg)
for(i in 1:n){
  if(psub$bachdeg[i]==3||psub$bachdeg[i]==4||psub$bachdeg[i]==6||psub$bachdeg[i]==7){
    psub$bachdeg[i]=1
  }else{
    psub$bachdeg[i]=0
  }
}
psub$bachdeg=as.factor(psub$bachdeg)
psub<-psub[-c(5)] # drop SCHL
str(psub$bachdeg)
str(psub) # 1224 obs. of  6 variables
summary(psub) # Target valuable(0:815, 1:409) -> Upsampling Needed

# Formula for glm 
select = colnames(psub)[0:4] 
formula1 = formula(paste("bachdeg~",paste(select,collapse = " + ")))

# Train/Test Partition 7:3
idx = createDataPartition(psub$bachdeg, p=0.7, list=F)
psubtrain = psub[idx,]
psubtest = psub[-idx,]

# Model1
model.glm1 = glm(formula1, psubtrain, family=binomial)
pred.glm1 = as.numeric(predict(model.glm1, psubtest, type="response") > 0.5)
confusionMatrix(as.factor(pred.glm1), as.factor(psubtest$bachdeg))
## Accuracy : 0.7541
## Sensitivity : 0.8893
## Specificity : 0.4836
table(pred.glm1)
table(psubtrain$bachdeg) # Upsampling Needed

# Upsampling
psubtrain_up = upSample(subset(psubtrain, select=-bachdeg), psubtrain$bachdeg)
table(psubtrain_up$Class)
formula2 = formula(paste("Class~",paste(select11, collapse = " + ")))

# Model2
model.glm2 = glm(formula2, psubtrain_up, family=binomial)
pred.glm2 = as.numeric(predict(model.glm2, psubtest, type="response") > 0.5)
confusionMatrix(as.factor(pred.glm2), psubtest$bachdeg)
## Accuracy : 0.735
## Sensitivity : 0.7746
## Specificity : 0.6557
table(pred.glm2)
