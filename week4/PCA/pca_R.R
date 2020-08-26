#####################################
# Principal Component Analysis(PCA) #
# 2018-08-08 Tobigs Week 04 Class   #
#####################################

# Set Up
rm(list=ls())
cur_dir = getwd()
setwd(cur_dir)
if(!require(rgl)) install.packages("rgl"); library(rgl)
if(!require(XQuartz)) install.packages("XQuartz"); library(XQuartz)

# 1. Random Data Generation
## 3-D data
## When generating the data for PCA implementation, the average of the dataset should always be zero! Normalization
set.seed(10)
X <- rnorm(n=1000, mean=0, sd=100) 
Y <- rnorm(n=1000, mean=0, sd=10)
Z <- rnorm(n=1000, mean=0, sd=50)
range <- c(min(X), max(X)) # change the range based on the x-axis, the axis which has the largest variance
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range)

## Able to see that the data points are distributed along the x-axis, and the samples on the y-axis are very narrow
## It seems that data can be explained in x-axis and z-axis, so let's sort the x-axis and z-axis so that there is correlation
X <- sort(rnorm(n=1000, mean=0, sd=100))
Y <- rnorm(n=1000, mean=0, sd=10)
Z <- sort(rnorm(n=1000, mean=0, sd=50))
range <- c(min(X), max(X))
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range) 
### Random Data Generation done ###

# 2. PCA Implementation
## 2-1. Data Matrix
OriginMatrix <- cbind(X, Y, Z) # 1000*3 matrix
head(OriginMatrix)

## 2-2. Covariance Matrix
CovMatrix <- cov(OriginMatrix)
CovMatrix

## 2-3. Eigen Value and Eigen Vector
lambdaA <- eigen(CovMatrix) # eigen(): return Eigen Value, Eigen Vector 
lambdaA # lambdaA$values for Eigen Value, lambdaA$vectors for Eigen Vector
## The component of Values and the column of vectors are mapped one-to-one.

## 2-4. New Data Set Generation(linear combination of OriginMatrix and Eigen Vector)
TransformedMatrix <- OriginMatrix %*% lambdaA$vectors
head(TransformedMatrix)

## 2-5. Check Linear Transformation of Dataset
range2 <- c(min(TransformedMatrix[,1]), max(TransformedMatrix[,1]))
plot3d(TransformedMatrix[,1], TransformedMatrix[,2], TransformedMatrix[,3], xlim=range2, ylim=range2, zlim=range2, col='red')
plot3d(X, Y, Z, xlim=range, ylim=range, zlim=range, add=T)
## Since it is not dimensionally reduced, the newly created variable TransformedMatrix is a linear transformation of OriginMatrix

## 2-6. Check the Component Impact of the variables
sd(TransformedMatrix[,1])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.9030655
sd(TransformedMatrix[,2])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.07973736
sd(TransformedMatrix[,3])/(sd(TransformedMatrix[,1])+sd(TransformedMatrix[,2])+sd(TransformedMatrix[,3]))# 0.01719718
## Since the impact of the third variable is small, remove it and plot
plot(TransformedMatrix[,1], TransformedMatrix[,2]) # all data are distributed in pc1 pc2

# 3. The Implemented PCA vs PCA Functions
## SVD(Singular Value Decomposition) using (prcomp와 princomp)
## prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, rank. = NULL, ...)
## x : data, retx : the axis rotation of variables, center=zero : the origin settings, scale : normalization
## prcomp : sdev, rotation(eigenvetors), center, scale, x(principal component)
pca <- prcomp(OriginMatrix, center=T, scale=T) # center=T(the center of the data -> 0(mean -> 0)), scale=T(variance -> 1)
## Interpretation of pca results: the variable with linear equation of PC1 =  0.70636575 * X + 0.04628937 * Y + 0.70633188 * Z
pca$sdev
pca$rotation # rotation(eigenvetors)

# 4. PCA in Data Pre-processing Step using dataset
## Data Load & EDA
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline") # setting variable name
str(wine) # Checking the data structure
head(wine) # Viewing the data 
summary(wine) 
### 178 data with 13 variables
### explanatory variables are all continuous variables
### the predictor variable Y(Cvs)needed to be converted to factor(Cvs: The type of wine into one of three classes)
unique(wine$Cvs)
wine$Cvs <- factor(wine$Cvs) # converted to factor
pairs(wine[,-1], col = wine$Classes, upper.panel = NULL, pch = 16, cex = 0.5) # 변수 간 상관성 확인 # upper.panel = NULL : meaning not to show the upper panel
### -1 : to see the relationship between variables beside y
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5) # legend next to the graph

## Data Partition
set.seed(10)
idx <- sample(1:nrow(wine), round(nrow(wine)*0.7), replace = F) # able to use createDataPartition
train <- wine[idx, -1] ; test <- wine[-idx, -1] 
train_label <- wine[idx, 1] ; test_label <- wine[-idx, 1]

dev.off() # update the plot, meaning will no longer send pdf files
winePCA <- prcomp(scale(train)) # pca besides the predictor variable, y. Two principal components
winePCA$x
plot(winePCA$x[,1:2], col = wine$Cvs) # $x[,1:2] -> going to use two principal components

## Determining the Number of Variables(elbow point & cumulative proportion)
plot(winePCA, type="l") # Good to include until the 3rd or 4th variable - subjective criteria
summary(winePCA) # Pc4 which is the elbow point can explain about 74 percent 
min(which(summary(winePCA)[[6]][3,] >= 0.85)) # raise the cumulative ratio to the desired one and get the index of the main component with cumulative explanatory percentage equal to 85% or more
### Originally it should be done using principal components that are over 85 percent.(6th component)
### PCA -> the first one is the most important component
### summary(winePCA)[[6]]  =>  Importance of components
### summary(winePCA)[[6]][3,]  =>  Cumulative Proportion

## The Dot Product of the Existing Data Matrix and the Eigen Vector
### The process of creating a new principal component through matrix multiplication
### calculate the constituent values of the new principal component
### rotation - coefficient values
### a,b of pc1 <- a*x1 + b*x2 +...
trainPRC <- as.matrix(train) %*% winePCA$rotation
testPRC <- as.matrix(test) %*% winePCA$rotation

trainF <- cbind(as.data.frame(trainPRC), train_label)
testF <- cbind(as.data.frame(testPRC), test_label)
colnames(trainF)[14] <- "label"; colnames(testF)[14] <- "label"
str(trainF)

## Linear Regression with Principal Components
fit <- lm(label~., family = "binomial", data = trainF)
summary(fit)

## Prediction
fit_pred <- predict(fit, type="response", newdata = testF)
test_pred<- round(fit_pred)

table <- table(test_label, test_pred)
table
sum(diag(table))/sum(table) # 96%
