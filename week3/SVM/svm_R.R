rm(list=ls())

# When using svm with R for data analytics, first of all, there are three packages for svm in R as follows
#e1071 : R library that implements an open source SVM program written in C++
#klaR : R library of the Department of Statistics, Dortmund Technical University, which implemented the algorithm
#kernlab :  R library, which can be easily changed and looks at every aspect in detail
# Going to practice only 2 libraries
install.packages("e1071")
install.packages("kernlab")

library(e1071)

# In e1071 library, a function called svm is used
# The default kernel value is radial
# Cost is a parameter that specifies the degree to prevent overfitting
# In other words, cost is a value that determines how much to pay if misclassified


####################################################
### Example 1) Non-linear SVM models

# Data Creation

# Fixing Seed - set the start of the sequence to a fixed value
set.seed(0801)

# Save into matrix form
x=matrix(rnorm(200),100,2) # 100 times, twice
x
group=ifelse(apply(x*x,1,sum)<=1.4,1,2) # based on variance 1.4 for rows
#apply(var, 1:row;2:col, function)
#ifelse(logic , True, False )

table(group)

# Labels colored above by column in x
plot(x,col=group)

library(e1071)
y=as.factor(group)
y

svm.model=svm(y~x,kernel='radial')
?svm
summary(svm.model)

# Visualization

# Activation of the graph window
quartz(height=10,width=10)

plot(x,pch=c(20,21)[svm.model$fitted],col=c('blue','red')[svm.model$fitted],xlim=c(-3,3),ylim=c(-3,3),xlab='x1',ylab='x2',main="Visualization of Non-linear SVM Predictions")


# ACC
sum(group==svm.model$fitted)/length(group)

# 0.95

######################################
### Example 2) Spam mail

library(e1071)
library(kernlab)

# Data Load
data(spam)
str(spam)
dim(spam)
# The frequency and characteristics of words and symbols are represented as variable values

svm.model<- svm(type~., data=spam, gamma=1, cost=1) # gamma and cost parameters are both set to 1. Originally, should adjust
addmargins(table(spam$type, svm.model$fitted)) # to see results including sum

# Train/Test Partition
n<-nrow(spam)
sub<-sample(1:n,round(0.7*n))
spam.train<-spam[sub,]
spam.test<-spam[-sub,]

svm.model.1<- svm(type~.,data=spam.train, gamma=1, cost=1)
svm.predict.2<-predict(svm.model.1,newdata=spam.test)

addmargins(table(spam.test$type,svm.predict.2)) # to see results including sum as above


# The total error rate
(5+242)/1380 # 0.1789855


#### Hyperparameter tuning
# gamma, cost?
# tune.svm( ) - the function for hyperparameter tuning of SVM model
# However, the kernel needs to be set
# Tune by using (10-fold cross-validation) method
?tune

tune.svm<- tune(svm,type~., data=spam.train,
                kernel="radial", ranges =list(gamma=c(0.1,1,10),
                                              cost=c(0.1,1,10))) # radial : rdf kernel
# Each value is a candidate for gamma and cost

summary(tune.svm)
tune.svm

######################################
### Example 2) Iris data

model <- svm(Species~., data=iris)
model

pred <- predict(model, iris[, -5])

# Store the predicted value of iris by using the model in the variable called pred.model
table(pred, iris$Species)

# Comparison by using table
iris

plot(cmdscale(dist(iris[,-5])), col = as.integer(iris[,5]),
     
     pch = c("o","+")[1:150 %in% model$index + 1])
# The predicted model is plotted using the plot function, support vectors are in the shape of +, and the others are in the form of O

# kernlab package
# ksvm function with iris data
library(kernlab)

irismodel <- ksvm(Species ~ ., data = iris,
  
                  type = "C-bsvc", kernel = "rbfdot", # gaussian kernel
                  
                  kpar = list(sigma = 0.1), C = 10,
                  
                  prob.model = TRUE)



# Model creation with various option values

predict(irismodel, iris[c(3, 10, 56, 68, 107, 120), -5], type = "probabilities") # probability value

predict(irismodel, iris[c(3, 10, 56, 68, 107, 120), -5], type = "decision")

predict(irismodel, iris[c(3, 10, 56, 68, 107, 120), -5])


# And if predict those values by type, able to see that the return values all different
# Also, able to make kernels myself, (originally, there are four kernels)
k <- function(x, y) {
  
  (sum(x * y) + 1) * exp(0.001 * sum((x - y)^2))
  
}

class(k) <- "kernel"


# ksvm with promotergene
data("promotergene")
str(promotergene)
gene <- ksvm(Class ~ ., data = promotergene, kernel = k, C = 10, cross = 5) # In this way, able to make my own kernel


######################################
### Visualization
# Data Load
data(iris)

# Model
svm.model <- svm(Species ~ Sepal.Length + Sepal.Width, data = iris, kernel = "linear")
# the + are support vectors
plot(iris$Sepal.Length, iris$Sepal.Width, col = as.integer(iris[, 5]), 
     pch = c("o","+")[1:150 %in% svm.model$index + 1], cex = 2, 
     xlab = "Sepal length", ylab = "Sepal width")

plot(svm.model, iris, Sepal.Width ~ Sepal.Length, 
     slice = list(sepal.width = 1, sepal.length = 2))

svm.pred  <- predict(svm.model, iris[,-5]) 
table(pred = svm.pred, true = iris[,5]) # show the confusion matrix