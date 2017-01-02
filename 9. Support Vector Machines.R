# Chapter 9 Lab: Support Vector Machines
rm(list = ls())
library(e1071)

#####
# Support Vector Classifier

set.seed(1011)
x = matrix(rnorm(20*2), ncol = 2) # generate 2 columns of random data with 20 observations
y = c(rep(-1,10), rep(1,10)) # generate responses
x[y == 1,] = x[y == 1,] + 1 # shift mean by 1
plot(x, col = (3-y)) # plot to see if linearly separable
dat = data.frame(x = x, y = as.factor(y)) # add to df, encode y as factor in order to perform classification 
# (as opposed to SVM-based regression), we must encode the response as a factor variable

# train linear SVM, plot and view output
svmfit = svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index # identifies support vectors (plotted as crosses)
summary(svmfit)

# fit SVM with lower cost: When the cost argument is small, then the margins will be wide and many support vectors 
# will be on the margin or will violate the margin.
svmfit1 = svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit1, dat)
svmfit1$index
summary(svmfit1)

# write custom function to make grid for displaying SVM
make.grid <- function(x, n = 75){
    grange = apply(x, 2, range) # find range of data
    x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
    x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
    expand.grid(x.1 = x1, x.2 = x2) # create grid as df
}
xgrid = make.grid(x) 

### plot SVM again this time with our custom function, which rotates axis vs. default plot which has x.2 on x-axis
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = 0.2) # plot grid
# note col = c('red', 'blue')[as.numeric(ygrid)] recycles the red & blue chr string to set dot colours
points(x, col = y + 3, pch = 19) # plot original data
points(x[svmfit$index,], pch = 5, cex = 2) # plot support vectors


### Now we perform CV in order to set cost parameter (that controls margin width) using default function in e1071 package
# default is 10-fold CV. The we view output of all models and of best model
# note: gamma parameter not needed for linear model (default is 1/data dim)
set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out) 
bestmod = tune.out$best.model # access best model from CV
summary(bestmod) # view best model

### Now we generate a test data set on which to make predictions
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest == 1,] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest)) # save test data as df
ypred = predict(bestmod, testdat) # make predictions on test data using best model
table(predict = ypred, truth = testdat$y) # view results

# retest to see results if we had used a different cost parameter of .01
svmfit = svm(y~., data = dat, kernel = "linear", cost = .01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

# Now consider a situation in which the 2 classes are linearly separable. Then we can find a separating hyperplane 
# using the svm() function. We first further separate the 2 classes in our simulated data so they are linearly separable:

x[y == 1,] = x[y == 1,] + 0.5
plot(x, col = (y + 5)/2, pch = 19) # in this contrived example the data is now barely linearly separable
dat = data.frame(x = x,y = as.factor(y))
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit,dat)

# Support Vector Machine

set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,]+2
x[101:150,] = x[101:150,]-2
y = c(rep(1,150),rep(2,50))
dat = data.frame(x = x,y = as.factor(y))
plot(x, col = y)
train = sample(200,100)
svmfit = svm(y~., data = dat[train,], kernel = "radial",  gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit = svm(y~., data = dat[train,], kernel = "radial",gamma = 1,cost = 1e5)
plot(svmfit,dat[train,])
set.seed(1)
tune.out = tune(svm, y~., data = dat[train,], kernel = "radial", ranges = list(cost = c(0.1,1,10,100,1000),gamma = c(0.5,1,2,3,4)))
summary(tune.out)
table(true = dat[-train,"y"], pred = predict(tune.out$best.model,newdata = dat[-train,]))

# ROC Curves

library(ROCR)
rocplot = function(pred, truth, ...){
    predob  =  prediction(pred, truth)
    perf  =  performance(predob, "tpr", "fpr")
    plot(perf,...)}
svmfit.opt = svm(y~., data = dat[train,], kernel = "radial",gamma = 2, cost = 1,decision.values = T)
fitted = attributes(predict(svmfit.opt,dat[train,],decision.values = TRUE))$decision.values
par(mfrow = c(1,2))
rocplot(fitted,dat[train,"y"],main = "Training Data")
svmfit.flex = svm(y~., data = dat[train,], kernel = "radial",gamma = 50, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.flex,dat[train,],decision.values = T))$decision.values
rocplot(fitted,dat[train,"y"],add = T,col = "red")
fitted = attributes(predict(svmfit.opt,dat[-train,],decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],main = "Test Data")
fitted = attributes(predict(svmfit.flex,dat[-train,],decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],add = T,col = "red")

# SVM with Multiple Classes

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0,50))
x[y =  = 0,2] = x[y =  = 0,2]+2
dat = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x,col = (y+1))
svmfit = svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y~., data = dat, kernel = "linear",cost = 10)
summary(out)
table(out$fitted, dat$y)
dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)

