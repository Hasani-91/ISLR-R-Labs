# Chapter 9 Lab: Support Vector Machines
rm(list = ls())
library(e1071)

#####
# 1. Support Vector Classifier

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
make.grid <- function(x, n = 125){
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
dat = data.frame(x = x,y = as.factor(y)) # store as df

# We fit the support vector classifier & plot the resulting hyperplane, using a very large value
# of cost so that no observations are misclassified.
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1e5) 
summary(svmfit)
plot(svmfit, dat)

# plot same thing using our custom function
xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19) # plot original data
points(x[svmfit$index,], pch = 5, cex = 2) # plot support vectors

# now try a smaller value of cost (misclassifies but will be more robust on test data)
svmfit = svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

# plot using custom  function
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19) # plot original data
points(x[svmfit$index,], pch = 5, cex = 2) # plot support vectors

#####
# 2. Support Vector Machine

# first generate some data with a non-linear class boundary and plot
set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2 # shift a bit to right
x[101:150,] = x[101:150,] - 2 # shift a bit to left
y = c(rep(1, 150), rep(2,50))
dat = data.frame(x = x, y = as.factor(y))
plot(x, col = y)

train = sample(200, 100) # split data in half randomly for training

# Fit SVM with a radial kernel and γ = 1:
svmfit = svm(y~., data = dat[train,], kernel = "radial",  gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

# Plot also using custom function
d <- dat[train,]
xgrid = make.grid(d[,-3])
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(d[,-3], col = d$y, pch = 19) # plot original data (note need to change colour ref)
points(d[svmfit$index,], pch = 5, cex = 2) # plot support vectors

# Now set high cost parameter to reduce the number of training errors (though risk overfitting)
svmfit = svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue')[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(d[,-3], col = d$y, pch = 19) # plot original data (note need to change colour ref)
points(d[svmfit$index,], pch = 5, cex = 2) # plot support vectors

# Use CV to choose γ and cost parameter
set.seed(1)
tune.out = tune(svm, y~., data = dat[train,], kernel = "radial", ranges = list(cost = c(0.1,1,10,100,1000), gamma = c(0.5,1,2,3,4)))
summary(tune.out)
# calculate test set predictions for model and summarise in table
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))

#####
# 3. ROC Curves
library(ROCR)

# function to plot an ROC curve given a vector containing a numerical score for each observation, 
# pred, and a vector containing the class label for each observation, truth.

rocplot = function(pred, truth, ...){
    predob  =  prediction(pred, truth) # creates object of class prediction
    perf  =  performance(predob, "tpr", "fpr") # check performance of true +ve rate & false +ve rate
    plot(perf, ...)} # plot performance & accept additional arguments

# Note: if the fitted value exceeds zero then the observation is assigned to one class, & 
# if it is less than zero then it is assigned to the other.
# Using decision.values = TRUE when fitting svm() to obtain fitted values and so then predict() 
# function will output the fitted values (not the class) i.e. the distance from boundary

# refit best model now with fitted values
svmfit.opt = svm(y~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.opt,dat[train,], decision.values = TRUE))$decision.values # grab fitted values

# plot data
par(mfrow = c(1,2)) 
rocplot(fitted, dat[train,"y"], main = "Training Data") # plots fitted values on training data
# increase γ to produce a more flexible fit (more local behaviour in radial kernel)
svmfit.flex = svm(y~., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = T)
# x11(); plot(svmfit.flex, dat[train,]) # quickview more flexible fit
fitted = attributes(predict(svmfit.flex,dat[train,],decision.values = T))$decision.values # obtain fitted values
rocplot(fitted,dat[train,"y"], add = T, col = "red") # plots fitted values on training data for more flexible model (too flexible! has fitted training data fully)

# Now plot test data ROCR 
fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"], main = "Test Data") # best model on test data
fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"], add = T, col = "red") # overfitted model on test data

#####
# 4. SVM with Multiple Classes
# If response is a factor containing more than two levels, then svm() will perform 
# multi-class classification using the one-versus-one approach
set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0,50))
x[y == 0, 2] = x[y == 0, 2] + 2
dat = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1)) # 2 classes
svmfit = svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1) # fit SVM
plot(svmfit, dat) # plot results

# plot using custom function
xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid) # predict classifications based on fitted model
plot(xgrid, col = c('red', 'blue', 'green')[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 4, pch = 19) # plot original data
points(x[svmfit$index,], pch = 5, cex = 2) # plot support vectors

#####
# 5. Application to Gene Expression Data

library(ISLR)
names(Khan)

# Data is list containing four components: xtrain, xtest, ytrain, and ytest. 
# xtrain contains the 2308 gene expression values for 63 subjects & ytrain records the corresponding tumor type. 
# xtest & ytest contain the corresponding testing sample information for a further 20 subjects.
# there are 4 tumour types denoted 1-4

# check dimensions
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
# view data: training and test sets consist of 63 and 20 observations respectively.
table(Khan$ytrain) # check amount of each tumour in training data
table(Khan$ytest) # check amount of each tumour in test data

dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain)) # training df

# there are a very large number of features relative to the number of observations. 
# This suggests that we should use a linear kernel, because the additional flexibility 
# that will result from using a polynomial or radial kernel is unnecessary.
out = svm(y~., data = dat, kernel = "linear", cost = 10) # fit linear SVM
summary(out)
table(out$fitted, dat$y) # view results, maybe overfitted as p >> n

# check test data perf (even if good doesn't mean results are stable)
dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)