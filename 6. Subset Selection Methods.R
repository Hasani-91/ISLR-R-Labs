rm(list = ls())
   
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
#####
# Chapter 6 Lab 1: Subset Selection Methods
# Best Subset Selection (want to predict salary variable)
# Data clean-up
Hitters <- Hitters
dim(Hitters)
sum(is.na(Hitters$Salary)) # how many NAs we have
Hitters = na.omit(Hitters) # remove variables with NA for salary
sum(is.na(Hitters)) # check if anymore NAs

# The regsubsets function (leaps library) performs best subset selection by identifying 
# the best model that contains a given number of predictors
# where best is quantified using RSS.
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full) # by default only reports up to 8 variable model

# fit all 19 variables (R automatically encodes qualitative variables)
# use contrasts() to see how they are coded - affects interpretation of coeffs
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary) # which metrics are contained in the summary
reg.summary$rsq # view R^2 of each model fit (i.e. best 1, 2, 3 variable model etc...)

# plot results
par(mfrow=c(2,2))
# RSS
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
# adj R^2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
m <- which.max(reg.summary$adjr2) # find max adj R^2 model 
points(m,reg.summary$adjr2[m], col="red",cex=2,pch=20) # add point to highlight max
# Cp
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
cp.min <- which.min(reg.summary$cp)
points(cp.min,reg.summary$cp[cp.min],col="red",cex=2,pch=20)
# BIC
BIC.min <- which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(BIC.min,reg.summary$bic[BIC.min],col="red",cex=2,pch=20)

# shows which features are in full model with 1 to 19 variables
graphics::plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

##### 
# Forward and Backward Stepwise Selection

# forward stepwise and output
regfit.fwd = regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

# backward stepwise and output
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)

<<<<<<< HEAD
# coefficients of models with 7 features/variables
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#####
### Choosing Among Models
=======
# Choosing Among Models
# Note: there is no predict() method for regsubsets(), hence end up writing own function
>>>>>>> origin/master

# create random sample to create training and test data
set.seed(1)
<<<<<<< HEAD
train = sample(c(TRUE,FALSE), nrow(Hitters) ,rep=TRUE)
test = (!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
=======
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
# fit best subset selection for each model up to 19 variables on training subset
regfit.best = regsubsets(Salary~., data = Hitters[train,], nvmax=19)

# make a model matrix from the test data - this has subsetted the original data
# and encoded the qual variables ready for regression, plus added intercept coeff
test.mat = model.matrix(Salary~., data = Hitters[test,])

# compute the validation set error for the best model of each model size
val.errors=rep(NA,19) # set up results vector

# Now we run a loop, and for each size i, we extract the coefficients from regfit.best 
# for the best model of that size, multiply them into the appropriate columns of the 
# test model matrix to form the predictions, and compute the test MSE.
>>>>>>> origin/master
for(i in 1:19){
    coefi = coef(regfit.best, id=i) # extract fitted model coeffs from training data
    pred = test.mat[,names(coefi)]%*%coefi # make prediction on test data
    val.errors[i] = mean((Hitters$Salary[test] - pred)^2) # compute test MSE
}

val.errors # view MSE for all models
minBSS <- which.min(val.errors) # find best model
coef(regfit.best, minBSS) # view coeffs of model with best test MSE from all BSS models

### Now write our own predict method to capture the work we did above
# takes a regression model as object, data to predict on and makes prediction

predict.regsubsets = function(object, newdata, id, ...) {
    form = as.formula(object$call[[2]]) # extracts formula from fitted model, e.g. Salary ~ .
    mat = model.matrix(form, newdata) # sets up design matrix
    coefi = coef(object, id = id) # extract fitted model coeffs from training data
    xvars = names(coefi)
    mat[,xvars]%*%coefi # make prediction
}

# Now perform BSS on full data set & select best 10 variable model
regfit.best = regsubsets(Salary~., data=Hitters, nvmax = 19)
coef(regfit.best, 10) # view coeffs (obv diff to best 10 variable model on training data)

# Now forget 10 variable model and try to choose among the models of different sizes 
# using crossvalidation with k folds
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)
# create results matrix, we're going to have k errors for each of the 19 BSS models
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19))) 

# For each of the k folds, fit all 19 BSS models to training data (i.e. everything except the kth fold)
# then for each of the 19 models, predict on the test (fold) data & compute test MSE
for(j in 1:k){
    best.fit = regsubsets(Salary~., data = Hitters[folds != j,], nvmax = 19)
    for(i in 1:19){
        pred = predict(best.fit, Hitters[folds == j,], id = i) # predict using our custom function
        cv.errors[j,i] = mean((Hitters$Salary[folds == j] - pred)^2) # compute test MSE
    }
}

mean.cv.errors = apply(cv.errors, 2, mean) # average errors of each model across folds
mean.cv.errors # view results
par(mfrow = c(1,1))
plot(mean.cv.errors, type = 'b') # plot results (best is 11 variable model)

# now fit to full data to get coeffs of 11 variable model
reg.best = regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best,11)

#####
# Chapter 6 Lab 2: Ridge Regression and the Lasso
# use glmnet package (has slightly different syntax from other model fitting functions)
# If alpha = 0 then a ridge regression model is fit, and if alpha = 1 then a lasso model is fit.

# The model.matrix() function is particularly useful for creating x; not only does it produce a 
# matrix corresponding to the 19 predictors but it also automatically transforms any 
# qualitative variables into dummy variables.

# creates design matrix and removes intercept term as don't shrink this in RR
x = model.matrix(Salary~., Hitters)[,-1] 
y = Hitters$Salary

# Ridge Regression

grid = 10^seq(10, -2, length = 100) # set grid of lambda values to fit over (from 10^10 to 10^-2)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) # perform ridge regression
dim(coef(ridge.mod)) # so we have returned 20 coeffs for each value of lambda (incl. intercept)

# view coeffs for a large value of lambda (λ = 11,498) and calc L2 norm
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # L2 norm (don't include intercept!)

# view coeffs for a smaller value of lambda (λ = 705) and calc L2 norm, should be larger
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) # L2 norm (don't include intercept!)

# can predict RR coeffs for a new value of lambda, say λ = 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

# Now split the samples into a training set and a test set in order to estimate the test error 
# of ridge regression and the lasso
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test] # salary values for test sample

# fit ride model on training data
ridge.mod = glmnet(x[train,], y[train], alpha = 0,lambda = grid, thresh = 1e-12)
# evaluate MSE on the test set, using λ = 4
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)

# for comparison fit model with just an intercept (i.e. predict each test observation using
# the mean of the training observations)
mean((mean(y[train])-y.test)^2)
# fit model for large lambda (essentially shrinks all coeffs to 0, hence same as just using intercept)
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

# Now check if using λ = 4 is better than least squares
# calculate both using predicted coeffs from our fitted model and the lm function just to compare
ridge.pred = predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred - y.test)^2) # so shrinkage does help
lm(y ~ x, subset = train) # fit lm model just to compare
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]

# Now instead of arbitrarily setting λ = 4, use CV to choose λ
# By default the function cv.glmnet() performs ten-fold cross-validation
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0) # perform CV for RR
plot(cv.out) # note scale is log(λ)
bestlam = cv.out$lambda.min # view best model 
bestlam

# We see that the value of λ that results in the smallest CV error is 212
# Now calc test MSE associated with this value of λ

ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2) # test MSE with λ = 212

# Finally, refit our ridge regression model on the full data set, using the value of λ 
# chosen by CV, and examine the coefficient estimates.
out = glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# The Lasso
# now use alpha = 1 and see if better than ridge regression
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
# perform CV and compute associated test MSE
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test,]) # predict for best lambda
mean((lasso.pred - y.test)^2) # compute test MSE

out = glmnet(x, y, alpha = 1, lambda = grid) # fit over full data using grid of lambdas
lasso.coef = predict(out,type = "coefficients", s = bestlam)[1:20,] # predict using best lambda from CV
lasso.coef # has set some coeffs to exactly 0
lasso.coef[lasso.coef != 0] 

#####
# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

set.seed(2)
# fits pcr model and computes 10 fold CV for each component used
pcr.fit = pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")
set.seed(1)
pcr.fit = pcr(Salary~., data = Hitters,subset = train,scale = TRUE, validation = "CV")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred = predict(pcr.fit,x[test,],ncomp = 7)
mean((pcr.pred-y.test)^2)
pcr.fit = pcr(y~x,scale = TRUE,ncomp = 7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit = plsr(Salary~., data = Hitters,subset = train,scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
pls.pred = predict(pls.fit,x[test,],ncomp = 2)
mean((pls.pred-y.test)^2)
pls.fit = plsr(Salary~., data = Hitters,scale = TRUE,ncomp = 2)
summary(pls.fit)
