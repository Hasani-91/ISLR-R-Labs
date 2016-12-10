# Chapter 6 Lab 1: Subset Selection Methods
rm(list=ls())
   
library(ISLR)
library(leaps)
library(glmnet)
library(pls)
#####
# Best Subset Selection (want to predict salary variable)

# Data clean-up
Hitters <- Hitters
dim(Hitters)
sum(is.na(Hitters$Salary)) # how many NAs we have
Hitters=na.omit(Hitters) # remove variables with NA for salary
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

# shows which features are in 
plot(regfit.full,scale="r2") # how does this know to use plot.regsubsets?
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models
# Note: there is no predict() method for regsubsets(), hence end up writing own function

set.seed(1)
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


# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
