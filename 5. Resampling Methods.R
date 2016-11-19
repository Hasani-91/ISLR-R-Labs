# Chaper 5 Lab: Cross-Validation and the Bootstrap
library(ISLR)
library(boot)
attach(Auto) # data has 392 observations
#############################################
### The Validation Set Approach (i.e. simply split data into half, one training and one validation set)

set.seed(1) # allows replication of results
train = sample(392,196) # select a random subset of 196 observations out of the original 392 observations
plot(mpg~horsepower, data=Auto) # plot data to view relationship

# 1. Linear model
lm.fit=lm(mpg~horsepower, data=Auto, subset=train) # fit linear model to subset
abline(lm.fit, col = 'red') # add linear regression line
mean((mpg-predict(lm.fit,Auto))[-train]^2) # makes prediction on full data & calculates MSE of validation set
par(mfrow = c(2, 2))
plot(lm.fit) # plot model output
# 2. try polynomial order 2, calculate MSE and plot model
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
par(mfrow = c(2, 2))
plot(lm.fit2)
# 3. try polynomial order 3 (cubic), calculate MSE and plot model
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
par(mfrow = c(2, 2))
plot(lm.fit3)
# setting a different seed will change the results (as we choose a different training set)
# run same 3 models again and check MSE
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#############################################
### Leave-One-Out Cross-Validation (LOOCV)

glm.fit=glm(mpg~horsepower,data=Auto) # fits linear model same as lm()
coef(glm.fit)
cv.err = cv.glm(Auto,glm.fit) # default is LOOCV if no k specified
cv.err$delta # The LOOCV estimate for the test MSE is the average of the n test error estimates

cv.error=rep(0,5) # blank vector for results

# fit 5 models of increasing complexity and calculate LOOCV for each
for (i in 1:5){
    glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
    cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
par(mfrow=c(1,1))
plot(cv.error, type = 'b') # plot LOOCV error across model complexity

#############################################
### k-Fold Cross-Validation

set.seed(17)
cv.error.10 = rep(0,10)
# fit polynomials up to degree 10 and then run 10 fold CV on each and record error
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit, K=10)$delta[1]
}
plot(cv.error.10, type = 'b')
#############################################
### The Bootstrap
# note: Portfolio is a data set in the ISLR package, 100 returns for two assets (X and Y)

# create function to calculate alpha as the min variance portfolio weight in X
alpha.fn = function(data,index){
    X=data$X[index]
    Y=data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100) # test function using initial data set
# test one random trial
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
# now test 1000 using bootstrap
b <- boot(Portfolio, alpha.fn, R = 1000)
b # gives mean estimate for the statistic and std error
plot(b) # histogram and QQ plot of bootstrap

### Estimating the accuracy of a Linear Regression Model
# we will use bootstrapping to compare the bootstrapped coeffs to the ones from a single lm()
# fit on the whole data (and look at accuracy of SEs)

boot.fn = function(data,index) return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto,1:392) # linear model of whole data set

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T)) # example of one set of coeffs with a random sample
boot.fn(Auto,sample(392,392,replace=T)) # example of another set of coeffs with a random sample
boot(Auto, boot.fn, 1000) # now estimate coeffs 1000 times

summary(lm(mpg~horsepower,data=Auto))$coef # summary of linear model for comparison

# fit quadratic model 
boot.fn2=function(data,index) coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto, boot.fn2, 1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

