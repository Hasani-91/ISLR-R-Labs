# Chapter 8 Lab: Decision Trees

#####
# Fitting Classification Trees
rm(list=ls())
library(tree)
library(ISLR)
attach(Carseats)

# Turn Sales into an arbitrary classification variable
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High) # add to df

# fit a classification tree to predict if high sales (exclude Sales!)
tree.carseats = tree(High~.-Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0) # include the category names for any qualitative predictors
tree.carseats

# Need to estimate the test error rather than simply computing the training error
set.seed(2)
train = sample(1:nrow(Carseats),  200)
Carseats.test = Carseats[-train, ] # test data
High.test = High[-train] # test data output

tree.carseats = tree(High~.-Sales, Carseats, subset = train) # fit tree on training data
# predict on test data
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
t <- table(tree.pred, High.test)
sum(diag(t))/sum(t) # % of correct predictions

# prune tree using CV to determine optimal level of complexity
# Note: the argument FUN = prune.misclass in order to indicate that we want the classification error rate
# to guide the cross-validation and pruning process, rather than the default for the cv.tree() 
# function, which is deviance

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats) # view the names of the outputs
cv.carseats # view model
par(mfrow = c(1, 2))
# plot the misclassification error rate as a function of both size of tree and k (cost complexity parameter)
# note that dev refers to misclassification error here, not deviance as we specified that
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# prune the tree to obtain the nine-node tree.
# plot and use for prediction
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
t1 <- table(tree.pred, High.test)
sum(diag(t1))/sum(t1) # % of correct predictions

# increase the value of best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats = prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
t2 <- table(tree.pred, High.test)
sum(diag(t2))/sum(t2) # % of correct predictions

#####
# Fitting Regression Trees
library(MASS)

# fit regression tree to predict value and plot
set.seed(1)
train  =  sample(1:nrow(Boston),  nrow(Boston)/2) # use half data as training
tree.boston = tree(medv~., Boston, subset = train) # fit tree
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

# Use  cv.tree() function to see whether pruning tree will improve performance.
# as regression now can use deviance (default call)
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b') # plot deviance vs. tree size
prune.boston = prune.tree(tree.boston, best = 5) # prune for 5 nodes (just for example)
plot(prune.boston) # plot pruned tree
text(prune.boston, pretty = 0)

# use the unpruned tree to make predictions on the test set
# this had lowest CV error
yhat = predict(tree.boston, newdata = Boston[-train, ]) # test set predictions
table(yhat) # test set predictions
boston.test = Boston[-train, "medv"] # test set actual values
plot(yhat, boston.test) # note as we average at each node predictions are bunched
abline(0, 1)
mean((yhat - boston.test)^2) # calc MSE

#####
# Bagging and Random Forests

library(randomForest)

# Bagging

# mtry = 13 indicates that all 13 predictors should be considered
# for each split of the tree (in other words, that bagging should be done)
set.seed(1)
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston
# make prediction and plot results
# note now we get a unique value for each prediction as it's the average of each bagged prediction.
yhat.bag  =  predict(bag.boston, newdata = Boston[-train, ])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test)^2)

# try different number of variables randomly sampled as candidates at each split
# predict, plot and output variable importance
set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf  =  predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - boston.test)^2) # test MSE
importance(rf.boston)
varImpPlot(rf.boston)

#####
# Boosting
# distribution = "gaussian" since this is a regression problem; 
# if it were a binary classification problem, we would use distribution = "bernoulli".

library(gbm)
set.seed(1)
boost.boston = gbm(medv~., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston) # relative influence plot

# partial dependence plots: illustrate the marginal effect of the selected variables 
# on the response after integrating out the other variables
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
# use the boosted model to predict medv on the test set
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2) # test MSE

# we can perform boosting with a different value of the shrinkage parameter λ 
# in. Default value is 0.001, and here we take λ = 0.2.
boost.boston = gbm(medv~., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost = predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost-boston.test)^2)