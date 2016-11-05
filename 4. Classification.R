# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN
rm(list = ls())
# The Stock Market Data
library(ISLR)
attach(Smarket)
summary(Smarket)
pairs(Smarket, col = Smarket$Direction)
corrSmarket <- cor(Smarket[,-9]) # correlation of numeric variables
dimSmarket <- dim(Smarket)
plot(Volume)

###########################
### Logistic Regression ###
###########################

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef # all coeffs
summary(glm.fit)$coef[,4] # p values

# vector of probabilities of market being Up or Down based on features supplied to model
# for a default binomial model the default predictions are of log-odds (probabilities on logit scale)
# so type = "response" gives the predicted probabilities
glm.probs = predict(glm.fit, type = "response") 

contrasts(Direction) # shows how qualitative variable Direction is coded in model
glm.pred = rep("Down",1250)
glm.pred[glm.probs > .5] = "Up" # create vector of predictions based on model prob greater than a threshold

table(glm.pred, Direction) # confusion matrix
mean(glm.pred == Direction) # % of correct predictions

# subset training data and test data 
train = (Year<2005) # split data into training data for year before 2005
Smarket.2005 = Smarket[!train,] # year 2005 is test data
Direction.2005 = Direction[!train] # actual direction of 2005 data

# fit new glm model based on just training data
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response") # probability vector for test data based on training data

# look at confusion matrix
glm.pred = rep("Down",252)
glm.pred[glm.probs>.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005) # number of correct predictions
mean(glm.pred!=Direction.2005) # incorrect predictions

# fit another glm model just on lag1 and lag2 features
glm.fit = glm(Direction~Lag1+Lag2,data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fit,Smarket.2005, type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

# gives probability for two days of an Up direction based on Lag1 and Lag2 having values supplied (and using model supplied)
predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")

####################################
### Linear Discriminant Analysis ###
####################################

library(MASS)

# fit LDA model using just lag1 and lag2 variables and training data (pre 2005)
lda.fit = lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)

lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# Quadratic Discriminant Analysis

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

# An Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
table(knn.pred,test.Y)
9/(68+9)
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15
glm.fit=glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs=predict(glm.fit,Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>.25]="Yes"
table(glm.pred,test.Y)
11/(22+11)