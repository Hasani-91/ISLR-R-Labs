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
lda.fit <- lda(Direction ~ Lag1+Lag2, data = Smarket, subset = train)
lda.fit # displays summary of model

# The plot() function produces plots of the linear discriminants, obtained by computing 
# −0.642 × Lag1 and −0.514 × Lag2 for each of the training observations.
# i.e. plots distribution of Up and Down group per LDA model fit.
# These coefficients in the linear combination are the values that separate the two groups the best (think of PCA)
# Example, look at: http://rstudio-pubs-static.s3.amazonaws.com/35817_2552e05f1d4e4db8ba87b334101a43da.html
plot(lda.fit) 

### The below replicates the plot(lda.fit)
c1 <- coef(lda.fit)[1]
c2 <- coef(lda.fit)[2]
x <- as.data.frame(subset(Smarket, train)$Lag1*c1 + subset(Smarket, train)$Lag2*c2)
x1 <- cbind(x, subset(Smarket, train)$Direction)
colnames(x1) <- c('LD', 'Direction')
x1Up <- subset(x1, x1$Direction == 'Up')
x1Down <- subset(x1, x1$Direction == 'Down')
par(mfrow=c(2,1)) 
hist(x1Down$LD, breaks=seq(-4.5,4.5,by=0.5), labels = TRUE, freq = FALSE, ylim = c(0,0.5), col = 'red')
hist(x1Up$LD, breaks=seq(-4.5,4.5,by=0.5), labels = TRUE, freq = FALSE, ylim = c(0,0.5), col = 'green')

########################################
lda.pred = predict(lda.fit, Smarket.2005) # make prediction based on our model
names(lda.pred)
lda.class=lda.pred$class # the predticted class of each observation
table(lda.class,Direction.2005) # view prediction vs. reality
mean(lda.class==Direction.2005) # model accuracy

# Applying a 50% threshold to the posterior probabilities allows us to recreate the predictions contained in lda.pred$class.
# i.e. if the posterior probability of a down move (the first column) is > 50% classify as down
# then sum these instances. Do the same for the up predictions
sum(lda.pred$posterior[,1] >= .5) 
sum(lda.pred$posterior[,1] < .5)

# view output probabilities and classes for prediction
lda.pred$posterior[1:20,1]
lda.class[1:20]
# see how many observations have a higher than 90% chance of being a down move
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