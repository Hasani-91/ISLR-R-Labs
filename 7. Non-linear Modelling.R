# Chapter 7 Lab: Non-linear Modeling
rm(list = ls())
library(ISLR)
attach(Wage)

#####
# Polynomial Regression and Step Functions

# the poly function returns a matrix whose columns are a basis of orthogonal polynomials, 
# which essentially means that each column is a linear combination of the variables age, age^2, age^3 and age^4
fit = lm(wage ~ poly(age, 4), data = Wage) # fit 4th degree polynomial in age
coef(summary(fit))

# Now 3 ways of doing a normal polynomial fit with one predictor

# 1. use raw = T to fit model directly, e.g. not orthogonal polynomials (fitted model same, coeffs diff)
fit2 = lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))
# 2. could also fit model it explicitly  (also not orthogonal polynomials, same as above)
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)
# 3. same but using cbind() is a tad more compact
fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
coef(fit2b)

# create grid of values of age for which to predict
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])

# predict using 4th degree poly fit for ages in grid
preds = predict(fit, newdata = list(age = age.grid), se = TRUE) # include SEs
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit) # matrix of SE
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0)) # control margins & number of plots on screen
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue") # plot fitted function values
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3) # plot SEs

# fitted values from model not using orthogonal polynomials (& check values are same)
preds2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

# Now create a set of (nested) models of increasing complexity and analyse using anova which
# performs analysis of variance using an F-test. In order to use the anova() function, 
# the models must be nested as anova compares them sequentially
fit.1 = lm(wage ~ age, data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # so output shows cubic model better than simpler but more complexity not needed

# given poly function creates orthogonal polynomials could have used to view p values
coef(summary(fit.5))
(-11.983)^2 # square of the t-statistics are equal to the F-statistics from the anova() function

# compare models with other terms in
fit.1 = lm(wage ~ education + age, data = Wage)
fit.2 = lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 = lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

# Next we consider the task of predicting whether an individual earns more than $250k per year
# use glm to fit a polynomial logistic regression model

fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds = predict(fit, newdata = list(age = age.grid), se = T) # predict on wage grid
 
# Note in glm default prediction type is type = "link", need to transform to probabilities
# could do directly by using type = "response" option in the predict() function.
pfit = exp(preds$fit)/(1 + exp(preds$fit)) # transform predicted values to probabilities
se.bands.logit  =  cbind(preds$fit + 2*preds$se.fit,  preds$fit-2*preds$se.fit) # SE bounds
se.bands  =  exp(se.bands.logit)/(1 + exp(se.bands.logit)) # transform SE bounds

preds = predict(fit, newdata = list(age = age.grid), type = "response", se = T) # demonstrate direct method

# plot results with two quirks
# 1. rescale response vector by 5 as y scale is cut to 0.2
# 2. use jitter() to add a little noise to the age values a bit so that observations with same age  do not cover each other up
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age),  I((wage > 250)/5), cex = .5, pch = "|", col = "darkgrey") 
lines(age.grid, pfit, lwd = 2,  col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3) # add SE bounds

# fits a linear step function to data
table(cut(age, 4)) # cuts age vector into 4 buckets (and locates breakpoints) of equal length (not density)
fit = lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

#####
# Splines
library(splines)

# fit regression splines using basis functions, default is cubic, here we pre-specify knots
# plot results
fit = lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = T) # make predictions
plot(age, wage, col = "gray") # plot full data
lines(age.grid, pred$fit, lwd = 2) # add our fitted function values
lines(age.grid, pred$fit + 2*pred$se, lty = "dashed") # add SE lines
lines(age.grid, pred$fit-2*pred$se, lty = "dashed") # add SE lines

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots") # let R find where to put knots data by quantiles

# fit a natural spline with four degrees of freedom and plot results
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid,  pred2$fit, col = "red", lwd = 2)

# fit a smoothing spline and plot results
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16) # fit one using arbitrary df
fit2 = smooth.spline(age, wage, cv = TRUE) # use CV to determine df
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# perform local regression
# the larger the span, the smoother the fit
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = .2, data = Wage) # span of 0.2: each neighborhood consists of 20% of observations
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# GAMs (Generalised Additive Models)
library(gam)
library(akima)

# can use lm() function initially
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage) # fit natural splines
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage) # fit smoothing splines

# plot fitted models
# Note: generic plot() function recognizes that gam2 is an object of class gam
# & invokes the appropriate plot.gam() method. Can use plot.gam() on gam1 even though lm model
par(mfrow = c(1, 3))
plot(gam.m3,  se = TRUE, col = "blue") 
plot.gam(gam1,  se = TRUE,  col = "red")

# Year looks almost linear so perform anova to see which model is best
# m1: a GAM excluding year
# m2: a GAM with year as linear function
# m3 (from above): a GAM with year as smoothing function
gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F") # perform and view anova (m2 best model)

summary(gam.m3) # view summary of a gam model
preds = predict(gam.m2, newdata = Wage) # predict using m2 model

# in gam package we can do local regression fits as building blocks using the lo() function
# here we used local regression on age term
gam.lo = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.lo,  se = TRUE,  col = "green")

# can use lo() to create intersections before fitting with gam
# the first term is an interaction between year and age, fit by a local regression surface
gam.lo.i = gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)
plot(gam.lo.i) # plot 2d surface

# Logistic regression GAM to predict high earner or not
gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

table(education, I(wage > 250)) # no high earners in '< HS Grad' category within education variable

# fit gam excluding this set
gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")