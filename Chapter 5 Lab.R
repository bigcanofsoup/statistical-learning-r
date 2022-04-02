
# Lab: Cross-Validation and the Bootstrap


## The Validation Set Approach

###
library(ISLR2)
set.seed(1)
train <- sample(392, 196) #split it into roughly two equal parts: sample() = do it randomly
###
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
###
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)  #mean square: mean(y-yhat^2), [-train] is the training data
###
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, #fit any degree of polynomial into horsepower; adding a quadratic term  has reduced the error
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, #adding the extra term does not help much in reducing error
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
###
set.seed(2)  #different sample
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, #25.72
              subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) #20.43
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, 
              subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) #20.38

#seems that going from order 1 to order 2 is on average better performing than going from order 2 to order 3

## Leave-One-Out Cross-Validation

###
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
###
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
###
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto) #glm goes along with a function called cv.glm (used to perform cross validation)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
###
cv.error <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto) #no families: do not need binomials for logistic, just SLR
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
## $k$-Fold Cross-Validation
###
set.seed(17)
cv.error.10 <- rep(0, 10) #setting glm as a function of x
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1] #10 fold ism uch faster due to fewer tests involved
  #error: still between one the same as they were for leave one out CV.
}
cv.error.10

## The Bootstrap


### Estimating the Accuracy of a Statistic of Interest

Portfolio <- data.frame(Portfolio) #hidden, use this to create a name
###
alpha.fn <- function(data, index) { #see MSE as a function of flexibility, curly brace to begin the function, brace at the end to end the function
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
###
alpha.fn(Portfolio, 1:100)
###
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))
###
boot(Portfolio, alpha.fn, R = 1000)    #final step: taking average of all of the alphas

### Estimating the Accuracy of a Linear Regression Model

###
boot.fn <- function(data, index)
  coef(lm(mpg ~ horsepower, data = data, subset = index))
boot.fn(Auto, 1:392)
###
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T)) #repace = t: with repalcement, creating a bootstrap set of indices fort this data set

#Is only done one time
boot.fn(Auto, sample(392, 392, replace = T))
###
boot(Auto, boot.fn, 1000) #to repeat the process how many times, use boot()
###
summary(lm(mpg ~ horsepower, data = Auto))$coef
###
boot.fn <- function(data, index)
  coef(
    lm(mpg ~ horsepower + I(horsepower^2), 
       data = data, subset = index)
  )
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(
  lm(mpg ~ horsepower + I(horsepower^2), data = Auto)
)$coef
###

