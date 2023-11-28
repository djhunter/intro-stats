library(tidyverse)
library(openintro)
library(caret)

## View the data

view(mariokart)

?mariokart

## Check for outliers

ggplot(mariokart, aes(x = total_pr)) + geom_histogram()

## What is the deal with those?

mariokart %>%
  filter(total_pr > 100) %>% 
  view()

## Remove outliers

mk <- mariokart %>%
  filter(total_pr < 100)

ggplot(mk, aes(x = total_pr)) + geom_histogram()

ggplot(mk, aes(x = total_pr, y = cond)) + geom_boxplot()
ggplot(mk, aes(x = total_pr, y = cond)) + geom_violin()

mmod1 <- lm(total_pr ~ cond, data = mk)
summary(mmod1)

## Optional note: same as t.test with equal variance!
t.test(total_pr ~ cond, data = mk, var.equal = TRUE)

## Record a table to keep track of models

## Control for Wii wheels

glimpse(mk)

ggplot(mk, aes(x = wheels, y = total_pr)) + geom_point()
ggplot(mk, aes(x = wheels, y = total_pr, color = cond)) + geom_point()

## Challenge: Control for wheels by adding it as a predictor. Call your new
## model mmod2. Compare with mmod1. What is the price difference between
## new and used versions?

## Solution:
mmod2 <- lm(total_pr ~ cond + wheels, data = mk)
summary(mmod2)

# The average price difference is only $5, when wheels are taken into account.

## Cross Validation

set.seed(3456)
tc <- trainControl(method = "cv", number = 5)
train(total_pr ~ cond, data = mk, method = "lm", trControl = tc)
train(total_pr ~ cond + wheels, data = mk, method = "lm", trControl = tc)

## Challenge: Find a model for predicting total_pr with a lower RMSE.

## Solution
train(total_pr ~ cond + wheels + start_pr, data = mk, method = "lm", trControl = tc)
train(total_pr ~ cond + wheels + seller_rate, data = mk, method = "lm", trControl = tc)
train(total_pr ~ cond + wheels + start_pr + seller_rate, data = mk, method = "lm", trControl = tc)
train(total_pr ~ cond + wheels + start_pr + stock_photo, data = mk, method = "lm", trControl = tc)

## Challenge: For the best model you found, 
## 1. Are all the predictors significant? 
## 2. Record the adjusted R^2.

## Solution
mmod3 <- lm(total_pr ~ cond + wheels + start_pr + seller_rate, data = mk)
summary(mmod3)

## Challenge: make some plots illustrating the important predictors that you found.

## Solution:
ggplot(mk, aes(x = start_pr, y = total_pr)) + geom_point() + geom_smooth()
ggplot(mk, aes(y = stock_photo, x = total_pr)) + geom_violin() + geom_boxplot()

