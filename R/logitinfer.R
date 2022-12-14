library(tidyverse)
library(caret)

## Load and inspect the data
nba <- read_csv("https://math.westmont.edu/data/nba.csv")
view(nba)

ggplot(nba, aes(x = PTS)) + geom_histogram()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_point()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot() # doesn't work

## Convert response variable to a factor
nba$TARGET_5Yrs <- factor(nba$TARGET_5Yrs)

ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot()

## Logistic regression models
bbmod1 <- glm(TARGET_5Yrs ~ PTS, family = "binomial", data = nba)
summary(bbmod1)
bbmod2 <- glm(TARGET_5Yrs ~ 3P Made, family = "binomial", data = nba) # error

glimpse(nba)
bbmod2 <- glm(TARGET_5Yrs ~ `3P Made`, family = "binomial", data = nba)
summary(bbmod2)

# Challenge: Improve bbmod2 by controlling for 3 point attempts (3PA). Do
# the p-values change? Can you explain why?

## Solution:
bbmod3 <- glm(TARGET_5Yrs ~ `3P Made` + `3PA`, family = "binomial", data = nba)
summary(bbmod3)

## Correlated predictors?
cor(nba[,2:19])
round(cor(nba[,2:19]), 2)

ggplot(nba, aes(x = `3PA`, y = `3P Made`)) + geom_point()
ggplot(nba, aes(x = `3P%`)) + geom_histogram()

## Cross Validation

tc <- trainControl(method = "cv", number = 10)
bbcv1 <- train(TARGET_5Yrs ~ PTS, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv1)
bbcv2 <- train(TARGET_5Yrs ~ `3P Made` + `3PA`, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv2)
bbcv2

## Challenge: Find a model with a better prediction accuracy.
## For the best model you find, generate a logistic regression 
## table. Which predictors are most important? 

## Solution?
bbcv3 <- train(TARGET_5Yrs ~ GP + PTS + OREB + AST, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv3)

bbmod3 <- glm(TARGET_5Yrs ~ GP + PTS + OREB + AST, family = "binomial", data = nba)
summary(bbmod3)

## Search for best model?
nba_complete <- nba %>%
  drop_na() %>%
  select(-Name)
fullmod <- glm(TARGET_5Yrs ~ ., family = "binomial", data = nba_complete)
summary(fullmod)
step(fullmod)
bestmod <- glm(TARGET_5Yrs ~ GP + MIN + FGA + `FG%` + `3P Made` + 
                 `3PA` + FTM + `FT%` + OREB + AST + BLK + TOV, 
               family = "binomial", data = nba_complete)
summary(bestmod)
summary(fullmod)

nba_nona <- nba %>%
  select(-Name, -`3P%`)
fullmod <- glm(TARGET_5Yrs ~ ., family = "binomial", data = nba_nona)
summary(fullmod)
step(fullmod)

bbcv4 <- train(TARGET_5Yrs ~ GP + `FG%` + `3P Made` + `3PA` + 
                 `FT%` + DREB + REB + AST + BLK, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv4)
