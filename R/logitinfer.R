library(tidyverse)
library(caret)

## Load and inspect the data
nba <- read_csv("https://math.westmont.edu/data/nba.csv")
view(nba)

ggplot(nba, aes(x = PTS)) + geom_histogram()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_point()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot()

## Logistic regression models
bbmod1 <- glm(TARGET_5Yrs ~ PTS, family = "binomial", data = nba)
summary(bbmod1)

bbmod2 <- glm(TARGET_5Yrs ~ 3P%, family = "binomial", data = nba) # error

glimpse(nba)
bbmod2 <- glm(TARGET_5Yrs ~ `3P%`, family = "binomial", data = nba)
summary(bbmod2)

# Challenge: Improve bbmod2 by controlling for 3 point attempts (3PA). Do
# the p-values change? Can you explain why?

## Solution:
bbmod3 <- glm(TARGET_5Yrs ~ `3P%` + `3PA`, family = "binomial", data = nba)
summary(bbmod3)

## Correlated predictors?
cor(nba[,7:31])
round(cor(nba[,7:31]), 1)

## Challenge: Why are some of the correlations NA? 

## Cross Validation

tc <- trainControl(method = "cv", number = 10)
train(TARGET_5Yrs ~ PTS,  data = nba, 
      method = "glm", family = "binomial", trControl = tc)
nba <- nba %>%
  mutate(T5Y = factor(ifelse(TARGET_5Yrs, "Yes", "No")))
train(T5Y ~ PTS,  data = nba, 
      method = "glm", family = "binomial", trControl = tc)
bbcv1 <- train(T5Y ~ PTS, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv1)

bbcv2 <- train(T5Y ~ `3P%` + `3PA`, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
bbcv2 <- train(T5Y ~ `3P` + `3PA`, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv2)
bbcv2

## Challenge: Find a model with a better prediction accuracy.
## For the best model you find, generate a logistic regression 
## table. Which predictors are most important? 

## Solution?
tc <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
bbcv3 <- train(T5Y ~ G + PTS + ORB + AST, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv3)

bbmod3 <- glm(TARGET_5Yrs ~ G + PTS + ORB + AST, family = "binomial", data = nba)
summary(bbmod3)

## Search for best model?
nba_complete <- nba %>%
  select(-TARGET_5Yrs, -PlayerID, -Player, -Pos, -Age, -Tm, -Year, -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`, -GS)
fullmod <- glm(T5Y ~ ., family = "binomial", data = nba_complete)
summary(fullmod)
step(fullmod)

## Challenge: Use the results of step, make a regression model using glm
## and test its accuracy using train. Does it beat the other models we found?

bestmod <- glm(T5Y ~ G + MP + PTS + FGA + DRB + AST + BLK + TOV + PF, 
               family = "binomial", data = nba_complete)
summary(bestmod)
summary(fullmod)

bbcv4 <- train(T5Y ~ G + MP + PTS + FGA + DRB + AST + BLK + TOV + PF, 
               data = nba, method = "glm", family = "binomial", trControl = tc)
confusionMatrix(bbcv4)
