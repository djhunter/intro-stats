library(tidyverse)

## Load and inspect the data
nba <- read_csv("https://math.westmont.edu/data/nba.csv")
view(nba)

ggplot(nba, aes(x = PTS)) + geom_histogram()
ggplot(nba, aes(x = PTS, color = TARGET_5Yrs)) + geom_histogram()
ggplot(nba, aes(x = PTS, fill = TARGET_5Yrs)) + geom_histogram()
ggplot(nba, aes(x = PTS, fill = TARGET_5Yrs)) + 
  geom_histogram(position = "dodge", binwidth = 100)
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_point()

## Convert response variable to a factor: Not needed for logi
# nba$TARGET_5Yrs <- factor(nba$TARGET_5Yrs)

## Logistic regression models
bbmod1 <- glm(TARGET_5Yrs ~ PTS, family = "binomial", data = nba)
summary(bbmod1)

# Write down the logistic regression equation

# Ayo Dosunmu rookie stats
#GP GS MIN  FGM-FGA FG%  3PTM-3PA 3P%  FTm-FTA FT%  OR  DR  REB AST BLK STL PF  TO  PTS
#77 40 27.4 3.6-6.9 52.0 0.9-2.4  37.6 0.7-1.0 67.9 0.4 2.4 2.8 3.3 0.4 0.8 2.4 1.4 8.8

## Ayo Dosunmu scored 678 points in his rookie year.
## Predict if Ayo will be in the league in 5 years:
predict(bbmod1, data.frame(PTS = 678))
exp(2.242346)/(1+exp(2.242346))
predict(bbmod1, data.frame(PTS = 678), type = "response")
# 0.90

## Challenge: Add total rebounds (TRB) as another predictor in your model. (bbmod2)
## Ayo scored 678 points as a rookie and had 216 rebounds. What
## probability does the model predict? Compare to bbmod1. Does it make sense?

# Solution
bbmod2 <- glm(TARGET_5Yrs ~ PTS + TRB, family = "binomial", data = nba)
summary(bbmod2)
predict(bbmod2, data.frame(PTS = 678, TRB = 216), type = "response")
# 0.86

## Challenge: make a scatterplot of TRB vs PTS, colored by TARGET_5Yrs
## What do you notice? Where would Ayo be?
## Solution
ggplot(nba, aes(x = PTS, y = TRB, color = TARGET_5Yrs)) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

## Challenge: Who is the highest-scoring rookie in this data set? (Use filter)
nba %>% filter(PTS > 2000) %>% glimpse()

## Challenge: Try making a model with more than 5 predictor 
# variables (your choice). Use the step function on your model.
# Does it eliminate any variables?
# Solution:
model3 <- glm(TARGET_5Yrs ~ PTS + G + AST + TRB + TOV + ORB, 
              family = "binomial", data = nba)
step(model3)

## Challenge: how could you display three variables in a plot,
# giving some information about 5yr success?
ggplot(nba, aes(x = PTS, y = TRB, size = AST, color = TARGET_5Yrs)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1")

## Challenge: When did they move the 3-point line in? Make a scatterplot
## to help you decide.


