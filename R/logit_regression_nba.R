library(tidyverse)

## Load and inspect the data
nba <- read_csv("https://math.westmont.edu/data/nba.csv")
view(nba)

ggplot(nba, aes(x = PTS)) + geom_histogram()
ggplot(nba, aes(x = PTS, color = TARGET_5Yrs)) + geom_histogram()
ggplot(nba, aes(x = PTS, fill = TARGET_5Yrs)) + geom_histogram()
ggplot(nba, aes(x = PTS, fill = TARGET_5Yrs)) + 
  geom_histogram(position = "dodge", binwidth = 3)
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_point()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot() # doesn't work

## Convert response variable to a factor
nba$TARGET_5Yrs <- factor(nba$TARGET_5Yrs)

ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot()

## Logistic regression models
bbmod1 <- glm(TARGET_5Yrs ~ PTS, family = "binomial", data = nba)
summary(bbmod1)

# Write down the logistic regression equation

# Ayo Dosunmu rookie stats
#GP GS MIN  FGM-FGA FG%  3PTM-3PA 3P%  FTm-FTA FT%  OR  DR  REB AST BLK STL PF  TO  PTS
#77 40 27.4 3.6-6.9 52.0 0.9-2.4  37.6 0.7-1.0 67.9 0.4 2.4 2.8 3.3 0.4 0.8 2.4 1.4 8.8

## Predict if Ayo will be in the league in 5 years:
predict(bbmod1, data.frame(PTS = 8.8))
exp(1.064908)/(1+exp(1.06498))
predict(bbmod1, data.frame(PTS = 8.8), type = "response")

## Challenge: Add games played  (G) as another predictor in your model. (bbmod2)
## Ayo played 77 games as a rookie and averaged 8.8 points. What
## probability does the model predict?

# Solution
bbmod2 <- glm(TARGET_5Yrs ~ PTS + G, family = "binomial", data = nba)
summary(bbmod2)
predict(bbmod2, data.frame(PTS = 8.8, G = 77), type = "response")
# 0.823

## Challenge: make a scatterplot of G vs PTS, colored by TARGET_5Yrs
## What do you notice? Where would Ayo be?
## Solution
ggplot(nba, aes(x = PTS, y = G, color = TARGET_5Yrs)) +
  geom_point() +
  geom_smooth() +
  scale_color_brewer(palette = "Set2")

## Challenge: Try making a model with more than 5 predictor 
# variables (your choice). Use the step function on your model.
# Does it eliminate any variables?
# Solution:
model3 <- glm(TARGET_5Yrs ~ PTS + G + AST + TRB + TOV, 
              family = "binomial", data = nba)
step(model3)

## Challenge: how could you display three variables in a plot,
# giving some information about 5yr success?
ggplot(nba, aes(x = PTS, y = G, size = TRB, color = TARGET_5Yrs)) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1")


