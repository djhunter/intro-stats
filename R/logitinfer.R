library(tidyverse)
library(caret)

nba <- read_csv("https://math.westmont.edu/data/nba.csv")
view(nba)

ggplot(nba, aes(x = PTS)) + geom_histogram()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_point()
ggplot(nba, aes(x = PTS, y = TARGET_5Yrs)) + geom_boxplot()
ggplot(nba, aes(x = PTS, y = factor(TARGET_5Yrs))) + geom_boxplot()

bbmod1 <- glm(TARGET_5Yrs ~ PTS, family = "binomial", data = nba)
summary(bbmod1)
bbmod2 <- glm(TARGET_5Yrs ~ 3P Made, family = "binomial", data = nba)

glimpse(nba)
bbmod2 <- glm(TARGET_5Yrs ~ `3P Made`, family = "binomial", data = nba)
summary(bbmod2)
# 3P made not significant, but control for games played (GP)?

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
