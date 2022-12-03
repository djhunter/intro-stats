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

mmod1 <- lm(total_pr ~ cond, data = mk)
summary(mmod1)

ggplot(mk, aes(x = total_pr, y = cond)) + geom_violin()

## Control for Wii wheels

## Challenge: Control for wheels, stock photo, and duration

