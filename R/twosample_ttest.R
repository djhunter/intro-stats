library(tidyverse)
library(openintro)

## By hand: T = 3.8 from group exercise
## df = 113
## P-value:
pt(-3.8, df = 113) * 2
(1-pt(3.8, df = 113)) * 2

## The easy way
data("births14")
births14 %>%
  group_by(habit) %>%
  summarize(xbar = mean(weight))

## Two sample t-test
t.test(weight ~ habit, data = births14)

## side by side box plots:
births14 %>%
  filter(!is.na(habit)) %>%
  ggplot(aes(x = weight, fill = habit)) +
  geom_boxplot() 

gss <- read_csv("http://math.westmont.edu/data/gss.csv")

## Challenge: P-hack. (not good)
## Find a significant association between a two-level categorical
## explanatory variable and a numerical response variable. Do a t-test,
## Record p-value, CI, interpretation. Make a side by side box plot.

t.test(income ~ party, data = gss)
gss %>%
  filter(party != "other") %>%
  t.test(income ~ party, data = .)

gss %>%
  filter(party != "other") %>%
  ggplot(aes(x = income, fill = party)) +
  geom_boxplot()


