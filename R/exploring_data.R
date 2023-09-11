library(tidyverse)
library(openintro)
data(smoking)
view(smoking)
?smoking

## Categorical Variables

smoking %>% count(marital_status)
smoking %>% count(smoke)
smoking %>% count(marital_status, smoke)
smoking %>%
  ggplot(aes(x = marital_status, fill = smoke)) +
  geom_bar(position = "stack")

## Challenge: Which marital category has the highest proportion of smokers?
## Hints: Try "fill" and/or "dodge" for position
smoking %>%
  ggplot(aes(x = marital_status, fill = smoke)) +
  geom_bar(position = "fill")

## Challenge: Find another pair of associated categorical variables in this
## data set and illustrate the association.

smoking %>%
  ggplot(aes(x = highest_qualification, fill = smoke)) +
  geom_bar(position = "fill")

# Age?
summary(smoking$age)
ggplot(smoking, aes(x = age)) +
  geom_boxplot()
## Challenge: Find another geometry that makes sense for this plot.
ggplot(smoking, aes(x = age)) +
  geom_histogram()
ggplot(smoking, aes(x = age)) +
  geom_density()
ggplot(smoking, aes(x = age)) +
  geom_bar()

## Is there an association between age and whether someone smokes?
ggplot(smoking, aes(x = age, color = smoke)) +
  geom_boxplot()

## Now restrict to just the smokers.
smokers <- smoking %>%
  filter(smoke == "Yes")
glimpse(smoking)
glimpse(smokers)
ggplot(smokers, aes(x = amt_weekends)) +
  geom_boxplot()
## Challenge: Q. Do men and women smokers differ in how much they smoke on 
## weekends? Make a plot and record (log sheet) a
## conclusion sentence.
ggplot(smokers, aes(x = amt_weekends, fill = gender)) +
  geom_boxplot()

ggplot(smokers, aes(x = amt_weekends, fill = marital_status)) +
  geom_boxplot()

## Warm-up challenge: Find a sensible geometry:
ggplot(smokers, aes(x = amt_weekends, y = amt_weekdays)) +
  geom_bin2d()

