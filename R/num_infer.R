## Inference for Numerical Data

library(tidyverse)
library(openintro)

data(yrbss)
glimpse(yrbss)
?yrbss

summary(yrbss$weight)
summary(yrbss$physically_active_7d)

active_weight <- yrbss %>%
  select(physically_active_7d, weight) %>%
  drop_na() %>%
  mutate(active = if_else(physically_active_7d > 4, "yes", "no"))

table(active_weight$active)

# Is weight associated to activity level?

ggplot(active_weight, aes(x = active, y = weight)) + geom_boxplot()
ggplot(active_weight, aes(x = active, y = weight)) + geom_violin()

t.test(weight ~ active, data = active_weight)

table(yrbss$strength_training_7d)

## Challenge: do youth who do strength training more than 3 days per week weigh
## more than those who don't? 

## Solution:
strength_weight <- yrbss %>%
  select(strength_training_7d, weight) %>%
  drop_na() %>%
  mutate(train = if_else(strength_training_7d > 3, "yes", "no"))

ggplot(strength_weight, aes(x = train, y = weight)) + geom_violin()
t.test(weight ~ train, data = strength_weight)
