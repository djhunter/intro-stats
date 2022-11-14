## Inference for Numerical Data

library(tidyverse)
library(openintro)

data(smoking)
glimpse(smoking)
?smoking
summary(smoking)

summary(smoking$age)

ggplot(smoking, aes(x = age)) +  geom_boxplot() 
ggplot(smoking, aes(x = age)) +  geom_histogram() 
ggplot(smoking, aes(x = age)) +  geom_density() 

ggplot(smoking, aes(x = age, fill = smoke)) +  geom_boxplot() 
ggplot(smoking, aes(x = age, fill = smoke)) +  geom_histogram(position = "dodge") 
ggplot(smoking, aes(x = age, fill = smoke)) +  geom_density(alpha = 0.3) 

t.test(age ~ smoke, data = smoking)

## Let's restrict our attention to just people who smoke.

smokers <- smoking %>%
  filter(smoke == "Yes")

summary(smokers)

ggplot(smokers, aes(x = amt_weekends)) + geom_boxplot()

## Challenge: Do men and women differ in how much they smoke on weekends? Make a plot
## and do a t-test.

# solution:
ggplot(smokers, aes(x = amt_weekends, fill = gender)) +  geom_boxplot() 
ggplot(smokers, aes(x = amt_weekends, fill = gender)) +  geom_histogram(position = "dodge") 
ggplot(smokers, aes(x = amt_weekends, fill = gender)) +  geom_density(alpha = 0.3) 

t.test(amt_weekends ~ gender, data = smokers)

## Multiple means: Grouping variables with more than 2 levels

summary(smokers$marital_status)

ggplot(smokers, aes(x = amt_weekends, fill = marital_status)) + geom_boxplot()
mstat.aov <- aov(amt_weekends ~ marital_status, data = smokers)
anova(mstat.aov)

## Challenge: Can you find a factor that predicts how much someone smokes
## on weekends? Try some. Make a boxplot, and then do an 
## anova. Is there evidence of an association? If so, follow up with TukeyHSD.

## Solution: 
ggplot(smokers, aes(x = amt_weekends, fill = region)) + geom_boxplot()
region.aov <- aov(amt_weekends ~ region, data = smokers)
anova(region.aov)
TukeyHSD(region.aov)

ggplot(smokers, aes(x = amt_weekends, fill = highest_qualification)) + geom_boxplot()
edu.aov <- aov(amt_weekends ~ highest_qualification, data = smokers)
anova(edu.aov)
TukeyHSD(edu.aov)

## Paired t-test

t.test(smokers$amt_weekdays, smokers$amt_weekends, paired = TRUE)
ggplot(smokers, aes(x = amt_weekdays, y = amt_weekends)) + geom_point()

## Challenge: Do men and women separately? Same difference?



