library(tidyverse)

gss <- read_csv("http://math.westmont.edu/data/gss2.csv")

gss %>%
  ggplot(aes(x = church, fill = atoms)) +
    geom_bar(position = "fill")

?prop.test

gss %>%
  select(church, atoms) %>%
  table() %>%
  prop.test()

## Interpretation: Churchgoers are significantly less likely to know 
## that electrons are smaller than atoms. 

?chisq.test

## Challenge: Conduct a chi-square test for an appropriate choice of 
## variables in this data set. Record an interpretation.

## Solution:
gss %>%
  select(atoms, major) %>%
  table() %>%
  chisq.test()
#################

?t.test

## Challenge: Conduct a two-sample t-test for an appropriate choice of 
## variables and record an interpretation.

## Solution:
t.test(words ~ atoms, data = gss)
## People who know that electrons are smaller than atoms score significantly
## better on the vocabulary quiz (between 0.2 and 1.3 points better, with 95%
## confidence.)
################

?aov
?TukeyHSD

## Challenge: Conduct an ANOVA F-test for an appropriate choice of 
## variables, and follow up appropriately. Record an interpretation.

## Solution:
mses.aov <- aov(status ~ major, data = gss)
anova(mses.aov)
TukeyHSD(mses.aov)
#######################

?lm

## Challenge: Fit a linear regression model for an appropriate choice of 
## variables. Record an interpretation of the p-values.

## Solution:
summary(lm(income ~ educ + words + atoms, data = gss))
#######################

?glm

## Challenge: Fit a logistic regression model for an appropriate choice of 
## variables. Record an interpretation of the p-values.

## Solution:
summary(glm(factor(atoms) ~ educ + income, family = "binomial", data = gss))
#######################
