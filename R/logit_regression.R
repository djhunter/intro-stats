library(tidyverse)
library(openintro)

# Look at the duke_forest data frame
duke_forest
view(duke_forest)

# Make a new data frame: 
#   Get rid of address, hoa, url columns
#   Add a factor column: 
#     New variable anypark: 0 if no spaces, 1 otherwise

homes <- duke_forest %>%
  select(-address, -hoa, -url) %>%
  mutate(anypark = factor(ifelse(parking == "0 spaces", 0, 1))) 
glimpse(homes)
view(homes)

# Logistic regression

## Predict anypark from lot size

ggplot(homes, aes(x = lot, y = anypark)) +
  geom_boxplot()

model1 <- glm(anypark ~ lot, family = "binomial", data = homes)
summary(model1)

## predicted logit for 0.25 acre lot
-0.3922 + 1.1686*0.25
## predicted probability for 0.25 acre lot
exp(-0.3922 + 1.1686*0.25)/(1 + exp(-0.3922 + 1.1686*0.25))

## Challenge: 
## 1. Compute the predicted logit for 0.5 acre lot.
## 2. Compute the predicted probability for 0.5 acre lot.

-0.3922 + 1.1686*0.5
exp(-0.3922 + 1.1686*0.5)/(1 + exp(-0.3922 + 1.1686*0.5))

## Check answers using the predict function.

# get predicted logit
predict(model1, data.frame(lot = c(0.25, 0.5)))

# get predicted probabilities
predict(model1, data.frame(lot = c(0.25, 0.5)), type = "response")

## Challenge: 
## 1. Create a logistic regression model to predict anypark from 
##    price, lot, area, bath, bed (model2).
## 2. Try using step(model2) to eliminate variables. 
##    What happens? Why did it happen?

model2 <- glm(anypark ~ price + lot + area + bath + bed, family = "binomial", data = homes)
summary(model2)
step(model2)

# Drop rows containing missing values

homes_complete <- drop_na(homes)
model3 <- glm(anypark ~ price + lot + area + bath + bed, family = "binomial", data = homes_complete)
summary(model3)
step(model3)

ggplot(homes_complete, aes(x = price, y = anypark)) +
  geom_boxplot()

## Challenge: 

# 1. Predict anypark from price (model4). Can use homes because we are not using lot.
# 2. Compare predicted probabilites for 400K vs 600K home.

model4 <- glm(anypark ~ price, family = "binomial", data = homes)
summary(model4)

## predicted probability for 400K home
exp(-0.4721 + 0.000001373*400000)/(1 + exp(-0.4721 + 0.000001373*400000))

## predicted probability for 600K home
exp(-0.4721 + 0.000001373*600000)/(1 + exp(-0.4721 + 0.000001373*600000))

predict(model4, data.frame(price = c(400000, 600000)))
predict(model4, data.frame(price = c(400000, 600000)), type = "response")
