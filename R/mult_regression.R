library(tidyverse)
library(openintro)

# Make a new data frame: 
#   Get rid of address, hoa, url columns

homes <- duke_forest %>%
  select(-address, -hoa, -url)
homes
view(homes)

## Last time: Single variable models
model1 <- lm(price ~ area, data = homes)
summary(model1)

model2 <- lm(price ~ lot, data = homes)
summary(model2)

## Multiple regression model

model3 <- lm(price ~ area + lot, data = homes)
summary(model3)

## use the model to make a prediction
predict(model3, data.frame(area = 1600, lot = 0.25))

## Challenge: multiple regression model
## Make a new model (model4) that predicts price from 
## area, lot, bed, bath, year_built
## Is this an improvement over model 3?

### Solution
model4 <- lm(price ~ area + lot + bed + bath + year_built, data = homes)
summary(model4)

## eliminate some variables
step(model4)

## Challenge: make a new model (model5) using the step results. 
##    1. Compare adjusted R^2 to model4.
##    2. Predict the price of a house with 2000 sq feet area, 
##       lot size 0.5, 3 baths, built in 1950.

### Solution
model5 <- lm(price ~ area + lot + bath + year_built, data = homes)
summary(model5)
summary(model4)
predict(model5, data.frame(area = 2000, lot = 0.5, bath = 3, year_built = 1950))
## 432615.3

## Categorical response variable 

#   Add a variable anypark: 0 if no spaces, 1 otherwise

homes$parking
unique(homes$parking)

homespark <- homes %>%
  mutate(anypark = factor(ifelse(parking == "0 spaces", 0, 1))) 
view(homespark)

## Challenge: Add anypark to model 5. Does the model improve?
### Solution
model6 <- lm(price ~ area + lot + bath + year_built + anypark, data = homespark)
summary(model6)
