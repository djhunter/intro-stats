library(tidyverse)
library(openintro)

# Look at the duke_forest data frame
duke_forest
glimpse(duke_forest)
view(duke_forest)

# Make a new data frame: 
#   Get rid of address, hoa, url columns

homes <- duke_forest %>%
  select(-address, -hoa, -url)
homes
view(homes)

# predicting price

## Scatterplot of price vs. area

ggplot(homes, aes(x = area, y = price)) +
  geom_point()

## Problem: We have some very influential observations
# with areas above 4500. Let's remove them

homes_small_area <- homes %>%
  filter(area < 4500)

ggplot(homes_small_area, aes(x = area, y = price)) + 
  geom_point()

## Challenge: Use homes:
# 1. Make a scatterplot of price vs lot.
# 2. Filter out lot sizes above 1.0. Call the new data frame homes_small_lot.
# 3. Make a scatterplot of price vs lot in your new data frame.
# 4. Which do you think has a stronger correlation (lot or area)?

### solution
ggplot(homes, aes(x = lot, y = price)) +
  geom_point()
homes_small_lot <- homes %>%
  filter(lot < 1.0)
ggplot(homes_small_lot, aes(x = lot, y = price)) +
  geom_point()

## Which is the better predictor?

model1 <- lm(price ~ area, data = homes)
summary(model1)

model1s <- lm(price ~ area, data = homes_small_area)
summary(model1s)

# Write down the regression equation, interpret regression slope.

## Challenge: make a model predicting price from lot. (model2 and model2s, 
# using homes and homes_small_lot, respectively. ) Based on the R^2 values,
# which predictor (area or lot) is more strongly associated with house price?

### solution
model2 <- lm(price ~ lot, data = homes)
summary(model2)

model2s <- lm(price ~ lot, data = homes_small_lot)
summary(model2s)

## Add the regression lines to the plots
ggplot(homes, aes(x = area, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

## Challenge: 
### 1. Add regression line to price vs lot scatterplot.
### 2. Modify 1 aesthetics to color by the cooling variable.
### solution
ggplot(homes, aes(x = lot, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(homes, aes(x = lot, y = price, color = cooling)) +
  geom_point() +
  geom_smooth(method = "lm")


## use the models to make a prediction
new_house <- data.frame(area = 1600, lot = 0.25)
predict(model1, new_house)
predict(model1s, new_house)
predict(model2, new_house)
predict(model2s, new_house)

## Categorical response variable (OPTIONAL)

#   Add a variable anypark: 0 if no spaces, 1 otherwise

homes$parking
unique(homes$parking)

homespark <- homes %>%
  mutate(anypark = factor(ifelse(parking == "0 spaces", 0, 1))) 
view(homespark)

## Predict anypark from lot size

ggplot(homespark, aes(x = lot, y = anypark)) +
  geom_boxplot()

