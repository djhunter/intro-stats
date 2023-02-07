library(tidyverse)
library(openintro)

## duke_forest data frame

glimpse(duke_forest)
view(duke_forest)

## make a new data frame called homes
homes <- duke_forest %>% 
  select(-address, -hoa, -url)

## Make a scatterplot price vs area

ggplot(homes, aes(x = area, y = price)) +
  geom_point()

## remove influential observations
homes_small_area <- homes %>%
  filter(area < 4500)

ggplot(homes_small_area, aes(x = area, y = price)) +
  geom_point()

## Challenge: Start with homes data frame.
#1. Make a scatterplot of price vs. lot.
#2. Filter out lot sizes above 1.0. New data frame: homes_small_lot
#3. Make a new scatterplot.
#4. Which predictor (area or lot) has a stronger correlation?

# Solution
ggplot(homes, aes(x = lot, y = price)) +
  geom_point()
homes_small_lot <- homes %>%
  filter(lot < 1.0)
ggplot(homes_small_lot, aes(x = lot, y = price)) +
  geom_point()

## Make regression tables
model1 <- lm(price ~ area, data = homes)
summary(model1)
model1s <- lm(price ~ area, data = homes_small_area)
summary(model1s)

## Challenge: Repeat above for price vs. lot analysis.
# Based on R^2 values, which is a better predictor (area or lot)?

# Solution
model2 <- lm(price ~ lot, data = homes)
summary(model2)
model2s <- lm(price ~ lot, data = homes_small_lot)
summary(model2s)

## Predictions

# Using model1, predict price of a 1600 sq foot house
116652 + 159.48*1600

# Use predict method:
new_house <- data.frame(area = 1600)
predict(model1, new_house)
predict(model1s, new_house)

new_houses <- data.frame(area = c(1600, 2000), lot = c(0.25, 0.5))
predict(model1, new_houses)
predict(model1s, new_houses)
predict(model2, new_houses)
predict(model2s, new_houses)
