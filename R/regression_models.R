library(tidyverse)
library(openintro)

# look at the duke_forest data frame
duke_forest
glimpse(duke_forest)
view(duke_forest)

duke_forest$parking
unique(duke_forest$parking)

# make a new data frame: 
#   Add a variable anypark: 0 if no spaces, 1 otherwise
#   Get rid of address, hoa, url columns

homes <- duke_forest %>%
  mutate(anypark = ifelse(parking == "0 spaces", 0, 1)) %>%
  select(-address, -hoa, -url)
homes
view(homes)

# predicting price

## Scatterplot of price vs. area

ggplot(homes, aes(x = area, y = price)) +
  geom_point()

## Challenge: make a scatterplot of price vs lot

### solution
ggplot(homes, aes(x = lot, y = price)) +
  geom_point()

## Which is the better predictor?

model1 <- lm(price ~ area, data = homes)
summary(model1)

## Challenge: make a model predicting price from lot

### solution
model2 <- lm(price ~ lot, data = homes)
summary(model2)

## Add the regression lines to the plots
ggplot(homes, aes(x = area, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

## Challenge: add regression line to price vs lot scatterplot
ggplot(homes, aes(x = lot, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

## OPTIONAL: side-by-side plots: install cowplot (use packages tab)
## load cowplot library
library(cowplot)
plot1 <- ggplot(homes, aes(x = area, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")
plot2 <- ggplot(homes, aes(x = lot, y = price)) +
  geom_point() +
  geom_smooth(method = "lm")
plot_grid(plot1, plot2)

## Multiple regression model

model3 <- lm(price ~ area + lot, data = homes)
summary(model3)

## use the model to make a prediction
predict(model3, data.frame(area = 1600, lot = 0.25))

## Challenge: multiple regression model
## Make a new model (model4) that predicts price from 
## area, lot, bed, bath, year_built, and anypark
## Is this an improvement over model 3?

### Solution
model4 <- lm(price ~ area + lot + bed + bath + year_built + anypark, data = homes)
summary(model4)

## eliminate some variables
step(model4)

## Challenge: make a new model (model5) using the step results. 
##    1. Compare adjusted R^2 to model4
##    2. Predict the price of a house with 2000 sq feet area, 
##       lot size 0.5, 3 baths, built in 1950.

### Solution
model5 <- lm(price ~ area + lot + bath + year_built, data = homes)
summary(model5)
summary(model4)
predict(model5, data.frame(area = 2000, lot = 0.5, bath = 3, year_built = 1950))
## 432615.3

## Categorical response variable
## Predict anypark from lot size

ggplot(homes, aes(x = lot, y = anypark)) +
  geom_boxplot()

glimpse(homes)
homes$anypark <- factor(homes$anypark)
glimpse(homes)

ggplot(homes, aes(x = lot, y = anypark)) +
  geom_boxplot()

model6 <- glm(anypark ~ lot, family = "binomial", data = homes)
summary(model6)

## predicted logit for 0.25 acre lot
-0.3922 + 1.1686*0.25
## predicted probability for 0.25 acre lot
exp(-0.3922 + 1.1686*0.25)/(1 + exp(-0.3922 + 1.1686*0.25))

## predicted logit for 0.5 acre lot
-0.3922 + 1.1686*0.5
## predicted probability for 0.5 acre lot
exp(-0.3922 + 1.1686*0.5)/(1 + exp(-0.3922 + 1.1686*0.5))

# get predicted logit
predict(model6, data.frame(lot = c(0.25, 0.5)))
# get predicted probabilities
predict(model6, data.frame(lot = c(0.25, 0.5)), type = "response")

## Challenge: predict anypark from price, lot, area, bath, bed (model7)
## try using step to eliminate variables. What happens? Why did it happen?

model7 <- glm(anypark ~ price + lot + area + bath + bed, family = "binomial", data = homes)
summary(model7)
step(model7)

# Drop rows containing missing values

homes_complete <- drop_na(homes)
model8 <- glm(anypark ~ price + lot + area + bath + bed, family = "binomial", data = homes_complete)
summary(model8)
step(model8)

ggplot(homes, aes(x = price, y = anypark)) +
  geom_boxplot()

## Challenge: 

# 1. predict anypark from price (model9)
# 2. compare predicted probabilites for 400K vs 600K home 

model9 <- glm(anypark ~ price, family = "binomial", data = homes)
summary(model9)

## predicted probability for 400K home
exp(-0.4721 + 0.000001373*400000)/(1 + exp(-0.4721 + 0.000001373*400000))

## predicted probability for 600K home
exp(-0.4721 + 0.000001373*600000)/(1 + exp(-0.4721 + 0.000001373*600000))

predict(model9, data.frame(price = c(400000, 600000)))
predict(model9, data.frame(price = c(400000, 600000)), type = "response")
