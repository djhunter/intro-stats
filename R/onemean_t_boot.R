library(tidyverse)
library(openintro)
glimpse(cherry)
?cherry
data(cherry)

## Challenge: Make a density plot of each variable in 
# this data set. For each density plot, RECORD:
# 1. Is the variable skewed?
# 2. Estimate a range in which most (i.e., 95% of the data lies).

### Solution
ggplot(cherry, aes(x = diam)) + 
  geom_density()
# skewed right, (10, 18)

ggplot(cherry, aes(x = height)) + 
  geom_density()
# skewed left (slightly), (65, 85)

ggplot(cherry, aes(x = volume)) + 
  geom_density()
# skewed right, (15, 75)
############################################

## Compute the means:
cherry %>%
  summarize(xbar_diam = mean(diam),
            xbar_height = mean(height),
            xbar_volume = mean(volume))

## Create a random resample
cherry %>%
  slice_sample(n = 31, replace = TRUE) %>%
  summarize(xbar_diam = mean(diam),
            xbar_height = mean(height),
            xbar_volume = mean(volume))

makexbars <- function(i) {
cherry %>%
  slice_sample(n = 31, replace = TRUE) %>%
  summarize(xbar_diam = mean(diam),
            xbar_height = mean(height),
            xbar_volume = mean(volume))
}

## Try it out:
makexbars()
makexbars()

## Use map to repeat the function:

xbarDF <- map(1:1000, makexbars) %>%
  bind_rows()

glimpse(xbarDF)

## Challenge: Make a density plot of the bootstrapped xbars 
# for each variable. For each density plot, RECORD:
# 1. Is the distribution of xbar skewed?
# 2. Estimate a range in which most (i.e., 95% of the data lies).

### Solution
ggplot(xbarDF, aes(x = xbar_diam)) + 
  geom_density()
# symmetric: (12.5, 14.5)
ggplot(xbarDF, aes(x = xbar_height)) + 
  geom_density()
# symmetric: (73, 79)
ggplot(xbarDF, aes(x = xbar_volume)) + 
  geom_density()
# symmetric: (25, 36)
#############################################

## One sample t-tests
cherry %>%
  select(diam) %>%
  t.test()

# or

t.test(cherry$diam)

## Do one-sample t-tests for the other two variables. Compare the 
# confidence interval to the estimates from the bootstrap distributions

### Solution:
cherry %>%
  select(height) %>%
  t.test()

cherry %>%
  select(volume) %>%
  t.test()

