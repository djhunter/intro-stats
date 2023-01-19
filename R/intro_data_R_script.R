library(openintro)
library(tidyverse)

glimpse(hsb2)
view(hsb2)
?hsb2

hsb2 %>% 
  count(schtyp)

## Challenge: How many different values of the race variable are there 
#             in the data, and how many observations are in each category?

## Solution:
hsb2 %>% 
  count(race)

ggplot(data = hsb2, aes(x = science, y = math)) + 
  geom_point()

ggplot(data = hsb2, aes(x = science, y = math, color = prog)) +
  geom_point()

## Challenge: Take a look at the email50 data frame. Count the number of emails 
# that are spam and the number that are not spam. Make a scatterplot of two of 
# the quantitative variables in the data set, colored by one of the categorical
# variables

## Solution
email50 %>% 
  count(spam)

ggplot(email50, aes(x = num_char, y = exclaim_mess, color = spam)) +
  geom_point()

