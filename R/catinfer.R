library(tidyverse)

gss <- read_csv("http://math.westmont.edu/data/gss.csv")

gss %>%
  ggplot(aes(x = metal)) +
    geom_bar()

## Challenge: incorporate opera into the above plot. Do you think
## the plot suggests an association between opera and metal? Record
## a sentence.

######################Solution:
gss %>%
  ggplot(aes(x = metal, fill = opera)) +
    geom_bar(position = "fill")
# It looks like people who like metal are less likely to like opera, but 
# the difference is small.
#####################################################

# make a table
gss %>%
  select(metal, opera) %>%
  table()

## Challenge: Make two new variables:
## p1hat = proportion of metal-likers who like opera
## p2hat = proportion of metal-dislikers who like opera
## Then compute p1hat - p2hat

## Solution:
p1hat <- 6/38
p2hat <- 52/262
p1hat - p2hat
## -0.04
###################################################

gss %>%
  select(metal, opera) %>%
  table() %>% 
  prop.test()

gss %>%
  select(metal, opera) %>%
  table() %>% 
  chisq.test()

## Challenge: Is political party associated with church attendance? 
## Make a bar plot: Record what you observe.
## Do a chisq.test. Record what you conclude.
## Try a prop.test. What happens. Why?

######################Solution:

gss %>%
  ggplot(aes(x = party, fill = church)) +
    geom_bar(position = "fill")

gss %>%
  select(church, party) %>%
  table() %>% 
  chisq.test()

gss %>%
  select(church, party) %>%
  table() %>% 
  prop.test()
#################################################

## Challenge: Find two categorical variables in the data
## set that have a significant association. Make a bar plot
## and do an appropriate test. Record the variables you used.

######################Solution:
gss %>%
  select(church, belief) %>%
  table() 

gss %>%
  ggplot(aes(x = belief, fill = church)) +
    geom_bar(position = "fill")

gss %>%
  select(church, belief) %>%
  table() %>% 
  chisq.test()

gss %>%
  select(belief, church) %>%
  filter(belief != "athiest") %>%
  table() %>% 
  prop.test()
###################################################

# Challenge: Are churchgoers happier? Answer with a bar plot and an 
# appropriate test/CI.

######################Solution:
gss %>%
  select(church, happy) %>%
  table() %>% 
  prop.test()

gss %>%
  ggplot(aes(x = church, fill = happy)) +
    geom_bar(position = "fill")
###################################################

#####################
## HW solutions

library(tidyverse)
library(openintro)
glimpse(cle_sac)
?cle_sac

cle_sac %>% ggplot(aes(x = city, fill = race)) + geom_bar()

cle_sac %>%
  select(city, race) %>%
  table() %>% 
  chisq.test()

