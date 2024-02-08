library(tidyverse)

gss <- read_csv("http://math.westmont.edu/data/gss.csv")

glimpse(gss)

gss %>% 
  count(church)

ggplot(gss, aes(x = church)) +
  geom_bar()

gss %>%
  summarize(phat = sum(church == "often")/300)

## Create a random resample
samp1 <- gss %>%
  slice_sample(n = 300, replace = TRUE)

samp1 %>%
  count(church)
  
# Challenge: 
# 1. Repeat the above two commands (make a sample, then count) a 
#   few times. Does the sample change?
# 2. Summarize to compute p_hat for this sample

######### Solution
gss %>%
  slice_sample(n = 300, replace = TRUE) %>%
  count(church)

gss %>%
  slice_sample(n = 300, replace = TRUE) %>%
  summarize(p_hat = sum(church == "often")/300)
#######

## Make lots of p_hats

makephat <- function(i) {
  gss %>%
  slice_sample(n = 300, replace = TRUE) %>%
  summarize(p_hat = sum(church == "often")/300)
}

## Try it out:
makephat()
makephat()

## Use map to repeat the function:
map(1:5, makephat)

map(1:5, makephat) %>%
  bind_rows()

phatDF <- map(1:100, makephat) %>%
  bind_rows()

ggplot(phatDF, aes(x = p_hat)) +
  geom_histogram()

## Challenge: 
## 1. Modify the above code to create a sampling distribution with 1000 p_hats.
## 2. Try using geom_density instead of geom_histogram.

#### Solution
phatDF <- map(1:1000, makephat) %>%
  bind_rows()

ggplot(phatDF, aes(x = p_hat)) +
  geom_density()
#######

## Create a confidence interval

phatDF %>%
  summarize(p025 = quantile(p_hat, 0.025),
            p975 = quantile(p_hat, 0.975))

## Interpretation: If we repeated the survey many times, 95% of the 
## phats would fall between 0.397 and 0.51.

gss %>%
  count(church)

## Simulated data: sample size of 3000 instead of 300.
biggss <- data.frame(
  church = c(rep("often", 1360), rep("rarely", 1640))
)

### Challenge: Redo the bootstrapping simulation, but this time
#              use a biggss simulated data. How does the 95% CI change?

############## Solution:

makephat <- function(i) {
  biggss %>%
  slice_sample(n = 3000, replace = TRUE) %>%
  summarize(p_hat = sum(church == "often")/3000)
}

phatDF <- map(1:1000, makephat) %>%
  bind_rows()

ggplot(phatDF, aes(x = p_hat)) +
  geom_density()
#######

## Create a confidence interval

phatDF %>%
  summarize(p025 = quantile(p_hat, 0.025),
            p975 = quantile(p_hat, 0.975))


###############################################
