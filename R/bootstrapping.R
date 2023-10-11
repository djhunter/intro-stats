library(tidyverse)

# Sample data: Poll of 50 people. 
#    Do you attend church regularly? (Yes, No)
#    20% respond yes
polldata <- data.frame(
  church = c(rep("Yes", 10), rep("No", 40))
)

glimpse(polldata)
polldata %>%
  count(church)

ggplot(polldata, aes(x = church)) +
  geom_bar()

polldata %>%
  summarize(phat = sum(church == "Yes")/n())

## Create a random sample
samp1 <- polldata %>%
  slice_sample(n = 50, replace = TRUE)

samp1 %>%
  count(church)
  
# Challenge: 
# 1. Repeat the above two commands (make a sample, then count) a 
#   few times. Does the sample change?
# 2. Summarize to compute p_hat for this sample

######### Solution
samp1 %>% 
  summarize(p_hat = sum(church == "Yes")/n())
#######

## Create a sampling distribution

phat_list <- list() # Start with an empty list
for (i in 1:10) {
  samp1 <- polldata %>%
    slice_sample(n = 50, replace = TRUE)
  phat_list[[i]] <- samp1 %>%
    summarize(p_hat = sum(church == "Yes")/n())
}
phatDF <- bind_rows(phat_list)
glimpse(phatDF)

ggplot(phatDF, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.05)

## Challenge: 
## 1. Modify the above code to create a sampling distribution with 1000 p_hats.
## 2. Try using geom_density instead of geom_histogram.

#### Solution
phat_list <- list() # Start with an empty list
for (i in 1:1000) {
  samp1 <- polldata %>%
    slice_sample(n = 50, replace = TRUE)
  phat_list[[i]] <- samp1 %>%
    summarize(p_hat = sum(church == "Yes")/n())
}
phatDF <- bind_rows(phat_list)

ggplot(phatDF, aes(x = p_hat)) +
  geom_density()

ggplot(phatDF, aes(x = p_hat)) +
  geom_density(adjust = 1.5)

ggplot(phatDF, aes(x = p_hat)) +
  geom_density(adjust = 2)
#######

## Summarize the distribution

phatDF %>%
  summarize(mean = mean(p_hat),
            SE = sd(p_hat),
            p025 = quantile(p_hat, 0.025),
            p975 = quantile(p_hat, 0.975))

# Quick approximation of 95% CI
0.2 - 1.96*0.056194
0.2 + 1.96*0.056194

### Challenge: Redo the simulation of a sampling distribution, but this time
#              use a sample size of 100 instead of 50. How does the standard
#              error change? How does the 95% CI change?

############## Solution:
phat_list <- list() # Start with an empty list
for (i in 1:1000) {
  samp1 <- polldata %>%
    slice_sample(n = 100, replace = TRUE)
  phat_list[[i]] <- samp1 %>%
    summarize(p_hat = sum(church == "Yes")/n())
}
phatDF <- bind_rows(phat_list)

ggplot(phatDF, aes(x = p_hat)) +
  geom_density(adjust = 1.5)

ggplot(phatDF, aes(x = p_hat)) +
  geom_density(adjust = 2)
#######

phatDF %>%
  summarize(mean = mean(p_hat),
            SE = sd(p_hat),
            p025 = quantile(p_hat, 0.025),
            p975 = quantile(p_hat, 0.975))

# Quick approximation of 95% CI
0.2 - 1.96*0.04023
0.2 + 1.96*0.04023


## Answers to HW:

phat_list <- list()
for (i in 1:1000) {
  samp1 <- population %>%
    sample_n(40)
  phat_list[[i]] <- samp1 %>%
    summarize(p_hat = sum(church == "Yes")/n())
}
phatDF <- bind_rows(phat_list)
glimpse(phatDF)

ggplot(phatDF, aes(x = p_hat)) +
  geom_histogram(binwidth = 0.05)

ggplot(phatDF, aes(x = p_hat)) +
  geom_density(adjust = 2)

phatDF %>%
  summarize(mean = mean(p_hat),
          SE = sd(p_hat),
          p025 = quantile(p_hat, 0.025),
          p975 = quantile(p_hat, 0.975))

0.05 - 1.96 * 0.034
0.05 + 1.96 * 0.034

