library(tidyverse)
library(openintro)

#build up slowly

glimpse(ucla_textbooks_f18)
textbooks <- ucla_textbooks_f18 %>% 
  select(subject, course_num, bookstore_new, amazon_new) %>%
  mutate(price_diff = bookstore_new - amazon_new) %>%
  filter(!is.na(price_diff))
  
t.test(textbooks$bookstore_new, textbooks$amazon_new, paired = TRUE)

t.test(textbooks$price_diff)

