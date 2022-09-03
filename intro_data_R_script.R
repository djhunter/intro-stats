library(tidyverse)
library(openintro)

glimpse(hsb2)

glimpse(email50)
hsb2 %>% 
  count(schtyp)
hsb2_public <- hsb2 %>%
  filter(schtyp == "public")
hsb2_public %>%
  count(schtyp)
email50_big <- email50 %>%
  filter(number == "big")

mean(hsb2$read)
avg_read <- mean(hsb2$read)
hsb2 <- hsb2 %>%
  mutate(read_cat = if_else(read < avg_read,
                            "below average",
                            "at or above average")
  )
hsb2

# Calculate median number of characters: med_num_char
med_num_char <- median(email50$num_char)

# Create num_char_cat variable in email50
email50_updated <- email50 %>%
  mutate(num_char_cat = if_else(num_char < med_num_char,
                                "below median",
                                "at or above median")
  )

# Count emails in each category
email50_updated %>%
  count(num_char_cat)

email50_updated <- email50 %>%
  mutate(number_cat = if_else(number == "none", 
                       "no", 
                       "yes")
  )

ggplot(email50_updated, aes(x = number_cat)) +
  geom_bar()

ggplot(data = hsb2, aes(x = science, y = math)) + 
  geom_point()

ggplot(data = hsb2, aes(x = science, y = math, color = prog)) +
  geom_point()

ggplot(email50, aes(x = num_char, y = exclaim_mess, color = spam)) +
  geom_point()

