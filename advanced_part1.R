# loading libraries
library(tidyverse)

library(DT)


# following code will search all the tweets in which "#PenguinAwarenessDay" is used
penguins_hashtag_tweets <- innocent %>%
  mutate(product = grepl("#PenguinAwarenessDay", text, ignore.case = TRUE)) %>%
  filter(product)


# let's make a data frame for our text and print it into Viewer
penguins_day_tweets_text <- penguins_hashtag_tweets %>%
  select(text)

# following code will push trough data into our Viewer
penguins_day_tweets_text %>%
  datatable

