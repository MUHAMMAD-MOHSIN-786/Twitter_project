# Load libraries
install.packages("plyr")  # use the line to install different packages
library(tidyverse)

library(rtweet) #for Twitter API 

library(DT) #for nicer tables

library(rvest) #for scraping

library(tidytext) #for text mining

library(wordcloud) #for wordclouds

library(stringr)  # for string manipulations

library(plyr)


url <- "https://www.innocentdrinks.co.uk/things-we-make/innocent-shots-1"
page <- read_html(url)
page


# let's look for products on the website
products <- page %>%
  html_nodes(".product-name") %>%
  html_text()

products


# let's clean our strings using "stringr" functions
products <- products %>%
  str_replace("\n","") %>%
  str_trim()

products


# let's extract all product related tweets from all earned tweets
products_regex <- paste(products, collapse="|") %>%
  paste0(., "|juice")

# 'Product' column checks whether our products feature in the text
juice_shots_innocent <- innocent %>%
  mutate(product = grepl(products_regex, text, ignore.case = TRUE)) %>%
  filter(product)
NROW(juice_shots_innocent)

# let's search for all tweets related to our products
searchquery <- paste(products, collapse=" OR ") %>%
  paste(., "OR juice") %>%
  paste0("to:innocent AND (", ., ")")

juice_shots_users <- search_tweets(searchquery, n=500, include_rts = F)
NROW(juice_shots_users)

# let's check the number of cols
ncol(juice_shots_innocent)
  # also check the no. of cols for another dataframe
ncol(juice_shots_users)


# let's check the diff b/w both data-frames
setdiff(names(dairyfree_innocent), names(dairyfree_users))



juice_shots_simple <- juice_shots_innocent %>%
  select(text, created_at, user_id, screen_name, source, reply_to_screen_name, retweet_count, reply_count, quote_count, hashtags)

juice_shots_simple %>%
  datatable


                    ### SENTIMENT_ANALYSIS ###
bing <- get_sentiments("bing")

bing


# let's make a token for juice shots
tokens <- juice_shots_simple %>%
  unnest_tokens(., output = "word", input = "text")

head(tokens)



# let's join both tokens and bing
sentiments <- inner_join(tokens, bing)
head(sentiments)


# let's check by numbers, how much are positive and negative
ggplot(sentiments) +
  geom_bar(aes(x = sentiment))


# there is no tweet to innocent, so no plot for "to"
sentiments <- sentiments %>%
  mutate(from_to = ifelse(screen_name == "innocent", "from"))

ggplot(sentiments) +
  geom_bar(aes(x = sentiment))+
  facet_wrap(~from_to)


# let's make wordcloud for tweets w.r.t size and frequency of the words
sentiments %>%
  group_by(sentiment) %>%
  count(word) %>%
  with(wordcloud(word, n, min.freq = 1, random.order=FALSE, ordered.colors=TRUE, colors=c("red","darkgreen")[factor(sentiment)]))


# NOTE: there is no col for juice_shots_users above in "innocent's tweets"  so that's why analysed 
# just "juice_shots_innocent" sentimently