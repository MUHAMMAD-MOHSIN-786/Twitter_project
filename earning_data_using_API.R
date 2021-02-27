# Load libraries

library(tidyverse)

library(rtweet) #for Twitter API

library(DT) #for nicer tables

library(rvest) #for scraping

library(tidytext) #for text mining

library(RColorBrewer) # a dependency for "wordcloud" library

library(wordcloud) #for wordclouds



# Name assigned to your app (the unique name you input above)
appname <- "coding_jia"

# API key (input your own! the below is fake)
key <- "GCDxvGZ15Xgh2xOuCvu02rUcd"

# API secret (input your own! the below is fake)
secret <- "XRSrmebvLtRethaXJXwfSpCLYJ8OfFVR6RXIhAwc2oKD6SUz9r"

# Access Token (input your own! the below is fake)
token <- "1354523195366322183-XOSt5Ud1jhmjReq00BLsZVjJP3EK2n"

# Access Secret (input your own! the below is fake)
access_secret <- "zO8IQK8uOQ8mZiskKaSDGMPIicYuZtYvLoH4HN0IwwCoN"

twitter_token <- create_token(
    app = appname,
    consumer_key = key,
    consumer_secret = secret, 
    access_token = token,
    access_secret = access_secret)

# let's see our dataframe
rate_limit(twitter_token) %>%
    datatable()     # this function pushes the results to Viewer in right side 


# We’ll start by getting tweets from Innocent’s company account 
innocent <- get_timeline("innocent", n = 3000)

head(innocent, 1)


# let's pull user's data with following function
profile <- users_data(innocent)

profile %>%
    distinct() %>%   # this gives all unique names as data frame
    datatable


# let's manipulate dates and time using the "lubridate" library
times <- innocent %>%
    mutate(Date = as.Date(created_at),
        Hour = format(created_at, format="%H"),
        Hour = as.numeric(Hour))

times %>%
    select(Date, Hour, text) %>%
    slice_head(n = 10)            # it works just like head() function


# let's count how many tweets are being tweeted by Innocent
times %>%
    count(Date) %>%
    arrange(desc(n)) 


# let's visuallize above data in more easier way using "ggplot()" which is tidyvrse's package
times %>%
    count(Date) %>%
    ggplot(aes(x = Date, y = n)) + #Plug in the data
    geom_line() + #Kind of plot
    labs(x = "Day", y = "Number of Tweets", title = "@innocent Tweets") #Labels


# let's plot the number of tweets agaist the time of day
ggplot(times) +
    geom_bar(aes(x = Hour)) +
    labs(x = "Time of day", y = "Number of Tweets", title = "@innocent Tweets") +
    scale_x_continuous(n.breaks = 24)


# let's find out the friends of innocent
friends <- get_friends("innocent")
NROW(friends)      # to see the friend-list
# head(friends)


# let's move on next pages to dig more friends 
friends2 <- get_friends("innocent", page = next_cursor(friends))
friends3 <- get_friends("innocent", page = next_cursor(friends2))
friends4 <- get_friends("innocent", page = next_cursor(friends3))
friends5 <- get_friends("innocent", page = next_cursor(friends4))
friends6 <- get_friends("innocent", page = next_cursor(friends5))
friends7 <- get_friends("innocent", page = next_cursor(friends6))


# let's bind the rows and distinct them
friends <- rbind(friends,friends2,friends3, friends4, friends5, friends6, friends7) %>%
    distinct 
head(friends)

# let's remove some friends
rm(friends2,friends3, friends4, friends5, friends6, friends7) 
# let's check the number of rows of remaining friends
NROW(friends)


# let's lookup for each userid against each friend
friends_moreinfo <- lookup_users(friends$user_id)


# let's look for just those who are verified (famous) friends
friends_moreinfo %>%
    filter(verified == TRUE) %>%
    select(screen_name) %>%
    datatable

# let's save all above hard earned data, to avoid the code above all over again. because,
# we can jsut queryfy a server just 15 times
save(innocent, friends_moreinfo, file = './R_programming/week_3/innocent_data.Rdata')
