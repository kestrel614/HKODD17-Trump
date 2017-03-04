#!/usr/bin/env Rscript

### Author: Yiming Li

trumpTweets <- read.csv("./data/realdonaldtrump.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
ivankaTweets <- read.csv("./data/ivankatrump.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# > str(trumpTweets)
# 'data.frame':	30563 obs. of  8 variables:
# > str(ivankaTweets)
# 'data.frame':	11263 obs. of  8 variables:

trumpTweets$created_at <- strptime(trumpTweets$created_at, "%a %b %d %H:%M:%S %z %Y")
#"Wed Jan 25 01:03:51 +0000 2017"
ivankaTweets$created_at <- strptime(ivankaTweets$created_at, "%a %b %d %H:%M:%S %z %Y")


trumpTweets$is_retweet <- ifelse(trumpTweets$is_retweet == "False", FALSE, TRUE)
ivankaTweets$is_retweet <- ifelse(ivankaTweets$is_retweet == "False", FALSE, TRUE)

save(file = "tweets.RData", list = ls())
