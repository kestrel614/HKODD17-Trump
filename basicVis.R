#!/usr/bin/env Rscript

### Author: Yiming Li

### Text Mining with R (http://tidytextmining.com/) was extensively referred to in this project.

# install.packages("tidytext")
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("purrr")

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(tidytext)
library(qdapRegex)
library(tidyr)
library(scales)
library(purrr)
library(broom)

###### Check distribution of their tweets (by time / by device)

trumpTweets <- read.csv("./data/realdonaldtrump.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
ivankaTweets <- read.csv("./data/ivankatrump.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
trumpTweets$created_at <- format(strptime(trumpTweets$created_at, "%a %b %d %H:%M:%S %z %Y"), "%Y-%m-%d %H:%M:%S %z")
ivankaTweets$created_at <- format(strptime(ivankaTweets$created_at, "%a %b %d %H:%M:%S %z %Y"), "%Y-%m-%d %H:%M:%S %z")

allTweets <- bind_rows(trumpTweets %>% mutate(person = "Donald"), ivankaTweets %>% mutate(person = "Ivanka")) %>% mutate(timestamp = ymd_hms(created_at))
# > table(allTweets$source)
#
#              Adly Connect             bitly bitlink                    Buffer
#                        13                         1                      3272
#                   BumeBox             Camera on iOS               Cloudhopper
#                        44                        28                        25
#                  Facebook                 Hootsuite                 Instagram
#                       105                        53                      1321
#          Instagram on iOS                       iOS              Media Studio
#                         1                         4                         1
#           Mobile Web (M5)  Neatly For BlackBerry 10             Offerpop Apps
#                        60                         5                         1
#                 Percolate                 Periscope                 Pinterest
#                        79                         7                        31
#             Safari on iOS                 Socialcam             Sprout Social
#                         1                         1                      1683
#               Thunderclap                    Tumblr                 TweetDeck
#                         1                         3                       504
#                Twitlonger           TwitLonger Beta                   Twitpic
#                        23                       406                        11
#               Twitter Ads       Twitter for Android    Twitter for BlackBerry
#                        67                     14520                        97
#   Twitter for BlackBerry®          Twitter for iPad        Twitter for iPhone
#                       441                        38                      3296
#      Twitter for Websites   Twitter Mirror for iPad             Twitter QandA
#                       109                         1                        10
#        Twitter Web Client                     Twubs UberSocial for BlackBerry
#                     15133                         8                        81
# UberSocial Pro for iPhone            ÜberSocialOrig       Vine - Make a Scene
#                       207                       123                        10
#            YouTube on iOS
#                         1
allTweets$device <- ifelse(allTweets$source == "Twitter for Android", "Android", 
	ifelse((allTweets$source == "Twitter for iPhone" | allTweets$source == "Twitter for iPad"), "iOS",
	ifelse(allTweets$source == "Twitter Web Client", "Web", "Otherwise")))

allTweets$persondevice <- paste(allTweets$person, allTweets$device, sep = "")

ggplot(allTweets, aes(x = timestamp, fill = person)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Tweets by Time")

ggplot(allTweets[allTweets$person == "Donald",], aes(x = timestamp, fill = device)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Donald Trump Tweets by Time and Device")

ggplot(allTweets[allTweets$person == "Ivanka",], aes(x = timestamp, fill = device)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Ivanka Trump Tweets by Time and Device")

allTweets$created_at <- NULL




##### Clean allTweets

# Keep only the completely recorded tweets
allTweets$incomplete <- grepl("\\(cont\\)", allTweets$text)
cleanedAllTweets <- allTweets[!allTweets$incomplete,]
cleanedAllTweets$incomplete <- NULL

# Keep only the tweets that are not retweets
#
# > table(cleanedAllTweets$is_retweet)
# False  True
# 31437  9935
cleanedAllTweets <- cleanedAllTweets[!as.logical(cleanedAllTweets$is_retweet),]
cleanedAllTweets$is_retweet <- NULL

# Extract URLs from tweets
rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
cleanedAllTweets$urls <- unlist(sapply(rm_twitter_n_url(cleanedAllTweets$text, extract=TRUE), function(x) return(paste(x, collapse = "\t"))))
cleanedAllTweets$text <- rm_twitter_n_url(cleanedAllTweets$text)

# Extract hashtags from tweets
cleanedAllTweets$hashtags <- unlist(sapply(rm_hash(cleanedAllTweets$text, extract=TRUE), function(x) return(paste(x, collapse = "\t"))))
cleanedAllTweets$text <- rm_hash(cleanedAllTweets$text)

# Remove quotation marks and collapse multiple whitespaces
cleanedAllTweets$text <- gsub("'", '', cleanedAllTweets$text)
cleanedAllTweets$text <- gsub("\"", '', cleanedAllTweets$text)
cleanedAllTweets$text <- gsub("\\s+", " ", str_trim(cleanedAllTweets$text))

### Tidy tweets using tidytext
tidy_tweets <- cleanedAllTweets %>% mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% unnest_tokens(word, text, token = "regex", pattern = "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))") %>% filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))




##### Get word frequency using tidyr
frequency <- tidy_tweets %>% group_by(person) %>% count(word, sort = TRUE) %>% left_join(tidy_tweets %>% group_by(person) %>% summarise(total = n())) %>% mutate(freq = n/total)
# > frequency
# Source: local data frame [29,589 x 5]
# Groups: person [2]
#
#    person             word     n  total        freq
#     <chr>            <chr> <int>  <int>       <dbl>
# 1  Donald            trump  1991 142627 0.013959489
# 2  Donald            obama  1128 142627 0.007908741
# 3  Donald           people   978 142627 0.006857047
# 4  Ivanka             tips   812  64851 0.012521010
# 5  Donald             time   715 142627 0.005013076
# 6  Donald @realdonaldtrump   706 142627 0.004949974
# 7  Donald           donald   673 142627 0.004718602
# 8  Donald          america   665 142627 0.004662511
# 9  Ivanka     @ivankatrump   569  64851 0.008773959
# 10 Donald        president   559 142627 0.003919314
# # ... with 29,579 more rows

frequency <- frequency %>% select(person, word, freq) %>% spread(person, freq) %>% arrange(Donald, Ivanka)
# > frequency
# # A tibble: 24,444 × 3
#               word       Donald       Ivanka
#              <chr>        <dbl>        <dbl>
# 1          @10best 7.011295e-06 1.541996e-05
# 2   @britneyspears 7.011295e-06 1.541996e-05
# 3   @callmemrwayne 7.011295e-06 1.541996e-05
# 4     @charlierose 7.011295e-06 1.541996e-05
# 5    @claretourism 7.011295e-06 1.541996e-05
# 6  @coachdanmullen 7.011295e-06 1.541996e-05
# 7        @dabg3241 7.011295e-06 1.541996e-05
# 8  @dailymailceleb 7.011295e-06 1.541996e-05
# 9           @doral 7.011295e-06 1.541996e-05
# 10  @drudge_report 7.011295e-06 1.541996e-05
# # ... with 24,434 more rows

ggplot(frequency, aes(Ivanka, Donald)) + geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25, color = "lightblue") + geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) + geom_abline(color = "red") + ggtitle("Word Frequency Difference Between Ivanka and Donald Trump")



##### Word usage
word_ratios <- tidy_tweets %>% filter(!str_detect(word, "^@")) %>% count(word, person) %>% filter(sum(n) >= 10) %>% spread(person, n, fill = 0) %>% ungroup() %>% mutate_each(funs((. + 1) / sum(. + 1)), -word) %>% mutate(logratio = log(Donald / Ivanka)) %>% arrange(desc(logratio))

word_ratios %>% group_by(logratio < 0) %>% top_n(15, abs(logratio)) %>% ungroup() %>% mutate(word = reorder(word, logratio)) %>% ggplot(aes(word, logratio, fill = logratio < 0)) + geom_col() + coord_flip() + ylab("log odds ratio (Donald/Ivanka)") + scale_fill_discrete(name = "", labels = c("Donald", "Ivanka")) + ggtitle("Top 15 Most Distinctive Words for Donald v.s. Ivanka Trump")



##### Changes in word usage
words_by_time <- tidy_tweets %>% filter(!str_detect(word, "^@")) %>% mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>% count(time_floor, person, word) %>% ungroup() %>% group_by(person, time_floor) %>% mutate(time_total = sum(n)) %>% group_by(word) %>% mutate(word_total = sum(n)) %>% ungroup() %>% rename(count = n) %>% filter(word_total > 30)
nested_data <- words_by_time %>% nest(-word, -person)

nested_models <- nested_data %>% mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., family = "binomial")))

slopes <- nested_models %>% unnest(map(models, tidy)) %>% filter(term == "time_floor") %>% mutate(adjusted.p.value = p.adjust(p.value))

top_slopes <- slopes %>% filter(adjusted.p.value < 0.00000000000000000001)
words_by_time %>% inner_join(top_slopes, by = c("word", "person")) %>% filter(person == "Donald") %>% ggplot(aes(time_floor, count/time_total, color = word)) + geom_line(size = 1) + labs(x = NULL, y = "Word frequency") + ggtitle("Top 8 Trending Words in Donald Trump's Tweets")

top_slopes <- slopes %>% filter(adjusted.p.value < 0.00000000000000000001)
words_by_time %>% inner_join(top_slopes, by = c("word", "person")) %>% filter(person == "Ivanka") %>% ggplot(aes(time_floor, count/time_total, color = word)) + geom_line(size = 1) + labs(x = NULL, y = "Word frequency") + ggtitle("Top 3 Trending Words in Ivanka Trump's Tweets")




##### Retweets

totals <- tidy_tweets %>% group_by(person, id_str) %>% summarise(rts = sum(retweet_count)) %>% group_by(person) %>% summarise(total_rts = sum(rts))
#   person total_rts
#    <chr>     <int>
# 1 Donald 270962155
# 2 Ivanka   3063530

word_by_rts <- tidy_tweets %>% group_by(id_str, word, person) %>% summarise(rts = first(retweet_count)) %>% group_by(person, word) %>% summarise(retweet_count = median(rts), uses = n()) %>% left_join(totals) %>% filter(retweet_count != 0) %>% ungroup()

# word_by_rts %>% filter(uses >= 5) %>% arrange(desc(retweet_count))

word_by_rts %>% filter(uses >= 5) %>% group_by(person) %>% top_n(10, retweet_count) %>% arrange(retweet_count) %>% mutate(word = factor(word, unique(word))) %>% ungroup() %>% ggplot(aes(word, retweet_count, fill = person)) + geom_col(show.legend = FALSE) + facet_wrap(~ person, scales = "free", ncol = 2) + coord_flip() + labs(x = NULL, y = "Median # of retweets for tweets containing each word") + ggtitle("Top 10 Words Leading to a Larger Amount of Retweets for Donald and Ivanka Trump")




##### Favorites

totals <- tidy_tweets %>% group_by(person, id_str) %>% summarise(favs = sum(favorite_count)) %>% group_by(person) %>% summarise(total_favs = sum(favs))
#   person total_favs
#    <chr>      <int>
# 1 Donald  704259573
# 2 Ivanka   11179329

word_by_favs <- tidy_tweets %>% group_by(id_str, word, person) %>% summarise(favs = first(favorite_count)) %>% group_by(person, word) %>% summarise(favorite_count = median(favs), uses = n()) %>% left_join(totals) %>% filter(favorite_count != 0) %>% ungroup()

# word_by_favs %>% filter(uses >= 5) %>% arrange(desc(favorite_count))

word_by_favs %>% filter(uses >= 5) %>% group_by(person) %>% top_n(10, favorite_count) %>% arrange(favorite_count) %>% mutate(word = factor(word, unique(word))) %>% ungroup() %>% ggplot(aes(word, favorite_count, fill = person)) + geom_col(show.legend = FALSE) + facet_wrap(~ person, scales = "free", ncol = 2) + coord_flip() + labs(x = NULL, y = "Median # of favorites for tweets containing each word") + ggtitle("Top 10 Words Leading to a Larger Amount of Favorites for Donald and Ivanka Trump")








save(file = "tweets-basicVis.RData", list = c("allTweets", "cleanedAllTweets", "ivankaTweets", "trumpTweets", "tidy_tweets", "frequency", "slopes", "word_by_favs", "word_by_rts", "word_ratios", "words_by_time"))









####### Run all the visualizations again and save to TrumpBasicVis.pdf
### Duplicated codes!!!

load("tweets-basicVis.RData")
# library(lubridate)
# library(ggplot2)
# library(dplyr)
# library(readr)
# library(stringr)
# library(tidytext)
# library(qdapRegex)
# library(tidyr)
# library(scales)
# library(purrr)
# library(broom)

pdf("TrumpBasicVis.pdf", width = 10)

### Page 1. Distribution of Tweets by Time

ggplot(allTweets, aes(x = timestamp, fill = person)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Tweets by Time")
# Donald, on average, tweets much more than Ivanka, especially starting from 2012.

### Pages 2 and 3. Distribution of Tweets by Time and Device (Two plots)

ggplot(allTweets[allTweets$person == "Donald",], aes(x = timestamp, fill = device)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Donald Trump Tweets by Time and Device")
# Donald tweets via iOS devices only starting from late 2015.

ggplot(allTweets[allTweets$person == "Ivanka",], aes(x = timestamp, fill = device)) + geom_histogram(alpha = 0.5, position = "identity", bins = 20) + ggtitle("Distribution of Ivanka Trump Tweets by Time and Device")
# Ivanka tweets a lot via applications like Instagram (check the data frame to see).

### Page 4. Word Frequency Difference Between Ivanka and Donald Trump

ggplot(frequency, aes(Ivanka, Donald)) + geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25, color = "lightblue") + geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) + geom_abline(color = "red") + ggtitle("Word Frequency Difference Between Ivanka and Donald Trump")
# Words near the red line are used with about equal frequencies by Donald and Ivanka, while words far away from the line are used much more by one person compared to the other.
# Words, hashtags, and usernames that appear in this plot are ones that they have both used at least once in tweets.
#
# They all like tweeting their surnames a lot.
# Donald tweets a lot about Obama whereas Ivanka does not.
# Ivanka tweets a lot about fashion and giving tips whereas Donald does not.

### Page 5. Word Preference: Top 15 Most Distinctive Words for Donald v.s. Ivanka Trump

word_ratios %>% group_by(logratio < 0) %>% top_n(15, abs(logratio)) %>% ungroup() %>% mutate(word = reorder(word, logratio)) %>% ggplot(aes(word, logratio, fill = logratio < 0)) + geom_col() + coord_flip() + ylab("log odds ratio (Donald/Ivanka)") + scale_fill_discrete(name = "", labels = c("Donald", "Ivanka")) + ggtitle("Top 15 Most Distinctive Words for Donald v.s. Ivanka Trump")
# DT is more political: Hillary, Obama, ObamaCare, Iran, Iraq, ISIS
# DT is more critical: crooked, illegal, dishonest
# Ivanka is more affectionate: XO, XX
# Ivanka likes fashion, cooking, working out and giving tips

### Pages 6 and 7. Top Trending Words in Donald and Ivanka Trump's Tweets (Adjusted p-value < 1E-20s)

top_slopes <- slopes %>% filter(adjusted.p.value < 0.00000000000000000001)

words_by_time %>% inner_join(top_slopes, by = c("word", "person")) %>% filter(person == "Donald") %>% ggplot(aes(time_floor, count/time_total, color = word)) + geom_line(size = 1) + labs(x = NULL, y = "Word frequency") + ggtitle("Top 8 Trending Words in Donald Trump's Tweets")
# Usages of Hillary and Clinton rises in late 2015, along with America.

words_by_time %>% inner_join(top_slopes, by = c("word", "person")) %>% filter(person == "Ivanka") %>% ggplot(aes(time_floor, count/time_total, color = word)) + geom_line(size = 1) + labs(x = NULL, y = "Word frequency") + ggtitle("Top 3 Trending Words in Ivanka Trump's Tweets")
# Tips, tips, tips.

### Page 8. Top 10 Words Leading to a Larger Amount of Retweets for Donald and Ivanka Trump
word_by_rts %>% filter(uses >= 5) %>% group_by(person) %>% top_n(10, retweet_count) %>% arrange(retweet_count) %>% mutate(word = factor(word, unique(word))) %>% ungroup() %>% ggplot(aes(word, retweet_count, fill = person)) + geom_col(show.legend = FALSE) + facet_wrap(~ person, scales = "free", ncol = 2) + coord_flip() + labs(x = NULL, y = "Median # of retweets for tweets containing each word") + ggtitle("Top 10 Words Leading to a Larger Amount of Retweets for Donald and Ivanka Trump")
# DT: self-evident...
# Ivanka: Famous scientists?? Appeals to families.

### Page 9. Top 10 Words Leading to a Larger Amount of Favorites for Donald and Ivanka Trump
word_by_favs %>% filter(uses >= 5) %>% group_by(person) %>% top_n(10, favorite_count) %>% arrange(favorite_count) %>% mutate(word = factor(word, unique(word))) %>% ungroup() %>% ggplot(aes(word, favorite_count, fill = person)) + geom_col(show.legend = FALSE) + facet_wrap(~ person, scales = "free", ncol = 2) + coord_flip() + labs(x = NULL, y = "Median # of favorites for tweets containing each word") + ggtitle("Top 10 Words Leading to a Larger Amount of Favorites for Donald and Ivanka Trump")
# Interpret this graph together with the previous one.

dev.off()
