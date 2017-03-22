#!/usr/bin/env Rscript

### Author: Yiming Li

### Text Mining with R (http://tidytextmining.com/) was extensively referred to in this project.





####################################### Functions / Packages Load

library(scales)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(Cairo) # For Chinese characters

library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

library(igraph)
library(ggraph)

# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





####################################### Start of main

load("tweets-basicVis.RData")
##### Trump on Android is angry in most afternoons
# By time, by device

################### Before the sentiment analysis, we do some word-freq-based analysis first

tidy_tweets %>% count(word, sort = TRUE)
# # A tibble: 24,444 × 2
#                word     n
#               <chr> <int>
# 1             trump  2494
# 2             obama  1130
# 3            people  1055
# 4              time   930
# 5  @realdonaldtrump   831
# 6              tips   821
# 7               day   739
# 8              love   714
# 9            donald   680
# 10          america   677
# # ... with 24,434 more rows

# > unique(tidy_tweets$persondevice)
# [1] "IvankaOtherwise" "DonaldOtherwise" "DonaldAndroid"   "IvankaiOS"
# [5] "DonaldiOS"       "IvankaWeb"       "DonaldWeb"



###### Word frequency of tweets via different devices

p1 <- tidy_tweets %>% filter(persondevice == "DonaldWeb") %>% count(word, sort = TRUE) %>% mutate(perc = n / sum(n)) %>% top_n(20) %>% mutate(word = reorder(word, perc)) %>% ggplot(aes(word, perc)) + geom_col() + xlab(NULL) + ylab(NULL) + coord_flip(ylim = c(0, 0.02)) + ggtitle("Top 20 Most Frequent Words in Donald Trump Tweets via Browser") + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 7))

p2 <- tidy_tweets %>% filter(persondevice == "DonaldAndroid") %>% count(word, sort = TRUE) %>% mutate(perc = n / sum(n)) %>% top_n(20) %>% mutate(word = reorder(word, perc)) %>% ggplot(aes(word, perc)) + geom_col() + xlab(NULL) + ylab(NULL) + coord_flip(ylim = c(0, 0.02)) + ggtitle("Top 20 Most Frequent Words in Donald Trump Tweets via Android") + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 7))

p3 <- tidy_tweets %>% filter(persondevice == "DonaldiOS") %>% count(word, sort = TRUE) %>% mutate(perc = n / sum(n)) %>% top_n(20) %>% mutate(word = reorder(word, perc)) %>% ggplot(aes(word, perc)) + geom_col() + xlab(NULL) + ylab(NULL) + coord_flip(ylim = c(0, 0.02)) + ggtitle("Top 20 Most Frequent Words in Donald Trump Tweets via iOS") + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 7))

p4 <- tidy_tweets %>% filter(persondevice == "DonaldOtherwise") %>% count(word, sort = TRUE) %>% mutate(perc = n / sum(n)) %>% top_n(20) %>% mutate(word = reorder(word, perc)) %>% ggplot(aes(word, perc)) + geom_col() + xlab(NULL) + ylab(NULL) + coord_flip(ylim = c(0, 0.02)) + ggtitle("Top 20 Most Frequent Words in Donald Trump Tweets via Others") + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 7))



###### Comparing the word frequencies of tweets via different devices

freqbyDonaldDevice <- tidy_tweets %>% count(persondevice, word) %>% mutate(proportion = n / sum(n)) %>% select(-n) %>% spread(persondevice, proportion) %>% gather(persondevice, proportion,`DonaldiOS`:`DonaldAndroid`)

p5 <- ggplot(freqbyDonaldDevice, aes(x = proportion, y = `DonaldWeb`, color = abs(`DonaldWeb` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~persondevice, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "DonaldWeb", x = NULL)

freqbyIvankaDevice <- tidy_tweets %>% count(persondevice, word) %>% mutate(proportion = n / sum(n)) %>% select(-n) %>% spread(persondevice, proportion) %>% gather(persondevice, proportion,`IvankaiOS`:`IvankaOtherwise`)

p6 <- ggplot(freqbyIvankaDevice, aes(x = proportion, y = `IvankaWeb`, color = abs(`IvankaWeb` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~persondevice, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "IvankaWeb", x = NULL)

cor.test(data = freqbyDonaldDevice[freqbyDonaldDevice$persondevice == "DonaldiOS",], ~ proportion + `DonaldWeb`)
# 	Pearson's product-moment correlation
#
# data:  proportion and DonaldWeb
# t = 28.85, df = 2776, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4511481 0.5083880
# sample estimates:
#       cor
# 0.4802792
cor.test(data = freqbyDonaldDevice[freqbyDonaldDevice$persondevice == "DonaldAndroid",], ~ proportion + `DonaldWeb`)
# 	Pearson's product-moment correlation
#
# data:  proportion and DonaldWeb
# t = 65.561, df = 4570, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6809440 0.7108322
# sample estimates:
#       cor
# 0.6961897

cor.test(data = freqbyIvankaDevice[freqbyIvankaDevice$persondevice == "IvankaiOS",], ~ proportion + `IvankaWeb`)
# 	Pearson's product-moment correlation
#
# data:  proportion and IvankaWeb
# t = 36.205, df = 1431, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6633982 0.7175219
# sample estimates:
#      cor
# 0.691429
cor.test(data = freqbyIvankaDevice[freqbyIvankaDevice$persondevice == "IvankaOtherwise",], ~ proportion + `IvankaWeb`)
# 	Pearson's product-moment correlation
#
# data:  proportion and IvankaWeb
# t = 37.732, df = 3027, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5408585 0.5893229
# sample estimates:
#       cor
# 0.5655788





################### Start of sentiment analysis
#### Lexicon we use: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

# Remove "trump" and other words not actually related to sentiment
custom_stop_words <- bind_rows(data_frame(word = c("trump", "mar", "miss", "funny", "fall"), lexicon = c("custom")), stop_words)
# %>% anti_join(custom_stop_words)

###### Plot sentiment over time

donaldsentiment <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = floor_date(timestamp, "month"), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)

p7 <- ggplot(donaldsentiment, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Donald Trump's tweets over time") + scale_x_datetime(breaks = date_breaks("6 months"), minor_breaks = date_breaks("1 month"), labels = date_format("%Y/%m")) + labs(x = NULL)

ivankasentiment <- tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = floor_date(timestamp, "month"), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)

p8 <- ggplot(ivankasentiment, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Ivanka Trump's tweets over time") + scale_x_datetime(breaks = date_breaks("6 months"), minor_breaks = date_breaks("1 month"), labels = date_format("%Y/%m")) + labs(x = NULL)



###### Most common positive / negative words

bing_word_counts_D <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

p9 <- bing_word_counts_D %>% group_by(sentiment) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + labs(y = "Contribution to sentiment", x = NULL) + coord_flip()

bing_word_counts_I <- tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()

p10 <- bing_word_counts_I %>% group_by(sentiment) %>% top_n(15) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") + labs(y = "Contribution to sentiment", x = NULL) + coord_flip()



##### New word clouds colored by sentiment

tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)



###### 子午丑未寅申卯酉辰戌巳亥時
# 子 夜半 23-1
# 丑 雞鳴 1-3
# 寅 平旦 3-5
# 卯 日出 5-7
# 辰 食時 7-9
# 巳 隅中 9-11
# 午 日中 11-13
# 未 日昳 13-15
# 申 晡時 15-17
# 酉 日入 17-19
# 戍 黃昏 19-21
# 亥 入定 21-23

# 0:23 -> 1:24 -> 子丑丑丑寅寅寅...
chinesetime <- c("子時", rep("丑時", 2), rep("寅時", 2), rep("卯時", 2), rep("辰時", 2), rep("巳時", 2), rep("午時", 2), rep("未時", 2), rep("申時", 2), rep("酉時", 2), rep("戌時", 2), rep("亥時", 2), "子時")
uniquechinesetime <- c("子時", "丑時", "寅時", "卯時", "辰時", "巳時", "午時", "未時", "申時", "酉時", "戌時", "亥時")

# Old way of calculating positiveness
#
# donaldsentiment_ct <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = factor(chinesetime[hour(timestamp)+1], levels = uniquechinesetime, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)
#
# Here we use ((# of +ve words / # of -ve words) - 1) to measure the overall positiveness
# This is because now we are considering only the Chinese hour categories
# If Trump tweets much less frequently in one hour, the result would be very biased.
#
# Therefore notice that this set of graphs is not comparable with p7 and p8
#
donaldsentiment_ct <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = factor(chinesetime[hour(timestamp)+1], levels = uniquechinesetime, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive / negative - 1)

p11 <- ggplot(donaldsentiment_ct, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Donald Trump's tweets by Chinese hours") + labs(x = NULL) + theme(axis.text.x = element_text(family = "SimSun"))

ivankasentiment_ct <- tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = factor(chinesetime[hour(timestamp)+1], levels = uniquechinesetime, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive / negative - 1)

p12 <- ggplot(ivankasentiment_ct, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Ivanka Trump's tweets by Chinese hours") + labs(x = NULL) + theme(axis.text.x = element_text(family = "SimSun"))

# Can use 夜半, etc. to be more poetic...
chinesetime3 <- c("夜半", rep("雞鳴", 2), rep("平旦", 2), rep("日出", 2), rep("食時", 2), rep("隅中", 2), rep("日中", 2), rep("日昳", 2), rep("晡時", 2), rep("日入", 2), rep("黃昏", 2), rep("入定", 2), "夜半")
uniquechinesetime3 <- c("夜半", "雞鳴", "平旦", "日出", "食時", "隅中", "日中", "日昳", "晡時", "日入", "黃昏", "入定")

donaldsentiment_ct2 <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = factor(chinesetime3[hour(timestamp)+1], levels = uniquechinesetime3, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive / negative - 1)

p11_alt <- ggplot(donaldsentiment_ct2, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Donald Trump's tweets by Chinese hours") + labs(x = NULL) + theme(axis.text.x = element_text(family = "SimSun"))

ivankasentiment_ct2 <- tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(device, index = factor(chinesetime3[hour(timestamp)+1], levels = uniquechinesetime3, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive / negative - 1)

p12_alt <- ggplot(ivankasentiment_ct2, aes(index, sentiment, fill = device)) + geom_col(show.legend = FALSE) + facet_wrap(~device, ncol = 1) + ggtitle("Sentiment of Ivanka Trump's tweets by Chinese hours") + labs(x = NULL) + theme(axis.text.x = element_text(family = "SimSun"))




###### Relationship between words
tweets_bigrams <- cleanedAllTweets %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
tweets_bigrams %>% count(bigram, sort = TRUE)
# # A tibble: 177,877 × 2
#       bigram     n
#        <chr> <int>
# 1  thank you  1574
# 2     of the  1507
# 3    will be  1283
# 4     in the  1150
# 5     on the   840
# 6    a great   826
# 7    for the   821
# 8       is a   817
# 9     to the   727
# 10     to be   659
# # ... with 177,867 more rows

bigrams_separated <- tweets_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
# > bigram_counts
# Source: local data frame [58,455 x 3]
# Groups: word1 [15,684]
#
#        word1         word2     n
#        <chr>         <chr> <int>
# 1     donald         trump   478
# 2    crooked       hillary   199
# 3    hillary       clinton   192
# 4     ivanka         trump   168
# 5  president         obama   153
# 6  celebrity    apprentice   149
# 7      trump         tower   145
# 8          5          tips   141
# 9      trump      national   139
# 10     trump international   131
# # ... with 58,445 more rows
bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")

# Trigrams
# cleanedAllTweets %>% unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word, !word3 %in% stop_words$word) %>% count(word1, word2, word3, sort = TRUE)


bigram_logratio <- bigrams_united %>% count(bigram, person) %>% filter(sum(n) >= 5) %>% spread(person, n, fill = 0) %>% ungroup() %>% mutate_each(funs((. + 1) / sum(. + 1)), -bigram) %>% mutate(logratio = log(Donald / Ivanka)) %>% arrange(desc(logratio))

p13 <- bigram_logratio %>% group_by(logratio < 0) %>% top_n(15, abs(logratio)) %>% ungroup() %>% mutate(bigram = reorder(bigram, logratio)) %>% ggplot(aes(bigram, logratio, fill = logratio < 0)) + geom_col() + coord_flip() + ylab("log odds ratio (Donald/Ivanka)") + scale_fill_discrete(name = "", labels = c("Donald", "Ivanka")) + ggtitle("Top 15 Most Distinctive Bigrams for Donald v.s. Ivanka Trump")


### Bigrams by device, needs further cleaning to yield more interesting results
bigram_tf_idf_D <- bigrams_united %>% filter(person == "Donald") %>% count(persondevice, bigram) %>% bind_tf_idf(bigram, persondevice, n) %>% arrange(desc(tf_idf)) %>% mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(persondevice) %>% top_n(15) %>% ungroup
bigram_tf_idf_I <- bigrams_united %>% filter(person == "Ivanka") %>% count(persondevice, bigram) %>% bind_tf_idf(bigram, persondevice, n) %>% arrange(desc(tf_idf)) %>% mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% group_by(persondevice) %>% top_n(15) %>% ungroup

p14 <- ggplot(bigram_tf_idf_D, aes(bigram, tf_idf, fill = persondevice)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + facet_wrap(~persondevice, ncol = 2, scales = "free") + coord_flip()
p15 <- ggplot(bigram_tf_idf_I, aes(bigram, tf_idf, fill = persondevice)) + geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + facet_wrap(~persondevice, ncol = 2, scales = "free") + coord_flip()



### "Crooked Hillary"
bigrams_separated %>% filter(word2 == "hillary") %>% count(word1, word2, sort = TRUE)
# Source: local data frame [127 x 3]
# Groups: word1 [127]
#
#      word1   word2     n
#      <chr>   <chr> <int>
# 1  crooked hillary   199
# 2     beat hillary    28
# 3     that hillary    14
# 4      for hillary    11
# 5     than hillary     6
# 6       of hillary     5
# 7       to hillary     5
# 8    about hillary     4
# 9  against hillary     4
# 10 beating hillary     4
# # ... with 117 more rows



### Graph for bigram

bigram_graph_DT <- bigrams_separated %>% filter(person == "Donald") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% count(word1, word2, sort = TRUE) %>% filter(n > 40) %>% graph_from_data_frame()

set.seed(4242)
p16 <- ggraph(bigram_graph_DT, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) + geom_node_point(color = "lightblue", size = 5) + geom_node_text(aes(label = name), vjust = 1, hjust = 1) + ggtitle("Directed graph of common bigrams in Donald Trump's tweets") + xlim(8.5, 25)

bigram_graph_IT <- bigrams_separated %>% filter(person == "Ivanka") %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% count(word1, word2, sort = TRUE) %>% filter(n > 20) %>% graph_from_data_frame()

set.seed(4242)
p17 <- ggraph(bigram_graph_IT, layout = "fr") + geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = grid::arrow(type = "closed", length = unit(.15, "inches")), end_cap = circle(.07, 'inches')) + geom_node_point(color = "lightblue", size = 5) + geom_node_text(aes(label = name), vjust = 1, hjust = 1) + ggtitle("Directed graph of common bigrams in Ivanka Trump's tweets") + xlim(8, 22)



###### Emotions of words
###### NRC Emotion lexicon: Anger, anticipation, disgust, fear, joy, sadness, surprise, trust
###### http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# custom_stop_words <- bind_rows(data_frame(word = c("trump", "mar", "miss", "funny", "fall"), lexicon = c("custom")), stop_words)
# %>% anti_join(custom_stop_words)

###### Over time

donaldsentiment_emo <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% left_join(get_sentiments("nrc")) %>% mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>% count(device, index = floor_date(timestamp, "month"), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(angerp = anger / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(anticipationp = anticipation / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(disgustp = disgust / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(fearp = fear / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(joyp = joy / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(sadnessp = sadness / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(surprisep = surprise / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(trustp = trust / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(neutralp = neutral / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral))


### Absolute counts
emo1 <- ggplot(donaldsentiment_emo, aes(index, anger)) + geom_col(show.legend = FALSE, fill = "red3") + facet_wrap(~device, ncol = 1) + ggtitle("Anger") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo2 <- ggplot(donaldsentiment_emo, aes(index, anticipation)) + geom_col(show.legend = FALSE, fill = "chartreuse3") + facet_wrap(~device, ncol = 1) + ggtitle("Anticipation") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo3 <- ggplot(donaldsentiment_emo, aes(index, disgust)) + geom_col(show.legend = FALSE, fill = "darkorange3") + facet_wrap(~device, ncol = 1) + ggtitle("Disgust") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo4 <- ggplot(donaldsentiment_emo, aes(index, fear)) + geom_col(show.legend = FALSE, fill = "slategrey") + facet_wrap(~device, ncol = 1) + ggtitle("Fear") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo5 <- ggplot(donaldsentiment_emo, aes(index, joy)) + geom_col(show.legend = FALSE, fill = "orange") + facet_wrap(~device, ncol = 1) + ggtitle("Joy") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo6 <- ggplot(donaldsentiment_emo, aes(index, sadness)) + geom_col(show.legend = FALSE, fill = "blue4") + facet_wrap(~device, ncol = 1) + ggtitle("Sadness") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo7 <- ggplot(donaldsentiment_emo, aes(index, surprise)) + geom_col(show.legend = FALSE, fill = "hotpink") + facet_wrap(~device, ncol = 1) + ggtitle("Surprise") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)

emo8 <- ggplot(donaldsentiment_emo, aes(index, trust)) + geom_col(show.legend = FALSE, fill = "darkolivegreen4") + facet_wrap(~device, ncol = 1) + ggtitle("Trust") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 310)


### Percentages
emop1 <- ggplot(donaldsentiment_emo, aes(index, angerp)) + geom_col(show.legend = FALSE, fill = "red3") + facet_wrap(~device, ncol = 1) + ggtitle("Anger") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop2 <- ggplot(donaldsentiment_emo, aes(index, anticipationp)) + geom_col(show.legend = FALSE, fill = "chartreuse3") + facet_wrap(~device, ncol = 1) + ggtitle("Anticipation") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop3 <- ggplot(donaldsentiment_emo, aes(index, disgustp)) + geom_col(show.legend = FALSE, fill = "darkorange3") + facet_wrap(~device, ncol = 1) + ggtitle("Disgust") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop4 <- ggplot(donaldsentiment_emo, aes(index, fearp)) + geom_col(show.legend = FALSE, fill = "slategrey") + facet_wrap(~device, ncol = 1) + ggtitle("Fear") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop5 <- ggplot(donaldsentiment_emo, aes(index, joyp)) + geom_col(show.legend = FALSE, fill = "orange") + facet_wrap(~device, ncol = 1) + ggtitle("Joy") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop6 <- ggplot(donaldsentiment_emo, aes(index, sadnessp)) + geom_col(show.legend = FALSE, fill = "blue4") + facet_wrap(~device, ncol = 1) + ggtitle("Sadness") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop7 <- ggplot(donaldsentiment_emo, aes(index, surprisep)) + geom_col(show.legend = FALSE, fill = "hotpink") + facet_wrap(~device, ncol = 1) + ggtitle("Surprise") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)

emop8 <- ggplot(donaldsentiment_emo, aes(index, trustp)) + geom_col(show.legend = FALSE, fill = "darkolivegreen4") + facet_wrap(~device, ncol = 1) + ggtitle("Trust") + scale_x_datetime(breaks = date_breaks("3 years"), minor_breaks = date_breaks("6 months"), labels = date_format("%Y")) + labs(x = NULL, y = NULL) + ylim(0, 0.25)


chinesetime2 <- c("子", rep("丑", 2), rep("寅", 2), rep("卯", 2), rep("辰", 2), rep("巳", 2), rep("午", 2), rep("未", 2), rep("申", 2), rep("酉", 2), rep("戌", 2), rep("亥", 2), "子")
uniquechinesetime2 <- c("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")

donaldemop_ct <- tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% left_join(get_sentiments("nrc")) %>% mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>% count(device, index = factor(chinesetime2[hour(timestamp)+1], levels = uniquechinesetime2, ordered = TRUE), sentiment) %>% spread(sentiment, n, fill = 0) %>% mutate(angerp = anger / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(anticipationp = anticipation / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(disgustp = disgust / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(fearp = fear / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(joyp = joy / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(sadnessp = sadness / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(surprisep = surprise / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(trustp = trust / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral)) %>% mutate(neutralp = neutral / (anger + anticipation + disgust + fear + joy + sadness + surprise + trust + neutral))

emop_ct1 <- ggplot(donaldemop_ct, aes(index, angerp)) + geom_col(show.legend = FALSE, fill = "red3") + facet_wrap(~device, ncol = 1) + ggtitle("Anger") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct2 <- ggplot(donaldemop_ct, aes(index, anticipationp)) + geom_col(show.legend = FALSE, fill = "chartreuse3") + facet_wrap(~device, ncol = 1) + ggtitle("Anticipation") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct3 <- ggplot(donaldemop_ct, aes(index, disgustp)) + geom_col(show.legend = FALSE, fill = "darkorange3") + facet_wrap(~device, ncol = 1) + ggtitle("Disgust") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct4 <- ggplot(donaldemop_ct, aes(index, fearp)) + geom_col(show.legend = FALSE, fill = "slategrey") + facet_wrap(~device, ncol = 1) + ggtitle("Fear") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct5 <- ggplot(donaldemop_ct, aes(index, joyp)) + geom_col(show.legend = FALSE, fill = "orange") + facet_wrap(~device, ncol = 1) + ggtitle("Joy") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct6 <- ggplot(donaldemop_ct, aes(index, sadnessp)) + geom_col(show.legend = FALSE, fill = "blue4") + facet_wrap(~device, ncol = 1) + ggtitle("Sadness") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct7 <- ggplot(donaldemop_ct, aes(index, surprisep)) + geom_col(show.legend = FALSE, fill = "hotpink") + facet_wrap(~device, ncol = 1) + ggtitle("Surprise") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)

emop_ct8 <- ggplot(donaldemop_ct, aes(index, trustp)) + geom_col(show.legend = FALSE, fill = "darkolivegreen4") + facet_wrap(~device, ncol = 1) + ggtitle("Trust") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(family = "SimSun", size = 5)) + ylim(0, 0.25)








################### Visualization and interpretation

#### Vis of word-freq-based analysis

pdf("TrumpWFBVis.pdf", width = 12)

multiplot(p1, p2, p3, p4, cols=2)

print(p5) ### Also check the cor.test results -- cor(DTiOS, DTWeb) ~= 0.48,  cor(DTAndroid, DTWeb) ~= 0.70
print(p6) ### Also check the cor.test results -- cor(ITiOS, ITWeb) ~= 0.69,  cor(ITOther, ITWeb) ~= 0.57
#
# The fact that cor(ITOther, ITWeb) is quite low is probably because "otherwise" is a wild mixture of Instagram posts, etc. But even this correlation is higher than cor(DTiOS, DTWeb)!

multiplot(emo1, emo2, emo3, emo4, emo5, emo6, emo7, emo8, cols=8)
multiplot(emop1, emop2, emop3, emop4, emop5, emop6, emop7, emop8, cols=8)

dev.off()

#### Vis of sentiment analysis

pdf("TrumpSentVis.pdf", width = 10)

print(p7) ### Donald is more negative on Android
print(p8) ### Ivanka is almost always positive

print(p9)
print(p10)

print(p13) ### Ivanka: 5 6 7 tips; Donald: Crooked Hillary
print(p14) ### Needs further cleaning to yield more interesting results
print(p15) ### Needs further cleaning to yield more interesting results

print(p16)
print(p17)

dev.off()

cairo_pdf("TrumpSentVis2-dt.pdf", width = 10)
print(p11) # Weirdly Trump tends to have a positivity boost in 申時 (15-17) on iOS and Web
dev.off()

cairo_pdf("TrumpSentVis2-it.pdf", width = 10)
print(p12)
dev.off()

cairo_pdf("TrumpSentVis2-dt2.pdf", width = 10)
print(p11_alt) # Weirdly Trump tends to have a positivity boost in 申時 (15-17) on iOS and Web
dev.off()

cairo_pdf("TrumpSentVis2-it2.pdf", width = 10)
print(p12_alt)
dev.off()

cairo_pdf("TrumpSentVis-emotion.pdf", width = 14)
multiplot(emop_ct1, emop_ct2, emop_ct3, emop_ct4, emop_ct5, emop_ct6, emop_ct7, emop_ct8, cols=8) # No apparent trend
dev.off()

pdf("ComparisonClouds.pdf")
### These should be presented along with the old word clouds and should not replace them!
### The two sets of word clouds present very different information.
###
### The size of a word’s text in these word clouds is in proportion to its frequency within its sentiment.
### We can use this visualization to see the most important positive and negative words.
### The sizes of the words are NOT comparable across sentiments.

### Duplicated codes!
tidy_tweets %>% filter(person == "Donald") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

tidy_tweets %>% filter(person == "Ivanka") %>% anti_join(custom_stop_words) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

dev.off()







save(file = "tweets-sentVis.RData", list = c("bigram_counts", "bigram_graph_DT", "bigram_graph_IT", "bigram_logratio", "bigram_tf_idf_D", "bigram_tf_idf_I", "bigrams_filtered", "bigrams_separated", "bigrams_united", "bing_word_counts_D", "bing_word_counts_I", "chinesetime", "chinesetime2", "chinesetime3", "custom_stop_words", "donaldemop_ct", "donaldsentiment", "donaldsentiment_ct", "donaldsentiment_emo", "freqbyDonaldDevice", "freqbyIvankaDevice", "ivankasentiment", "ivankasentiment_ct", "tweets_bigrams", "uniquechinesetime", "uniquechinesetime2", "uniquechinesetime3"))

