#!/usr/bin/env Rscript

### Author: Yiming Li

#### Read in the data and preliminary formatting

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

trumpTweets$year <- as.integer(format(trumpTweets$created_at, "%Y"))
ivankaTweets$year <- as.integer(format(ivankaTweets$created_at, "%Y"))
trumpTweets$month <- as.integer(format(trumpTweets$created_at, "%m"))
ivankaTweets$month <- as.integer(format(ivankaTweets$created_at, "%m"))
trumpTweets$day <- as.integer(format(trumpTweets$created_at, "%d"))
ivankaTweets$day <- as.integer(format(ivankaTweets$created_at, "%d"))

save(file = "tweets.RData", list = ls())



#### Create word count matrix
load("tweets.RData")

library(stringr)
library(qdapRegex)
# R CMD INSTALL SnowballC_0.5.1.tar.gz
library(SnowballC)

cleanTweets <- function(x) {
	
	incompletes <- x[grep("\\(cont\\)", x)] # 446
	x <- x[! 1:length(x) %in% grep("\\(cont\\)", x)]

	# Extract URLs and hashtags
	rm_twitter_n_url <- rm_(pattern=pastex("@rm_twitter_url", "@rm_url"))
	urls <- rm_twitter_n_url(x, extract=TRUE)	
	hashtags <- rm_hash(x, extract=TRUE)
	hashtagsU <- unlist(hashtags)
	hashtagsU <- hashtagsU[!is.na(hashtagsU)]
	
	x <- rm_twitter_n_url(x)
	x <- rm_hash(x)
	
	cleaned <- paste(x, collapse = " ")
	
	cleaned <- gsub("'", '', cleaned)
	cleaned <- gsub("\"", '', cleaned)
	cleaned <- gsub("\\s+", " ", str_trim(cleaned))
		
	cleaned <- tolower(cleaned)
	
	# By donald j. trump
	cleaned <- gsub("--donald j. trump", "bydjtrump", cleaned)
	cleaned <- gsub("–donald j. trump", "bydjtrump", cleaned)
	cleaned <- gsub("– donald trump", "bydjtrump", cleaned)
	
	# Remove possessives ('s)
	cleaned <- gsub("'s", "", cleaned)
		
	# Extract @-ed accounts
	referred <- unlist(str_extract_all(cleaned, "(^|[^@\\w])@(\\w{1,15})\\b"))
	referred <- gsub("[^[:alnum:]@_]", "", referred)
	cleaned <- gsub("(^|[^@\\w])@(\\w{1,15})\\b", "", cleaned)
	# table(referred)[table(referred) > 10]
		
	cleaned <- gsub("&amp;", "and", cleaned)
	cleaned <- gsub("–donald j. trump", "bydjtrump", cleaned)
	
	cleaned <- gsub("[[:punct:]]", " ", cleaned)
	cleaned <- gsub("\\s+", " ", str_trim(cleaned))
	cleaned <- rm_number(cleaned)
	
	cleaned <- gsub("donald trump", "donaldtrump", cleaned)
	cleaned <- gsub("donald j trump", "donaldtrump", cleaned)
	
	cleaned <- gsub("\\s+", " ", str_trim(cleaned))
	
	###### Into words
	cleanedV <- unlist(strsplit(cleaned, " "))
	# Drop any empty words 
	cleanedV <- cleanedV[cleanedV != ""]	
	
	# Use wordStem() to stem the words
	# return a character vector of all words in the speech
	cleanedVStems <- wordStem(cleanedV)	
	
	return(list(incompletes = incompletes, urls = urls, referred = referred, hashtags = hashtags, hashtagsU = hashtagsU, cleaned = cleaned, cleanedV = cleanedV, cleanedVStems = cleanedVStems))
}

trumpTweetsV <- cleanTweets(trumpTweets$text)
ivankaTweetsV <- cleanTweets(ivankaTweets$text)

# > names(trumpTweetsV)
# [1] "incompletes"   "urls"          "referred"      "hashtags"
# [5] "hashtagsU"     "cleaned"       "cleanedV"      "cleanedVStems"

# sort(table(trumpTweetsV$hashtagsU))

# write.table(table(trumpTweetsV$cleanedV)[table(trumpTweetsV$cleanedV) > 500], file = "trumpWordCountsGT500.txt", sep = ":", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(table(ivankaTweetsV$cleanedV), file = "ivankaWordCounts.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# write.table(trumpTweetsV$cleanedV, file = "trumpTweets.txt", sep = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)

save(file = "tweets2.RData", list = c("cleanTweets", "trumpTweetsV", "ivankaTweetsV"))
