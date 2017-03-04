#!/usr/bin/env Rscript

### Author: Yiming Li

#### Read in the data

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


load("tweets.RData")
#### Create word count matrix

library(stringr)
# R CMD INSTALL SnowballC_0.5.1.tar.gz
# library(SnowballC)

cleanTweets <- function(x) {
	cleaned <- paste(x, collapse = " ")
	
	cleaned <- gsub("'", '', cleaned)
	cleaned <- gsub("\"", '', cleaned)
	cleaned <- gsub("\\s+", " ", str_trim(cleaned))
	
	# Extract URLs
	urls <- unlist(str_extract_all(cleaned, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"))
	urls2 <- unlist(str_extract_all(cleaned, "www(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"))
	urls <- c(urls, urls2) ## Some broken URLs
	cleaned <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", '', cleaned)
	cleaned <- gsub("www(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", '', cleaned)
	
	cleaned <- tolower(cleaned)
	
	cleaned <- gsub("--donald j. trump", "bydjtrump", cleaned)
	cleaned <- gsub("–donald j. trump", "bydjtrump", cleaned)
	cleaned <- gsub("– donald trump", "bydjtrump", cleaned)
	#by donald j. trump
		
	# Extract @-ed accounts
	referred <- unlist(str_extract_all(cleaned, "@(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"))
	cleaned <- gsub("@(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", '', cleaned)
	referred <- gsub("[[:punct:]]", "", referred)
	# table(referred)[table(referred) > 10]
	
	# Extract hashtags
	hashtags <- unlist(str_extract_all(cleaned, "#(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"))
	cleaned <- gsub("#(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", '', cleaned)
	# table(referred)[table(referred) > 10]
	
	cleaned <- gsub("[[:punct:]]", "", cleaned)
	
	cleaned <- gsub("donald trump", "donaldtrump", cleaned)
	cleaned <- gsub("donald j trump", "donaldtrump", cleaned)
	
	cleaned <- gsub("\\s+", " ", str_trim(cleaned))
	
	
	###### Into words
	cleanedV <- unlist(strsplit(cleaned, " "))
	# Drop any empty words 
	cleanedV <- cleanedV[cleanedV != ""]	
	
	# Use wordStem() to stem the words
	# return a character vector of all words in the speech
	# cleanedVStems <- wordStem(cleanedV)	
	
	return(list(urls = urls, referred = referred, cleaned = cleaned, cleanedV = cleanedV))
}

trumpTweetsV <- cleanTweets(trumpTweets$text)
ivankaTweetsV <- cleanTweets(ivankaTweets$text)



write.table(table(trumpTweetsV$cleanedV), file = "trumpWordCounts.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(table(ivankaTweetsV$cleanedV), file = "ivankaWordCounts.txt", sep = "\t", row.names = FALSE, quote = FALSE)

save(file = "tweets2.RData", list = ls())


trumpTweetsV$cleanedV



