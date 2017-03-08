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
	
	return(list(urls = urls, referred = referred, hashtags = hashtags, cleaned = cleaned, cleanedV = cleanedV))
}

trumpTweetsV <- cleanTweets(trumpTweets$text)
ivankaTweetsV <- cleanTweets(ivankaTweets$text)

# which(table(trumpTweetsV$hashtags) == max(table(trumpTweetsV$hashtags)))
# #trump2016
#       1543

# sort(table(trumpTweetsV$hashtags))

# write.table(table(trumpTweetsV$cleanedV)[table(trumpTweetsV$cleanedV) > 500], file = "trumpWordCountsGT500.txt", sep = ":", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(table(ivankaTweetsV$cleanedV), file = "ivankaWordCounts.txt", sep = "\t", row.names = FALSE, quote = FALSE)
# write.table(trumpTweetsV$cleanedV, file = "trumpTweets.txt", sep = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)

save(file = "tweets2.RData", list = ls())




### For Yue's visualization

forYue <- data.frame(user = character(0), year = integer(0), chinaWEB = integer(0), chinaAND = integer(0), chinaIOS = integer(0), usWEB = integer(0), usAND = integer(0), usIOS = integer(0), rusWEB = integer(0), rusAND = integer(0), rusIOS = integer(0), totWEB = integer(0), totAND = integer(0), totIOS = integer(0), stringsAsFactors=FALSE)

findWordCount <- function(vec = vec, word = word) {
	if (word %in% names(vec)) {
		return(as.integer(vec[word]))
	} else {
		return(0)
	}
}

for (i in 1:6) {
	
	year <- i + 2011
	trumpTweetsWEB <- trumpTweets$text[trumpTweets$year == year & trumpTweets$source == "Twitter Web Client"]
	trumpTweetsAND <- trumpTweets$text[trumpTweets$year == year & trumpTweets$source == "Twitter for Android"]
	trumpTweetsIOS <- trumpTweets$text[trumpTweets$year == year & (trumpTweets$source == "Twitter for iPhone" | trumpTweets$source == "Twitter for iPad")]
	
	if (length(trumpTweetsWEB) != 0) {
		tmp <- table(cleanTweets(trumpTweetsWEB)$cleanedV)
		totWEB <- sum(tmp)
		chinaWEB <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usWEB <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusWEB <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totWEB <- 0
		chinaWEB <- 0
		usWEB <- 0
		rusWEB <- 0
	}
	
	if (length(trumpTweetsAND) != 0) {
		tmp <- table(cleanTweets(trumpTweetsAND)$cleanedV)
		totAND <- sum(tmp)
		chinaAND <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usAND <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusAND <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totAND <- 0
		chinaAND <- 0
		usAND <- 0
		rusAND <- 0
	}
	
	if (length(trumpTweetsIOS) != 0) {
		tmp <- table(cleanTweets(trumpTweetsIOS)$cleanedV)
		totIOS <- sum(tmp)
		chinaIOS <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usIOS <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusIOS <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totIOS <- 0
		chinaIOS <- 0
		usIOS <- 0
		rusIOS <- 0
	}
	
	forYue <- rbind(forYue, data.frame(user = "donald", year = year, chinaWEB = chinaWEB, chinaAND = chinaAND, chinaIOS = chinaIOS, usWEB = usWEB, usAND = usAND, usIOS = usIOS, rusWEB = rusWEB, rusAND = rusAND, rusIOS = rusIOS, totWEB = totWEB, totAND = totAND, totIOS = totIOS, stringsAsFactors=FALSE))
	
}

for (i in 7:12) {
	
	year <- i - 6 + 2011
	ivankaTweetsWEB <- ivankaTweets$text[ivankaTweets$year == year & ivankaTweets$source == "Twitter Web Client"]
	ivankaTweetsAND <- ivankaTweets$text[ivankaTweets$year == year & ivankaTweets$source == "Twitter for Android"]
	ivankaTweetsIOS <- ivankaTweets$text[ivankaTweets$year == year & (ivankaTweets$source == "Twitter for iPhone" | ivankaTweets$source == "Twitter for iPad")]
	
	if (length(ivankaTweetsWEB) != 0) {
		tmp <- table(cleanTweets(ivankaTweetsWEB)$cleanedV)
		totWEB <- sum(tmp)
		chinaWEB <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usWEB <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusWEB <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totWEB <- 0
		chinaWEB <- 0
		usWEB <- 0
		rusWEB <- 0
	}
	
	if (length(ivankaTweetsAND) != 0) {
		tmp <- table(cleanTweets(ivankaTweetsAND)$cleanedV)
		totAND <- sum(tmp)
		chinaAND <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usAND <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusAND <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totAND <- 0
		chinaAND <- 0
		usAND <- 0
		rusAND <- 0
	}
	
	if (length(ivankaTweetsIOS) != 0) {
		tmp <- table(cleanTweets(ivankaTweetsIOS)$cleanedV)
		totIOS <- sum(tmp)
		chinaIOS <- findWordCount(vec = tmp, word = "china") + findWordCount(vec = tmp, word = "chinas") + findWordCount(vec = tmp, word = "chinese")
		usIOS <- findWordCount(vec = tmp, word = "usas") + findWordCount(vec = tmp, word = "usa") + findWordCount(vec = tmp, word = "america") + findWordCount(vec = tmp, word = "american") + findWordCount(vec = tmp, word = "americas") + findWordCount(vec = tmp, word = "americans")
		rusIOS <- findWordCount(vec = tmp, word = "russia") + findWordCount(vec = tmp, word = "russian") + findWordCount(vec = tmp, word = "russians") + findWordCount(vec = tmp, word = "russias")
	} else {
		totIOS <- 0
		chinaIOS <- 0
		usIOS <- 0
		rusIOS <- 0
	}
	
	forYue <- rbind(forYue, data.frame(user = "ivanka", year = year, chinaWEB = chinaWEB, chinaAND = chinaAND, chinaIOS = chinaIOS, usWEB = usWEB, usAND = usAND, usIOS = usIOS, rusWEB = rusWEB, rusAND = rusAND, rusIOS = rusIOS, totWEB = totWEB, totAND = totAND, totIOS = totIOS, stringsAsFactors=FALSE))
	
}

write.table(forYue, file = "forYue.txt", sep = "\t", row.names = FALSE, quote = FALSE)
