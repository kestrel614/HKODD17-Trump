#!/usr/bin/env Rscript

### Author: Yiming Li

### For Yue's visualization

load("tweets.RData")

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
