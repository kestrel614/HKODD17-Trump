#!/usr/bin/env Rscript

### Author: Yuqiong Li & Yiming Li

# install.packages("tm")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("slam")

library(tm)
library(wordcloud)
library(RColorBrewer)

processCorpus <- function(docs, graphfile = "wordcloud.pdf") {
	
	# Convert the text to lower case
	docs <- tm_map(docs, content_transformer(tolower))

	# Remove numbers
	docs <- tm_map(docs, removeNumbers)
	
	# Remove english common stopwords
	docs <- tm_map(docs, removeWords, stopwords("english"))

	# Remove your own stop word
	# specify your stopwords as a character vector
	docs <- tm_map(docs, removeWords, c("the", "get")) 
	# docs <- tm_map(docs, content_transformer(gsub), pattern = "thanks", replacement = "thank", fixed=TRUE)
	
	# Remove punctuations
	docs <- tm_map(docs, removePunctuation)

	# Eliminate extra white spaces
	docs <- tm_map(docs, stripWhitespace)

	# Text stemming
	docs <- tm_map(docs, stemDocument)
	
	dtm <- TermDocumentMatrix(docs)
	m <- as.matrix(dtm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	# head(d, 10)

	pdf(file = graphfile)
	set.seed(1234)
	wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
	dev.off()
	
	return(list(docs = docs, dtm = dtm, d = d))
	
}

load("tweets2.RData")
trumpCorpus <- processCorpus(Corpus(VectorSource(trumpTweetsV$cleaned)), graphfile = "trumpcloud.pdf")
ivankaCorpus <- processCorpus(Corpus(VectorSource(ivankaTweetsV$cleaned)), graphfile = "ivankacloud.pdf")
save(file = "tweets3.RData", list = c("trumpCorpus", "ivankaCorpus"))
