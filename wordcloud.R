install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("slam")

library(tm)
library(wordcloud)
library(RColorBrewer)

# Aggregate data



docs <- Corpus(VectorSource(trumpTweetsV$cleaned))
docs <- Corpus(VectorSource(ivankaTweetsV$cleaned))

docs <- tm_map(docs, content_transformer(gsub), 
               pattern = "thanks", replacement = "thank", fixed=TRUE)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("the", "will", "get")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

