#!/usr/bin/env Rscript
# The data are available on the Web at 
# http://www.stat.berkeley.edu/users/nolan/stat133/data/stateoftheunion1790-2012.txt.zip

# Author (of the whole script, both parts A and B): Yiming Li
# UID: 24125490

library(ape) # Needs library "ape" for as.phylo and plot.phylo
# install.packages("Rstem", repos = "http://www.omegahat.org/R")
library(Rstem)

### Use readLines() to read this plain text file into R and assign it to speeches
speeches <- readLines("stateoftheunion1790-2012.txt")

### Use grep and regular expressions to determine which 
### lines of speeches have the three asterisks in them. 
breaks <- grep("^\\*\\*\\*$", speeches)

### Use breaks to identify the elements in speeches (which correspond to lines in the file
### that have the date of the speech.  
### Place these dates in a character vector called tempDates.
tempDates <- speeches[breaks+4]
	
### Use regexpr to identify the location of the year in 
### each tempDates. Then use substr() to extract the year 
### Convert the year to numeric. Call it speechYr
yrLocs <- regexpr(",", tempDates)
speechYr <- as.numeric(substr(tempDates, yrLocs+2, yrLocs+6))

### Use gsub to extract the month of the speech from tempDates. 
### Convert the month to numeric. 
### Assign it to a vector called speechMo.
speechMo <- gsub("(^[[:alpha:]]+) .*$", "\\1", tempDates)
speechMo <- unname(sapply(speechMo, function(x) {
	switch(x,
		January = 1,
		February = 2,
		March = 3,
		April = 4,
		May = 5,
		June = 6,
		July = 7,
		August = 8,
		September = 9,
		October = 10,
		November = 11,
		December = 12,
		NA)
}))

###  Use breaks to extract the name of the president from speeches
###  Assign the names to the character vector presidents. 
presidents <- speeches[breaks+3]

### Cut up the speeches vector according to speech, 
### and place each speech into an element of the list.
### Call the list speechesL.
### Each element of the list should be a character vector
### Each element in the character vector should correspond
### to a sentence in the speech (not a line in the file).
speechesLL <- sapply(1:length(breaks), function(i) {
	if (i < length(breaks)) {
		paste(speeches[(breaks[i]+6):(breaks[i+1]-2)], collapse = " ")
	} else {
		paste(speeches[(breaks[i]+6):(length(speeches)-1)], collapse = " ")
	}
})

speechesL <- lapply(speechesLL, function(x) {
	unlist(strsplit(x, "(?<!(Mr|Ms|Dr))(?<!(Mrs|Sen|Sgt))([[:space:]]*[\\.\\?!]+ [[:space:]]*)(?=[A-Z])|[\\.?!]+$", perl=T))
})

#### Person B: Preparation
### You will work with the list called speechesL that person A created.
### For each speech, your will create a word vector that 
### counts the number of occurrences of each word used in the \
### speech.
### The following steps will help you create a word vector for 
### each speech:
	
###  Complete the function speechToWords() shown below
speechToWords = function(sentences) {
	# sentences is character vector of sentences for each speech
	# Use gsub to eliminate apostrophes and numbers 
	# Drop the phrase [Applause]
	# Turn characters to lower case.
	speechMo <- tolower(gsub("[[:digit:]]|'|(\\[Applause\\])", "", sentences))
	
	# Use strsplit to split the text up by blanks and punctuation 
	# Unlist the return value
	speechMo <- unlist(strsplit(speechMo, "[[:space:][:punct:]]+"))
	
	# Drop any empty words 
	speechMo <- speechMo[speechMo != ""]
	
	# Use wordStem() to stem the words
	# return a character vector of all words in the speech
	return(wordStem(speechMo))
}

# Apply the speechToWords() to each speech in speechesL
# Assign it to the list speechWords
speechWords <- lapply(speechesL, speechToWords)
	
### Unlist the return value and use unique() 
### to get the bag of words.  
### Alphabetize the bag of words, and call it uniqueWords
uniqueWords <- sort(unique(unlist(speechWords)))

### For each speech create a word vector the same length as uniqueWords
### Think about vector operations to do this. Consider the table function,
### and assignment/indexing vectors using names. Ultimately, create a
### matrix with the number of columns equaling the number of speeches and
### the number of rows equaling the number of words in uniqueWords.
emptyVec <- rep(0, length(uniqueWords))
names(emptyVec) <- uniqueWords

wordVecs <- lapply(speechWords, function(x){
	emptyVec[names(table(x))] <- table(x)
	emptyVec
})

### Convert the list from the lapply into a matrix
wordMat <- matrix(unlist(wordVecs), 
                 ncol  = length(wordVecs), 
                 byrow = F)
	
####################
### Person A: Explore

### Now create the final data structure that you will use 
### to analyze the data. I have provided you with a starting data frame, 
### called speechesDF. It has two variables Pres, which 
### holds the president's name, and party,
### which provides the political party of the president.
### It is available in the rda file on bspace

### To this data frame add the following variables:
# yr - year of the speech (numeric)
# month - month of the speech (numeric)
# words - number of words in the speech (you will use person B's work)
# chars - number of letters in the speech
# sent - number of sentences in the speech
# short - proportion of words that are four or fewer letters

speechesDF$yr <- speechYr
speechesDF$month <- speechMo
speechesDF$words <- sapply(1:nrow(speechesDF), function(i) sum(wordMat[,i]))
speechesDF$chars <- sapply(1:nrow(speechesDF), function(i) sum(wordMat[,i]*nchar(uniqueWords)))
speechesDF$sent <- sapply(speechesL, function(x) length(x))
speechesDF$short <- sapply(1:nrow(speechesDF), function(i) sum(wordMat[,i]*as.numeric(nchar(uniqueWords) <= 4))/sum(wordMat[,i]))

### Explore the speeches using this data frame.  
### Make one plot that provides an interesting comparison 
### of the speeches/presidents.
pdf("HW6.pdf", width = 10)

pbcol <- c(R="#FF0000", D="#0000FF", W="#CD853F", U="#0000FF", F="#000000", DR="#800080")
times <- speechesDF$yr + speechesDF$month / 12
symbols(times, speechesDF$sent, circles = speechesDF$words, bg = pbcol[as.character(speechesDF$party)], inches = 0.3, main = "Relationship Between Speech Length and Time\nLength of speech seems to rise and then fall back as time forwards.\nDemocrats tend to be more verbose than their contemporary republicans.", xlab = "Year", ylab = "Number of sentences in the speech", cex.main = 0.9, sub = "*Radii of circles are proportional to the number of words in the speeches.", cex.sub = 0.8)
i1 <- which(speechesDF$short == max(speechesDF$short)) # Uses many short words
m1 <- paste(speechesDF$Pres[i1], " (", speechesDF$party[i1], ")\nHighest proportion of short words", sep = "")
i2 <- which(speechesDF$short == min(speechesDF$short)) # Uses few short words
m2 <- paste(speechesDF$Pres[i2], " (", speechesDF$party[i2], ")\nLowest proportion of short words", sep = "")
i3 <- which(speechesDF$chars == max(speechesDF$chars)) # Uses most characters
m3 <- paste(speechesDF$Pres[i3], " (", speechesDF$party[i3], ")\nMost characters", sep = "")
text(times[c(i1,i2,i3)], speechesDF$sent[c(i1,i2,i3)], labels = c(m1,m2,m3), cex = 0.8)
legend(1770, 1450, c("Political Party", "Republican", "Democratic", "Whig", "Federalist / Independent", "Democratic-Republican"), fill = c("white", "#FF0000", "#0000FF", "#CD853F", "#000000", "#800080"), border = "white", cex = 0.75)

##############
### Person B: Distances between speeches
### In this final stage of analysis, compute the 
### distance between presidents using the Shannon-Jensen (SJ) metric 
### Use computeSJDistance() to compute the distance between 
### presidents. 
source("computeSJDistance.R")
### To do this, first add together the word vectors for 
### the speeches made by the same president.
### The resulting matrix should be called presMat
presInd <- sapply(1:length(unique(presidents)), function(i) which(presidents==unique(presidents)[i])[1])
presMat <- lapply(1:length(presInd), function(i) {
	if (i < length(presInd)) {
		if (presInd[i]==(presInd[i+1]-1)) {
			wordMat[,presInd[i]]
		} else {
			apply(wordMat[,(presInd[i]):(presInd[i+1]-1)],1,sum)
		}
	} else {
		apply(wordMat[,(presInd[i]):(ncol(wordMat))],1,sum)
	}
})
presMat <- matrix(unlist(presMat), byrow = F, nrow = nrow(wordMat))

### Next compute the number of presidents that used each word
### This is our "document frequency". Call it df
df <- sapply(1:nrow(presMat), function(i) sum(presMat[i,] != 0))

### Call computeSJDistance() , passing it the presMat, df,
### and the unique words
### The return value is a matrix, call it presDist
presDist = computeSJDistance(tf = presMat, df = df, 
                             terms = uniqueWords)
rownames(presDist) = unique(presidents)

### Use multidimensional scaling to produce a visualization 
### of the results.
### Use labels, lines, color, etc. to make the 
### plot as informative as possible. 
presCol <- pbcol[as.vector(read.table(pipe("cut -f1 Parties.txt"))$V1)]
names(presCol) <- as.vector(read.table(pipe("cut -f2 Parties.txt"))$V1)
# Roughly categorize the presidents by half a decade
# i.e. By the time of the first speech given, inclusively,
# 1. 1790 -- 1849
# 2. 1850 -- 1897
# 3. 1901 -- 1946
# 4. 1953 -- 2009
yrCat <- speechesDF$yr[presInd]
names(yrCat) <- speechesDF$Pres[presInd]
yrCat <- ifelse(yrCat <= 1849, 1, ifelse(yrCat <= 1897, 2, ifelse(yrCat <= 1946, 3, 4)))
yrCols <- c("#006400", "#808000", "#00FF00", "#00FF7F")
yrColsTrans <- c(rgb(0, 100, 0, alpha = 110, maxColorValue = 255), rgb(128, 128, 0, alpha = 110, maxColorValue = 255), rgb(0, 255, 0, alpha = 110, maxColorValue = 255), rgb(0, 255, 127, alpha = 110, maxColorValue = 255))

mds <- cmdscale(presDist)
mdsLabels <- sapply(1:nrow(mds), function(i) {
	if (length(unlist(strsplit(rownames(mds)[i], " "))) == 2) {
		gsub("^([A-Z])[[:alpha:]]+.* ([[:alpha:]]+)$", "\\2 \\1", rownames(mds)[i])
	} else {
		gsub("^([A-Z])[[:alpha:]]+.* ([[:alpha:]]).* ([[:alpha:]]+)$", "\\3 \\1\\2", rownames(mds)[i])
	}
})
# "Lou Reed" would be rewritten as "Reed L" and "Kurt D. Cobain" as "Cobain KD" to save space
# Two "wrong" cases:
# "Martin van Buren" as "Buren Mv" and "George H.W. Bush" as "Bush GH"

plot(mds, main = "Distances Between Presidents' Speeches -- Multi-Dimensional Scaling\nPresidents cluster by era more than by parties; Obama and Bush Jr. seems to be more distant.", xlab = "Coordinate 1", ylab = "Coordinate 2", type = "n", xlim = c(-0.04, 0.07), cex.main = 0.85)
rect(mds[,1]-0.008, mds[,2]-0.002, mds[,1]+0.008, mds[,2]+0.002, col = yrColsTrans[yrCat[rownames(mds)]], border = NA)
text(mds[,1], mds[,2], labels = mdsLabels, cex = 0.7, col = presCol[rownames(mds)])
legend(-0.003, 0.038, c("Political Party", "Republican", "Democratic", "Whig", "Federalist / Independent", "Democratic-Republican"), fill = c("white", "#FF0000", "#0000FF", "#CD853F", "#000000", "#800080"), border = "white", cex = 0.75)
legend(-0.003, 0.021, c("Era", "1790 -- 1850", "1850 -- 1900", "1900 -- 1950", "1950 -- 2000s"), fill = c("white", "#006400", "#808000", "#00FF00", "#00FF7F"), border = "white", cex = 0.75)

### Use hierarchical clustering to produce a visualization 
### of the results.
hc <- hclust(as.dist(presDist), method = "complete")
# Use complete linkage to avoid chaining

clucols4 <- c("#8B0000AA", "#FF4500AA", "#008000AA", "#4B0082AA")
clucols5 <- c("#8B0000AA", "#FF1493AA", "#FF4500AA", "#008000AA", "#4B0082AA")
clucols6 <- c("#8B0000AA", "#FF1493AA", "#FF4500AA", "#008000AA", "#1E90FFAA", "#4B0082AA")
clucols7 <- c("#8B0000AA", "#FF1493AA", "#FF4500AA", "#FFFF0088", "#008000AA", "#1E90FFAA", "#4B0082AA")
clus4 <- cutree(hc, 4) # cutting dendrogram in 4 clusters
clus5 <- cutree(hc, 5) # cutting dendrogram in 5 clusters
clus6 <- cutree(hc, 6) # cutting dendrogram in 6 clusters
clus7 <- cutree(hc, 7) # cutting dendrogram in 7 clusters
n <- length(hc$labels)

par(mar = c(1,1,1,1)) # bottom, left, top, right
plot(as.phylo(hc), type="cladogram", tip.color=presCol[hc$labels], cex = 0.8, x.lim = c(-0.015,0.125), y.lim = c(-1, 45), edge.color = "#000000AA") # cladogram
rect(rep(0.108,time=n), 1:n-0.25, rep(0.113,time=n), 1:n+0.25, col = clucols4[clus4[hc$labels[hc$order]]], border = NA)
rect(rep(0.102,time=n), 1:n-0.25, rep(0.107,time=n), 1:n+0.25, col = clucols5[clus5[hc$labels[hc$order]]], border = NA)
rect(rep(0.096,time=n), 1:n-0.25, rep(0.101,time=n), 1:n+0.25, col = clucols6[clus6[hc$labels[hc$order]]], border = NA)
rect(rep(0.09,time=n), 1:n-0.25, rep(0.095,time=n), 1:n+0.25, col = clucols7[clus7[hc$labels[hc$order]]], border = NA)
rect(rep(0.117,time=n), 1:n-0.25, rep(0.122,time=n), 1:n+0.25, col = yrCols[yrCat[hc$order]], border = NA)
text(c(0.0925,0.0985,0.1045,0.1105), rep(0.1,times=4), labels = c("7","6","5","4"), cex = 0.8, col = "#000000AA")
text(0.055, 45.5, labels = "Cladogram of Presidents According to Speeches Words Choice", font = 2, cex = 1.2)
text(0.055, 43.5, labels = "Presidents cluster by era more in the larger clades; presidents in a small clade tend to be of the same party.\nBush Jr. is an anomalous president, who always has his own cluster when we cut the tree into several clusters.", cex = 0.8)
text(c(0.1015,0.1195), c(-1,-1), labels = c("By Clusters", "By Era"), cex = 0.8, col = "#000000AA")
legend(-0.02, 41, c("Political Party", "Republican", "Democratic", "Whig", "Federalist / Independent", "Democratic-Republican"), fill = c("white", "#FF0000", "#0000FF", "#CD853F", "#000000", "#800080"), border = "white", cex = 0.75)
legend(-0.02, 32, c("Era", "1790 -- 1850", "1850 -- 1900", "1900 -- 1950", "1950 -- 2000s"), fill = c("white", "#006400", "#808000", "#00FF00", "#00FF7F"), border = "white", cex = 0.75)

dev.off()
