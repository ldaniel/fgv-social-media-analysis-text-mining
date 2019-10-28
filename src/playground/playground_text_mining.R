# loading required libraries --------------------------------------------------

# general libraries
library(dplyr)
library(ggplot2)

# libraries for text mining
library(tm)
library(SnowballC)
library(wordcloud)

# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")
source("./src/datapreparation/step_01_config_environment.R")

# create corpus ---------------------------------------------------------------
docs <- Corpus(DirSource("../../data/raw/"))
inspect(docs)

# inspect a particular document
writeLines(as.character(docs[[1]]))

# start preprocessing
toSpace <- content_transformer((function(x, pattern) {
  return (gsub(pattern, " ", x))
}))

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " _")

# good practice to check after each step
writeLines(as.character(docs[[1]]))

# remove punctuation
docs <- tm_map(docs, removePunctuation)

# transform to lower case
docs <- tm_map(docs, content_transformer(tolower))

# stript digits
docs <- tm_map(docs, removeNumbers)

# remove stopwords from standard stopword list (how to check this? how to add your own?)
docs <- tm_map(docs, removeWords, stopwords("english"))

# strip whitespace
docs <- tm_map(docs, stripWhitespace)

# inspect document
writeLines(as.character(docs[[1]]))

# need SnowballC library for stemming -----------------------------------------
docs <- tm_map(docs, stemDocument)

# inspect document
writeLines(as.character(docs[[1]]))

# some clean up
docs <- tm_map(docs, content_transformer(gsub), pattern = "organiz", replace = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "organis", replace = "organ")
docs <- tm_map(docs, content_transformer(gsub), pattern = "andgovern", replace = "govern")
docs <- tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replace = "enterprise")
docs <- tm_map(docs, content_transformer(gsub), pattern = "team-", replace = "team")

# inspect document
writeLines(as.character(docs[[1]]))

# create document-term matrix
dtm <- DocumentTermMatrix(docs)

# inspect segment of document term matrix
inspect(dtm[1:1, 5:10])

# collapse matrix by summing over columns- this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))

# length should be total number of terms
length(freq)

# create sort order (asc)
ord <- order(freq, decreasing = TRUE)

# inspect most frequently occurring terms
freq[head(ord)]

# inspect least frequently occurring terms
freq[tail(ord)]

# remove very frequent and very rare words
#dtmr <- DocumentTermMatrix(docs, control = list(wordLengths=c(4, 20), bounds = list(global = c(3, 27))))
dtmr <- DocumentTermMatrix(docs, control = list(wordLengths=c(4, 20)))

freqr <- colSums(as.matrix(dtmr))

# lngth should be total number or terms
length(freqr)

# create sort order (asc)
ordr <- order(freqr, decreasing = TRUE)

# inspect most frequently occurring terms
freqr[head(ordr)]

# inspect least frequently occurring terms
freqr[tail(ordr)]

# list most frequent terms, lower bound specified as second argument
findFreqTerms(dtmr, lowfreq = 8)

# correlations
findAssocs(dtmr, "buddha", 0.2)
findAssocs(dtmr, "women", 0.6)
findAssocs(dtmr, "system", 0.6)

# histogram
wf = data.frame(term = names(freqr), occurrences = freqr)

p <- ggplot(subset(wf, freqr > 8), aes(term, occurrences))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# wordcount

# setting the same seed each time ensures consistent look across clouds
set.seed(42)

# limit words by specifying min frequency
wordcloud(names(freqr), freq, min.freq = 70)

# ... and color
wordcloud(names(freqr), freq, min.freq = 70, colors = brewer.pal(6, "Dark2"))
