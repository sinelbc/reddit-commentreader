# To do:
# - how to ignore hyperlinks (e.g. http//:)
# - robust way to stem words and then restem words
# - possibly store comment, author, timestamp, likes/dislikes into dataframe?

Sys.setlocale(locale="C")
Sys.setenv(NOAWT=TRUE)
library(rJava) #must come *after*  Sys.setenv(NOAWT=TRUE) and Sys.setlocale(locale="C")
library(RWeka) #must come *after*  Sys.setenv(NOAWT=TRUE) and Sys.setlocale(locale="C")
library(RWekajars) #must come *after*  Sys.setenv(NOAWT=TRUE) and Sys.setlocale(locale="C")
library(Snowball) #must come *after*  Sys.setenv(NOAWT=TRUE) and Sys.setlocale(locale="C")
library(stringr)
library(RCurl)
library(XML) 
library(tm) #text mining package
library(wordcloud)
library(RColorBrewer)

#reddit thread xml. Paste link here...
doc <- xmlTreeParse(file="http://www.reddit.com/r/technology/comments/1cn8y9.xml?limit=2000&depth=10",
                    useInternalNodes=TRUE)
#remember .xml at the end is required... 
#limits = max number of comments, 
#depth = how far down to go. 

top <- xmlRoot(doc)
nodes <- getNodeSet(top, "//channel/item/description") #second argument is path to comments from root. See xml file.
comments <- lapply(nodes, function(x) xmlSApply(x, xmlValue)) #xmlValue is the way to get just the text out of the comment.

comments <- as.vector(comments) #comments must be as.vector to be put into corpus.
redditComments.corpus <- Corpus(VectorSource(comments))

cleanCorpus <- function(corpus) { # Performs a variety of cleanup functions to make improve corpus
  require(tm)
  genericWords <- c(stopwords("SMART"), stopwords("english"), "hey") # These junky/filler words that will be removed/ignored from the corpus
  corpus.tmp <- tm_map(corpus, tolower) #makes everything lowercase
  corpus.tmp <- tm_map(corpus.tmp, removeWords, genericWords) #this line actually removes the words specified above
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation) # Removes punctuation marks
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  #corpus.tmp <- tm_map(corpus.tmp, stemDocument) #converts words to "stems." Requires the Wekajars and Snowball packages--sometimes causes problems. Comment this line out if it causes problems.
  #corpus.tmp <- tm_map(corpus.tmp, stemCompletion, dictionary=dictCorpus)
  return(corpus.tmp)
}

redditComments.corpus <- cleanCorpus(redditComments.corpus) #use the cleanCorpus function on your corpus

redditComments.dtm <- TermDocumentMatrix(redditComments.corpus,
                                         control = list(minWordLength = 3) #excludes words shorter than 3 chars
                                         )

ae.m <- as.matrix(redditComments.dtm)
ae.v <- sort(rowSums(ae.m),decreasing=TRUE)
ae.d <- data.frame(word = names(ae.v),freq=ae.v)
table(ae.d$freq)
# display.brewer.all()
wordColors <- brewer.pal(8,"Dark2")
png("commentWords.png", width=2000,height=2000)
wordcloud(ae.d$word,ae.d$freq, scale=c(20,4),min.freq=4,max.words=1000, random.order=FALSE, rot.per=.1, colors=wordColors) # play around with scale, min.freq, and max.words to adjust the cloud.
dev.off()


#redditComments.dtm <- removeSparseTerms(redditComments.dtm, sparse=0.95) # remove rarely occuring terms...speeds things up and simplifies things when the document is big.

findFreqTerms(redditComments.dtm, lowfreq=5)
#findAssocs(redditComments.dtm, 'cat', corlimit=.2)


redditComments.dtm1 <- removeSparseTerms(redditComments.dtm, sparse=0.95) # play around with the sparse term.
redditComments.dtm.df <- as.data.frame(inspect(redditComments.dtm1))
redditComments.dtm.df.scale <- scale(redditComments.dtm.df)
d <- dist(redditComments.dtm.df, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters

#Words higher on the dendrogram are more frequent. Closer together are more associated...
