## Processing stage

library(tm)
moldova <- read.csv("Moldova_1.csv")

library("tm")
moldovatext <- Corpus(VectorSource(moldova$"Your.experience"))
moldovatext <- tm_map(moldovatext, removePunctuation)

for(j in seq(moldovatext))   
{   
  moldovatext[[j]] <- gsub("/", " ", moldovatext[[j]])   
  moldovatext[[j]] <- gsub("@", " ", moldovatext[[j]])   
  moldovatext[[j]] <- gsub("\\|", " ", moldovatext[[j]])   
}  

moldovatext <- tm_map(moldovatext, removeNumbers)  
moldovatext <- tm_map(moldovatext, tolower)
moldovatext <- tm_map(moldovatext, removeWords, stopwords("english")) 

library(SnowballC)   
moldovatext <- tm_map(moldovatext, stemDocument) 
moldovatext <- tm_map(moldovatext, stripWhitespace) 

moldovatext <- tm_map(moldovatext, PlainTextDocument) 

## Stage the data

dtm <- DocumentTermMatrix(moldovatext)   
dtm   

tdm <- TermDocumentMatrix(moldovatext)   
tdm   

freq <- colSums(as.matrix(dtm))   
length(freq)  

ord <- order(freq) 

findFreqTerms(dtm, lowfreq=20)

wf <- data.frame(word=names(freq), freq=freq)

wf$word <- factor(wf$word, levels = wf$word[order(wf$freq)])

library(ggplot2)   
p <- ggplot(subset(wf, freq>20), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

library(wordcloud) 
wordcloud(names(freq), freq, min.freq=20)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), random.color = FALSE, colors=brewer.pal(6,"Reds") ) 


dtmss <- removeSparseTerms(dtm, 0.95) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss) 

library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit   

plot(fit, hang=-1)  

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red")

library(fpc)   

d <- dist(t(dtmss), method="euclidean")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

d <- dist(t(dtmss), method="maximum")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

d <- dist(t(dtmss), method="manhattan")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

d <- dist(t(dtmss), method="canberra")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

d <- dist(t(dtmss), method="binary")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

d <- dist(t(dtmss), method="minkowski")   
kfit <- kmeans(d,3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
