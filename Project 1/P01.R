library(pdftools)
library(tm)
library(wordcloud)
library(graph)
library(Rgraphviz)
library(igraph)
library(topicmodels)

setwd('D:/R programming runs/Assisgnments/Project 1/MDS503P01')

## Getting all the pdf files using regex from working directory
pdf_files <- list.files(pattern = "pdf$")

## Creating corpus with help of **tm** package
pdf_corp <- Corpus(URISource(pdf_files), readerControl=list(reader = readPDF()))
pdf_corp

## Removing punctuation, stopwords, & converting words to lowercase and then creating term document matrix

pdf_tdm <- TermDocumentMatrix(pdf_corp, control = list(removePunctuation=T, stopwords=T, tolower=T, removeNumbers=T))
inspect(pdf_tdm)

## Getting most frequent used words where the words will be repeated atleast 25 times
pdf.freq <- findFreqTerms(pdf_tdm, lowfreq=2, highfreq=Inf)
pdf.freq


# We convert term-document matrix(sparse matrix) to regular matrix and 
#then with help of wordcloud package, we create word cloud

m <- as.matrix(pdf_tdm)

# Plot word cloud without color
freq <- sort(rowSums(m), decreasing=T)
wordcloud(words=names(freq), freq=freq, min.freq=4, random.order=F)

# Plot word cloud with colors
my_colors <- sample(colors(), 10)
wordcloud(words=names(freq), freq=freq, min.freq=4, random.order=F, colors = my_colors)


# Plot network graph
plot(pdf_tdm, terms=pdf.freq, corThreshold=0.8, weighting=T)


# Getting topic modeling
set.seed(16)
myLda <- LDA(as.DocumentTermMatrix(pdf_tdm), k=5)
terms(myLda, 10)

#Here, k=5 denotes 5 topic & 5 in terms() denotes 10 terms in each of the 10 topics  
#We can observe that the 'social', 'media', 'marketing', 'online', 'network', etc. are most used terms in all of the topics.