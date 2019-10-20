######### INSTALLING JSON READING ######### 
install.packages("jsonlite")

#########INSTALLING LIBRARIES FOR NLP ######### 
install.packages("tm")
install.packages("rJava") #FIX macOS SUPPORTING ISSUE BY RUNNING "sudo R CMD javareconf" in TERMINAL
install.packages("wordcloud")
install.packages("textir")
install.packages("RWeka")
install.packages("qdap")
install.packages("maptpx")

######### LOADING LIBRARY ######### 
#library("rjson") -- not working
library("jsonlite")
library("tm")
library("rJava")
library("wordcloud")
library("textir")
library("RWeka")
library("qdap")
library("maptpx")

######### BEGIN EXPLORE ######### 
articles <- fromJSON("../datasets/articles.json")
str(articles)

barplot(table(articles$sentiment,articles$source$country),las=2)
hist(articles$sentiment[articles$country=="United States"])


us_only <- articles[articles$country=="United States",]

######### NLP ######### 
#### BUILDING CORPUS ####
contentCorpus<- iconv(articles$contents,to="utf-8-mac")
ccorpus<-Corpus(VectorSource(contentCorpus))

excerptCorpus<- iconv(articles$excerpt,to="utf-8-mac")
ecorpus<-Corpus(VectorSource(excerptCorpus))

inspect(ccorpus[1:5])

#### CLEANING ####
ccorpus<-tm_map(ccorpus,tolower) 
ccorpus<-tm_map(ccorpus,removePunctuation)
ccorpus<-tm_map(ccorpus,removeNumbers)
ccleaned<-tm_map(ccorpus,removeWords,stopwords("en"))
stopwords("en")

removeURL<- function(x) gsub('http[[:alnum:]]*','',x)
ccleaned<-tm_map(cleaned,content_transformer(removeURL))
ccleaned<-tm_map(cleaned,stripWhitespace)

ecorpus<-tm_map(ecorpus,tolower) #make it all lowercase
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
stopwords("english")
ecleaned<-tm_map(ecorpus,removeWords,stopwords("english"))
inspect(ecleaned[1:5])
#remove united and airlines
ecleaned<-tm_map(ecleaned,removeWords,c("united","airlines","flight","airline","passenger","flights","will","according"))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)

#### TERM DOCUMENT MATRIX ####
#to structure the data
etdm<-TermDocumentMatrix(ecleaned)
etdm
ematrix<-as.matrix(etdm)
ematrix[1:10,1:20]

#BARPLOT
w <- rowSums(ematrix)
w <- w[w>5000]
barplot(w)
