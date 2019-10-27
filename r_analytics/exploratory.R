######### INSTALLING JSON READING ######### 
install.packages("jsonlite")

#########INSTALLING LIBRARIES FOR NLP ######### 
install.packages("tm")
#install.packages("rJava") #FIX macOS SUPPORTING ISSUE BY RUNNING "sudo R CMD javareconf" in TERMINAL
install.packages("wordcloud")
#install.packages("textir")
#install.packages("RWeka")
#install.packages("qdap")
#install.packages("maptpx")
install.packages("syuzhet")
install.packages("topicmodels")
install.packages("tidytext")

######### LOADING LIBRARY ######### 
#library("rjson") -- not working
library("jsonlite")
library("tm")
#library("rJava")
library("wordcloud")
#library("textir")
#library("RWeka")
#library("qdap")
#library("maptpx")
library("syuzhet")
library("topicmodels")
library("tidytext")

######### BEGIN EXPLORE ######### 
articles <- fromJSON("../datasets/articles.json")
head(articles)
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
removeURL<- function(x) gsub('http[[:alnum:]]*','',x)
changeWords<- function(x) gsub('australi','',x)

ecorpus<-tm_map(ecorpus,tolower) #make it all lowercase
ecorpus<-tm_map(ecorpus,removePunctuation)
ecorpus<-tm_map(ecorpus,removeNumbers)
stopwords("english")
ecleaned<-tm_map(ecorpus,removeWords,stopwords("english"))
ecleaned<-tm_map(ecorpus,content_transformer(changeWords))
inspect(ecleaned[1:5])
#remove united and airlines
ecleaned<-tm_map(ecleaned,removeWords,c(stopwords("english"),'plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
inspect(ecleaned[1:5])
ecleaned<-tm_map(ecleaned,stripWhitespace)


#### k1 ####
k1 <- read.csv("../four-fold-export/k1.csv")
contentCorpus<- iconv(k1$contents,to="utf-8-mac")
ccorpus<-Corpus(VectorSource(contentCorpus))

ccorpus<-tm_map(ccorpus,tolower) 
ccorpus<-tm_map(ccorpus,removePunctuation)
ccorpus<-tm_map(ccorpus,removeNumbers)
ccleaned<-tm_map(ccorpus,removeWords,c(stopwords("en"),'\\n','plane',"united","airlines","flight","airline","passenger","flights","will","according","…","—","’s","says","said"))
ccleaned<-tm_map(ccorpus,removeWords,c('\\n'))
inspect(ccleaned[1:5])
ccleaned<-tm_map(ccleaned,stripWhitespace)
ctdm<-TermDocumentMatrix(ccleaned)
cmatrix<-as.matrix(ctdm)

cmatrix[1:10,1:20]

attributes(ccleaned)
df <- data.frame(text = get("content", ccleaned),stringsAsFactors = F)
head(df$text)
str(df)
sentiment <- get_nrc_sentiment(df$text)
df$s<-sentiment
head(sentiment)
head(df$s)
#### TERM DOCUMENT MATRIX ####
#to structure the data
etdm<-TermDocumentMatrix(ecleaned)
etdm

ematrix<-as.matrix(etdm)
ematrix[1:10,1:20]
ematrix<-NULL
#### BARPLOT #### 
w <- rowSums(cmatrix)
sw[1:10]
sw<-sort(w,TRUE)
ov2k <- w[w>1000]
barplot(sw[1:100],las=2,main="Over 2000")
sw[1:100]
w["new"]


##### SENTIMENT AGAINST PUBLISHER ####
#FREQUENCY
barplot(table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))]),
        legend=T,
        ylim=c(0,1300))
contingencytb<-table(articles$sentiment[is.element(articles$source$publisher,names(tpub[1:10]))],articles$source$publisher[is.element(articles$source$publisher,names(tpub[1:10]))])
for(i in 1:10){
  contingencytb[,i]<-contingencytb[,i]/sum(contingencytb[,i])
}
barplot(contingencytb) # PROPORTION OF THE SENTIMENTS
contingencytb


all_art<- table(articles$source$publisher,articles$sentiment)

for(i in 1:3763){
  if(sum(all_art[i,])<10){
    t[]
  }
  t[i,]<-all_art[i,]/sum(all_art[i,])
}
all_art<-sort(all_art,T)
t<-sort(all_art[,"-1"],T)
head(t)
all_art[-1,]
#### WORDCLOUD #### 
set.seed(222)
wordcloud(words=names(sw),
          freq=sw,
          max.words = 100,
          min.freq = 2000,
          colors = brewer.pal(8,"Dark2"))

#### SENTIMENT ANALYSIS #### 
attributes(ecleaned)
df <- data.frame(text = get("content", ecleaned),stringsAsFactors = F)
head(df$text)
str(df)
sentiment <- get_nrc_sentiment(df$text)
df$s<-sentiment
head(sentiment)
head(df$s)

names(sentiment)
barplot(table(sentiment))
barplot(table(as.vector(sentiment)))
tableofsentiment<-c(sum(sentiment$anger),
                    sum(sentiment$anticipation),
                    sum(sentiment$disgust),
                    sum(sentiment$fear),
                    sum(sentiment$joy),
                    sum(sentiment$sadness),
                    sum(sentiment$surprise),
                    sum(sentiment$trust),
                    sum(sentiment$negative),
                    sum(sentiment$positive))

barplot(tableofsentiment,
        names.arg = names(sentiment))


negsen<-c(sum(sentiment$anger),
                    sum(sentiment$disgust),
                    sum(sentiment$fear),
                    sum(sentiment$sadness),
                    sum(sentiment$surprise),
                    sum(sentiment$negative))

barplot(negsen,
        names.arg = c("Anger","Disgust","Fear","Sadness","Surprise","Negative"))

#### EXPORTING CSV ####
#OK HAVE TO DROP AUTHORS BECAUSE IT IS A LIST OF CHR AND NOT A VECTOR SO CANNOT USE WRITE WITH IT--
selected <- articles[,!names(articles) %in% c("authors","has_video","topics","image_link","link","source.link")]
selected <- flatten(selected)

first <- sample(1:39109,9777)
k1<-selected[first,]
selected<-selected[-first,]

second <- sample(1:29332,9777)
k2<-selected[second,]
selected<-selected[-second,]

third <- sample(1:19555,9777)
k3<-selected[third,]

k4<-selected[-third,]

write.csv(k1,"../four-fold-export/k1.csv",row.names = F)
write.csv(k2,"../four-fold-export/k2.csv",row.names = F)
write.csv(k3,"../four-fold-export/k3.csv",row.names = F)
write.csv(k4,"../four-fold-export/k4.csv",row.names = F)

?write.csv
head(k1)

#### TOP10PUBLISHERS ####
test1<-read.csv("../four-fold-export/k1.csv")
#head(test1)
tpub <- table(articles$source$publisher)
tpub<- sort(tpub,T)
barplot(tpub[1:10],las=1,
               main="Top 10 Publishing Sources",
                 ylab="Amount of Articles",
                 xlab="Sources",
                 names.arg = names(tpub[1:10]),
                 col="red")
names(tpub[1:10])

#### TOPIC CLUSTERING ####

# set a seed so that the output of the model is predictable
k =6
ctdm_lda <- LDA(ctdm, k = k, control = list(seed = 1234))
ctdm_lda


ldamat<-as.matrix(topics(ctdm_lda))
which(ldamat[,1]==2)[1:20]
ldamat[30]

etdm_topics <- tidy(ctdm_lda,matrix="beta")
etdm_topics[100:150,]




